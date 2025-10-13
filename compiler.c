#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

// Lox's precedence levels in order from lowest to highest
typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    // We store the name of the variable. When we're resolving an identifier, we compare the identifier's
    // lexeme with each local's name to find a match. 
    Token name;
    // The depth field records the scope depth of the block where the local variable was declared.
    int depth;
} Local;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
    // We treat the series of nested Compiler structs as a stack. Instead of using an array we use a
    // linked list. Each Compiler points back to the Compiler for the function taht encloses it, all the
    // way back to the root Compiler for the top-level code.
    struct Compiler* enclosing;
    // The “top level” of a Lox program is also imperative code and we need a chunk to compile that into. 
    // We can simplify the compiler and VM by placing that top-level code inside an automatically defined 
    // function too. That way, the compiler is always within some kind of function body, and the VM always 
    // runs code by invoking a function. It’s as if the entire program is wrapped inside an implicit 
    // main() function. One semantic corner where that analogy breaks down is global variables. They have 
    // special scoping rules different from local variables, so in that way, the top level of a script 
    // isn’t like a function body.
    ObjFunction* function;
    FunctionType type;
    // Simple, flat array of all locals that are in scope during each point in the compilation process.
    // They are ordered in the array in the order that their declarations appear in the code. Since the
    // instruction operand we'll use to encode a local is a single byte, our VM has a hard limit on the 
    // number of locals that can be in scope at once. 
    Local locals[UINT8_COUNT];
    // Tracks how many locals are in scope - how many of those array slots are in use. 
    int localCount;
    // Tracks scope depth. The number of blocks surrounding the current bit of code we're compiling. 
    int scopeDepth;
} Compiler;

Parser parser;
// If we were principled engineers, we’d give each function in the front end a parameter that accepts a 
// pointer to a Compiler. We’d create a Compiler at the beginning and carefully thread it through each 
// function call
Compiler* current = NULL;
Chunk* compilingChunk;

// Right now, the chunk pointer is stored in a module-level variable like we store other
// global state. Later, when we start compiling user-defined functions, the notion of 
// "current chunk" gets more complicated
static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    // While the panic mode flag is set we suppress any other errors that get detected
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

// Steps forward through the token stream.
static void advance() {
    parser.previous = parser.current;

    for(;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

// Reads and validates the expected type of the next token
static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

// If current token has given type, we consume the token and return true
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

// Appending a single byte to the chunk
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

// Emits a bytecode instruction and writes a placeholder operand for the jump offset.
static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn() {
    emitByte(OP_NIL);
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

// Goes back into the bytecode and replaces the operand at the given location with the 
// calculated jump offset.
static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");  
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    // Creating an ObjFunction in the compiler might seem a little strange. A function object 
    // is the runtime representation of a function, but here we are creating it at compile time. 
    // The way to think of it is that a function is similar to a string or number literal. It 
    // forms a bridge between the compile time and runtime worlds. When we get to function 
    // declarations, those really are literals—they are a notation that produces values of a 
    // built-in type. So the compiler creates function objects during compilation. Then, at runtime, 
    // they are simply invoked.
    compiler->function = newFunction();
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }

    // The compiler implicitly claims stack slot zero for the VM's own internal use
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    // We give it an empty name so that the user can't write an identifier that refers to it.
    local->name.start = "";
    local->name.length = 0;
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    // When we pop a scope, we walk backward through the local array looking for any 
    // variables declared at the scope depth we just left
    // (this could be optimized with OP_POPN)
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
        emitByte(OP_POP);
        current->localCount--;
    }
}

// Forward declarations

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static uint8_t identifierConstant(Token* name); 
static int resolveLocal(Compiler* compiler, Token* token);
static uint8_t argumentList();

// Our ypothetical array of function pointers doesn’t just list functions to parse
// expressions that start with a given token. Instead, it’s a table of function 
// pointers. One column associates prefix parser functions with token types. The 
// second column associates infix parser functions with token types.
static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    // Compile the right hand operand
    // We use one higher level of precedence for the right operand because the binary
    // operators are left-associative.
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        default: return; // Unreachable
    }
}

// a function call expression is kind of an infix ( operator. You have a 
// high-precedence expression on the left for the thing being called—usually 
// just a single identifier. Then the ( in the middle, followed by the 
// argument expressions separated by commas, and a final ) to wrap it up at 
// the end.
static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    // Emit a new OP_CALL instruction to invoke the function, using the 
    // argument count as an operand.
    emitBytes(OP_CALL, argCount);
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: return; // Unreachable
    }
}

// We assume the initial ( has already been consumed.
// We recursively call back into `expression()` to compile the expression
// between the parantheses, then parse the closing ) at the end.
//
// As far as the back end is concerned, there’s literally nothing to a grouping 
// expression. Its sole function is syntactic—it lets you insert a l
// ower-precedence expression where a higher precedence is expected. Thus, it 
// has no runtime semantics on its own and therefore doesn’t emit any bytecode. 
// The inner call to expression() takes care of generating bytecode for the 
// expression inside the parentheses.
static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// To compile number literals, we store a pointer to this function at the 
// `TOKEN_NUMBER` index in the array.
// We assume the token for the number literal has already been consumed 
// and is stored in `previous`.
static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

// At the point this is called, the left-hand side expression has already been compiler. That means
// at runtime, its value will be on top of the stack. If that value is falsey, then we know the entire
// and must be false, so we skip the right operand and leave the left-hand side value as the result of
// the entire expression. Otherwise, we discard the left-hand value and evaluate the right operand 
// which becomes the result of the whole and expression.
static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

// If the left-hand side is truthy, then we skip over the right operand. 
// Thus we need to jump when a value is truthy. When the left-hand side
// is falsey, it does a tiny jump over the next statement. That statement
// is an unconditional jump over the code for the right operand.
static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

// Takes the string's characters directly from the lexeme. We also trim the ""s
// If Lox supported string escape sequences we'd translate those here
static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length -2)));
}

// In the parse function for the identifier expressions, we look for an equals
// sign after the identifier. If we find one, instead of emitting code for a 
// variable access, we compile the assigned value and then emit an assignment 
// instruction.
static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    
    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand
    // We use the unary operator's own PREC_UNARY precedence to permit
    // nested unary expressions like `!!doubleNegative`. Sunce unary
    // operators have pretty high precedence, that correctly excludes 
    // things like binary operators.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction
    // We write the negate instruction after its operand's bytecode since 
    // first we evaluate the operand then we pop that value, negate it, 
    // and push the result.
    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return;    // Unreachable
    }
}
// a table that, given a token type, lets us find the function to compile 
// a prefix expression starting with a token of that type, the function to 
// compile an infix expression whose left operand is followed by a token 
// of that type, and the precedence of an infix expression that uses that 
// token as an operator.
ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

// This function starts at the current token and parses any expression at 
// the given precedence level or higher. 
// Every expression starts with a prefix token: literal, variable or identifier
// unary operator or grouping. The prefix rule parses this "base" of the expression,
// which becomes the left-hand side for any upcoming operators.
// Pratt parsing = prefix first → optional infix chain → repeat recursively according to precedence.
static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expected expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

// Takes the given token and adds its lexeme to the chunk's constant table as a string. It then
// returns the index of that constant in the constant table.
// Global variables are looked up by name at runtime. That means the VM - the bytecode interpreter
// loop - needs access to the name. A whole string is too big to stuff into the bytecode stream as 
// an operand. Instead, we store the string in the constant table and the instruction then refers 
// to the name by its index in the table.
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

// We walk the list of locals that are currently in scope. If one has the same name as the identifier
// token, the identifier must refer to that variable, we found it! We walk backwards to ensure correct
// shadowing in inner scopes.
// At runtime we load and store locals using the stack slot index, so that's what the compiler needs
// to calculate after it resolves the variable. Whenever a variable is declared, we append it to the 
// locals array in Compiler. That means the first local variable is at index zero, the next one is at 
// index one, and so on. In other words, the locals array in the compiler has the exact same layout as
// the VM's stack will have at runtime. The variable's index in the locals array is the same as its 
// stack slot.
static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount -1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

// Initializes the next available Local in the compiler's array of variables. It stores the variable's
// name and the depth of the scope that owns the variable.
static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
}

static void declareVariable() {
    // If we're in the top-level global scope, we bail out because global variables are late bound
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    for (int i = current->localCount -1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    // Exit function if we are in local scope
    // At runtime locals aren't looked up by name so there's no need to stuff the variable's name
    // into the constant table, so if the declaration is inside a local scope, we return a dummy 
    // table index instead.
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialized() {
    // A top-level function declaration will also call this function. When that happens, there is
    // no local variable to mark initialized - the function is bound to a global variable.
    if (current->scopeDepth == 0) return;
    current->locals[current->localCount -1].depth = current->scopeDepth;
}

// This function outputs the bytecode instruction that defines the new variable and stores its
// initial value. The index of the variable's name in the constant table is the instruction's 
// operand. As usual in a stack-based VM, we emit this instruction last. At runtime, we execute 
// the code for the variable's initializer first. That leaves the value on the stack. Then this
// instruction takes that value and stores it away for later.
static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        // There is no code to create a local variable at runtime
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

// Returns the number of arguments it compiled. Each argument expression generates code that 
// leaves its value on the stack in preparation for the call. 
static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

// Returns the rule at the given index
// This function exists solely to handle a declaration cycle in the C code.
// `binary()` is defined before the rules table so the table can store a 
// pointer to it. That means the body of `binary()` cannot access the table
// directly
static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

// We map each token type to a different kind of expression. We define a 
// function for each expression that outputs the appropriate bytecode. 
// Then we bould an array of function pointers. The indexes in the array
// correspond to the `TokenType` enum values, and the function at each 
// index is the code to compile an expression of that token type.
static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

// Create a separate Compiler for each function being compiled. When we start 
// compiling a function declaration, we create a new Compiler on the C stack 
// and initialize it. initCompiler() sets that Compiler to be the current one.
// Then, as we compile the body, all of the functions that emit bytecode write 
// to the chunk owned by the new Compiler's function.
// After we reach the end of the function's block body, we call endCompiler().
// That yields the newly compiler function object, which we store as a contant
// in the surrounding function's constant table. 
static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");

    // Semantically, a parameter is simply a local variable declared in the
    // outermost lexical scope of the function body. Unlike local variables,
    // which have initialziers, there's no code here to initialzie the 
    // parameter's argument
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            uint8_t constant = parseVariable("Expected parameter name.");
            defineVariable(constant);
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");

    block();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL(function)));
}

// Functions are first-class values, and a function declaration simply creates 
// and stores one in a newly declared variable. 
static void funDeclaration() {
    // We parse the name just like any other variable declaration. A function
    // declaration at the top level will bind the function to a global variable.
    // Inside a block or other function, a function declaration creates a local
    // variable.
    uint8_t global = parseVariable("Expected a function name.");
    // It is safe for a function to refer to its own name inside its body. 
    markInitialized();
    // We compile the function itself. Its parameter list and block body. For that 
    // we use a helper which leaves the resulting function object on top of the stack
    function(TYPE_FUNCTION);
    // Store the function back into the variable we declared for it.
    defineVariable(global);
}

static void varDeclaration() {
    // Index of the variable name string stored in the chunk's constant table
    uint8_t global = parseVariable("Expect variable name");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        // Desugaring var a; into var a = nil;
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");

    defineVariable(global);
}

// An "expression statement" is simply an expression followed by a semicolon.
// They're how we write an expression in a context where a statement is expected.
// Usually, it's so that you can call a function or evaluate an assignment for its
// side effect.
// Semantically, an expression statement evaluates the expression and discards the result
static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

// https://craftinginterpreters.com/image/jumping-back-and-forth/for.png
static void forStatement() {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    // Initializer clause
    if (match(TOKEN_SEMICOLON)) {
        // No initializer
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    // Condition clause
    int loopStart = currentChunk()->count;
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        // Pop condition
        emitByte(OP_POP);
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        // When the increment is present, we need to compile it now, but it shouldn't 
        // execute yet. So, first, we emit an unconditional jump that hops over the 
        // increment caluse's code to the body of the loop.
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        // Next, we compile the increment expression itself. This is usually an assignment.
        expression();
        // Whatever it is, we only execute it for its side effect, so we also emit a pop.
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expecte ')' after for clauses.");

        // First we emit a loop instruction. This is the main loop that takes us back to the
        // top of the for loop - right before the condition expression if there is one. That 
        // loop happens right after the increment, since the increment executes at the end of 
        // each loop iteration.
        emitLoop(loopStart);
        // Then we change loopStart to point to the offset where the increment expression begins.
        // Later, when we emit the loop instruction after the body statement, this will cause it
        // to jump up to the increment expression instead of the top of the loop like it does when 
        // there is no increment.
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement();
    emitLoop(loopStart);

    // After the loop body we need to patch the jump if there is condition
    if (exitJump != -1) {
        patchJump(exitJump);
        // Pop condition
        emitByte(OP_POP);
    }

    endScope();
}

// First we compile the condition expression, bracketed by parantheses. At runtime,
// that will leave the condition value on top of the stack. We'll use that to determine
// whether to execute the then branch or skip it.
// Then we emit a new OP_JUMP_IF_FALSE instruction. It has an operand for how much to 
// offset the ip - how many bytes of code to skip. If the condition is falsey, it adjusts
// the ip by that amount.
static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    // Pop the condition
    emitByte(OP_POP);
    statement();

    int elseJump = emitJump(OP_JUMP);

    // We use backpatching. We emit the jump instruction first with a placeholder offset
    // operand. We keep track of where that half-finished instruction is. Next, we compile
    // the body. Once that's done, we know how far to jump. So we go back and replace that 
    // placeholder offset with the real one now that we can calculate it.
    patchJump(thenJump);
    // Pop the condition
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expected ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

// We compile the condition expression, surrounded by mandatory parantheses.
// Tha's followed by a jump instruction that skips over the subsequent body statement if the 
// condition is falsey. We patch the jump after compiling the body and take care to pop the 
// condition value from the stack on either path.
// After executing the body of a while loop, we jump all the way back to before the condition.
// That way, we re-evaluate the condition expression on each iteration. We store the chunk's 
// current instruction count in loopStart to record the offset in the bytecode right before the 
// condition expression we're about to compile. Then we pass that into this help function.
// https://craftinginterpreters.com/image/jumping-back-and-forth/while.png
static void whileStatement() {
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
}

// We skip tokens indiscriminately until we reach something that looks like a statement boundary. 
// We recognize the boundary by looking for a preceding token that can end a statement, like a 
// semicolon. Or we’ll look for a subsequent token that begins a statement, usually one of the 
// control flow or declaration keywords.
static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
      if (parser.previous.type == TOKEN_SEMICOLON) return;
      switch (parser.current.type) {
        case TOKEN_CLASS:
        case TOKEN_FUN:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN:
          return;

        default:
          ; // Do nothing
      }

      advance();
    }
}

/*
declaration    → classDecl
               | funDecl
               | varDecl
               | statement ;
*/
static void declaration() {
    if (match(TOKEN_FUN)) {
        funDeclaration();
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

/*
statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;

block          → "{" declaration* "}" ;
*/
static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();
    
    while (!match(TOKEN_EOF)) {
        declaration();
    }

    // We get the function object from the compiler
    ObjFunction* function = endCompiler();
    // If there were no compile errors, we return it. Otherwise, we signal an error by returning
    // NULL. This way, the VM doesn't try to execute a function that may contain invalid bytecode
    return parser.hadError ? NULL : function;
}