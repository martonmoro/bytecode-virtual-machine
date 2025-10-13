#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

// It would be a chore to pass around a pointer to the VM to all of the functions so we
// declare a single global VM object
VM vm;

static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
}

static void runtimeError(const char* format, ...) {
    // Lets us pass an arbitrary number of arguments to this fm
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    // After printing the error message itself, we walk the acll stack from top to bottom.
    // For each frame, we find the line number that corresponds to the current ip inside the
    // frame's function. Then we print that line number along with the function name.
    for (int i = vm.frameCount -1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->function;
        size_t instruction = frame->ip - function->chunk.code - 1; // The -1 is because the IP is already sitting on the next instruction to be executed but we want the stack trace to point to the previous failed instruction
        fprintf(stderr, "[line %d] in ", getLine(&function->chunk.lineArray, instruction));
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

// Takes a pointer to a C function and the name it will be known as in Lox.
// Both copyString() and newNative() dynamically allocate memory. That means once we have a 
// GC, they can potentially trigger a collection. If that happens, we need to ensure the 
// collector knows we’re not done with the name and ObjFunction so that it doesn’t free them 
// out from under us. Storing them on the value stack accomplishes that.
static void defineNative(const char* name, NativeFn function) {
    // Wrap the function in an ObjNative and then store that in a global variable with 
    // the given name.
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM() {
    resetStack();
    vm.objects = NULL;
    initTable(&vm.globals);
    initTable(&vm.strings);

    defineNative("clock", clockNative);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    freeObjects();
}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop(){
    vm.stackTop--;
    return *vm.stackTop;
}

// Returns value from stack but doesn't pop it
 static Value peek(int distance) {
  return vm.stackTop[-1 - distance];
}

static bool call(ObjFunction* function, int argCount) {
    if (argCount != function->arity) {
        runtimeError("Expected %d arguments but got %d.", function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    // Create a new callframe
    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->function = function;
    frame->ip = function->chunk.code; // start at beginning of function
    // Ensures that the arguments already on the stack line 
    // up with the function's parameters
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_FUNCTION:
                return call(AS_FUNCTION(callee), argCount);
            case OBJ_NATIVE: {
                NativeFn native = AS_NATIVE(callee);
                // If the object being called is a native function we invoke the C function
                // right then and there. 
                Value result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default:
                break; // Non-callable oject type
        }
    }
    runtimeError("Can only call functions and classes.");
    return false;
}

// Nil and false are falsey, every other value behaves like true
static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING(pop());
    ObjString* a = AS_STRING(pop());

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    push(OBJ_VAL(result));
}

 static InterpretResult run() {
    // Store current topmost CallFrame in a local variable inside the main bytecode execution function
    CallFrame* frame = &vm.frames[vm.frameCount - 1];

    #define READ_BYTE() (*frame->ip++)
    #define READ_CONSTANT() (frame->function->chunk.constants.values[READ_BYTE()])
    // Takes the next two bytes from the chunk and builds a 16-bit unsigned integer out of them
    #define READ_SHORT() \
        (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
    // Reads a one-byte operand from the bytecode chunk. It treats that as an index into the chunk's
    // constant table and returns the string at that index.
    #define READ_STRING() AS_STRING(READ_CONSTANT())
    // This macro needs to expand to a series of statements. To be careful macro authors,
    // we want to ensure those statements all end up in the same scope when the macro is expaneded.
    // This trick gives us a way to contain multiple statements inside a block that also permits
    // a semicolon at the end.
    // In this macro we decrement and increment `stackTop` unnecessarily
    // #define BINARY_OP(op) \
    //     do { \
    //         double b = pop(); \
    //         double a = pop(); \
    //         push(a op b); \
    //     } while (false)
    #define BINARY_OP(valueType, op) \
        do { \
            if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
                runtimeError("Operands must be numbers."); \
                return INTERPRET_RUNTIME_ERROR; \
            } \
            double b = AS_NUMBER(vm.stackTop[-1]); \
            double a = AS_NUMBER(vm.stackTop[-2]); \
            vm.stackTop[-2] = valueType(a op b); \
            vm.stackTop--; \
        } while (false)


    for (;;) {
        #ifdef DEBUG_TRACE_EXECUTION
            printf("          ");
            for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
                printf("[ ");
                printValue(*slot);
                printf(" ]");
            }
            printf("\n");
            // we need to convert ip back to a relative offset from the beginning of the bytecode
            disassembleInstruction(&frame->function->chunk, (int)(frame->ip - frame->function->chunk.code));
        #endif

        uint8_t instruction;
        // This is called decoding or dispatching the instruction
        // To learn about more efficient bytecode dispatch look 
        // up “direct threaded code”, “jump table”, and “computed goto”.
        switch (instruction = READ_BYTE()) {
            // For now we are just printing out the constant
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_NIL: push(NIL_VAL); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_POP: pop(); break;
            case OP_SET_LOCAL: {
                // It takes the assigned value from the top of the stack and stores it in the stack
                // slot corresponding to the local variable. Note that it doesn’t pop the value from 
                // the stack. Remember, assignment is an expression, and every expression produces a 
                // value. The value of an assignment expression is the assigned value itself, so the 
                // VM just leaves the value on the stack.
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                // It seems redundant to push the local’s value onto the stack since it’s already 
                // on the stack lower down somewhere. The problem is that the other bytecode 
                // instructions only look for data at the top of the stack. This is the core aspect 
                // that makes our bytecode instruction set stack-based. Register-based bytecode 
                // instruction sets avoid this stack juggling at the cost of having larger instructions 
                // with more operands.
                // Previously we read the given local slot directly from the VM's stack array, which
                // meant it indexed the slot starting from the bottom of the stack. Now, it accesses 
                // the current frame's slots array, which means it accesses the given numbered slot 
                // relative to the beginning of that frame.
                push(frame->slots[slot]);
                break;
            }
            case OP_GET_GLOBAL: {
                // We pull the constant table index from the instructuon's 
                // operand and get the variable name
                ObjString* name = READ_STRING();
                Value value;
                // We use the name as a key to look up the variable's value 
                // in the global hash table
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                // We get the name of the variable from the constant table
                ObjString* name = READ_STRING();
                // Then we take the value from the top of the stack and store
                // it in the hash table with that name as the key.
                tableSet(&vm.globals, name, peek(0));
                // We don’t pop the value until after we add it to the hash table. 
                // That ensures the VM can still find the value if a garbage 
                // collection is triggered right in the middle of adding it to the 
                // hash table. That’s a distinct possibility since the hash table 
                // requires dynamic allocation when it resizes.
                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    // The call to `tableSet()` stores the value in the global variable
                    // table even if the variable wasn't previously defined. So we also
                    // take care to delete that zombie value from the table.
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // One of the differences from OP_DEFINE_GLOBAL is that setting a variable
                // does not pop the value off the stack. Assignment is an expression, so it
                // needs to leave that value there in case the assignemtn is nested inside 
                // some larger expression.
                break;
            }
            case OP_EQUAL: {
                Value b = vm.stackTop[-1];
                Value a = vm.stackTop[-2];
                vm.stackTop[-2] = BOOL_VAL(valuesEqual(a, b));
                vm.stackTop--; 
                break;
            }
            case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD:      {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    // double b = AS_NUMBER(pop()); 
                    // double a = AS_NUMBER(pop()); 
                    // push(NUMBER_VAL(a + b));
                    double b = AS_NUMBER(vm.stackTop[-1]);
                    double a = AS_NUMBER(vm.stackTop[-2]);
                    vm.stackTop[-2] = NUMBER_VAL(a + b);
                    vm.stackTop--;
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT:
                vm.stackTop[-1] = BOOL_VAL(isFalsey(vm.stackTop[-1]));
                break;
            // case OP_NEGATE: push(-pop()); break; - This does a redundant pop followed by a push
            case OP_NEGATE: {
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                vm.stackTop[-1] = NUMBER_VAL(-AS_NUMBER(vm.stackTop[-1]));
                break;
            }
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();
                // peek(argCount) returns stackTop[-1-distance] which is the function itself
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                // If callValue() is successful, there will be a new frame
                // on the CallFrame stack for the called function. We need
                // to update the cached pointer to the current frame.
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_RETURN: {
                // When a function returns a value, that value will be on top of the stack.
                Value result = pop();
                // Discard frame
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
        }
    }
    #undef READ_BYTE
    #undef READ_SHORT
    #undef READ_CONSTANT
    #undef READ_STRING
    #undef BINARY_OP
    
 }

 // First we create a new empty chunk and pass it over to the compiler. 
 // The compiler will take the user's program and fill up the chunk with
 // bytecode. At least, that's what it will do if the program doesn't 
 // have any compile errors. We send the complete chunk over to the VM
 // to be executed. When the VM finishes, we free the chunk and we're done
 InterpretResult interpret(const char* source) {
    // First, we pass the source code to the compiler. It returns us a new 
    // ObjFunction containing the compiled top-level code.
    ObjFunction* function = compile(source);
    // If we get NULL back, it means there was some compile-time error which
    // the compiler has already reported
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    // We store the function on the stack and prepare an initial CallFrame
    // to execute its code. Now we can see that the compiler sets aside 
    // stack slot zero to store the function being called.
    push(OBJ_VAL(function));
    // In the new CallFrame, we point to the function, initialize its ip to
    // point to the beginning of the function's bytecode, and set up its 
    // stack window to start at the very bottom of the VM's value stack
    call(function, 0); // Set up the first frame for executing the top-level code

    return run();
 }