#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"

// It would be a chore to pass around a pointer to the VM to all of the functions so we
// declare a single global VM object
VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
}

static void runtimeError(const char* format, ...) {
    // Lets us pass an arbitrary number of arguments to this fm
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t instruction = vm.ip - vm.chunk->code - 1;
    int line = getLine(&vm.chunk->lineArray, instruction);
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
}

void initVM() {
    resetStack();
}

void freeVM() {}

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

// Nil and false are falsey, every other value behaves like true
static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

 static InterpretResult run() {
    #define READ_BYTE() (*vm.ip++)
    #define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
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
            disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
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
            case OP_EQUAL: {
                Value b = vm.stackTop[-1];
                Value a = vm.stackTop[-2];
                vm.stackTop[-2] = BOOL_VAL(valuesEqual(a, b));
                vm.stackTop--; 
                break;
            }
            case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD:      BINARY_OP(NUMBER_VAL, +); break;
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
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }
    #undef READ_BYTE
    #undef READ_CONSTANT
    #undef BINARY_OP
    
 }

 // First we create a new empty chunk and pass it over to the compiler. 
 // The compiler will take the user's program and fill up the chunk with
 // bytecode. At least, that's what it will do if the program doesn't 
 // have any compile errors. We send the complete chunk over to the VM
 // to be executed. When the VM finishes, we free the chunk and we're done
 InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;

    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
 }