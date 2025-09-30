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
    #define BINARY_OP(op) \
        do { \
            vm.stackTop[-2] = vm.stackTop[-2] op vm.stackTop[-1]; \
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
            case OP_ADD:      BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE:   BINARY_OP(/); break;
            // case OP_NEGATE: push(-pop()); break; - This does a redundant pop followed by a push
            case OP_NEGATE: {
                vm.stackTop[-1] = -vm.stackTop[-1];
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

 InterpretResult interpret(const char* source) {
    compile(source);
    return INTERPRET_OK;
 }