#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// A CallFrame represents a single ongoing function call
// Each time a function is called we create one of these structs
typedef struct {
    ObjFunction* function;
    // Instead of storing the return address in the callee's frame, the caller stores
    // its own ip. When we return from a function, the VM will jump to the ip of the 
    // caller's CallFrame and resume from there.
    uint8_t* ip;
    // Points into the VM's value stack at the first slot that this function can use
    Value* slots;
} CallFrame;

typedef struct
{
    // An array of CallFrame structs treated as a stack (it replaces the chunk and ip fields)
    // Now each CallFrame has its own ip and its own pointer to the ObjFunction that it's 
    // executing. From there, we can get to the function's chunk
    CallFrame frames[FRAMES_MAX];
    // Stores the current height of the CallFrame stack - the number of ongoing function calls
    int frameCount;

    // value stack (shared by all functions)
    Value stack[STACK_MAX];
    // points to the next free slot
    Value* stackTop;
    Table globals;
    Table strings;
    Obj* objects;
} VM;

typedef enum
{
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif