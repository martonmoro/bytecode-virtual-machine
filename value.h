#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,
} ValueType;

// We are using a tagged union to represent values
typedef struct {
    ValueType type;
    union 
    {
        bool boolean;
        double number;
        Obj* obj;
    } as; 
} Value;
// Right now on a 64 bit machine with a typical C compiler our layout look like this: 
// 4 byte type, 4 byte padding 8 byte union
// The compiler adds that padding to keep that double on the nearest eight-byte boundary
// (We could move the tag field after the union, but that doesn’t help much either. 
// Whenever we create an array of Values—which is where most of our memory usage for 
// Values will be—the C compiler will insert that same padding between each Value to keep the 
// doubles aligned.)

// For small fixed-size values like integers, many instruction sets store the value directly
// in the code stream right after the opcode. These are called imeediate instructions because
// the bits for the value are immediately after the opcode.
//
// That doesn't work well for large or variable-sized constants like strings. In a native compiler
// to machine code, those bigger constants get stored in a separate "constant data" region in the
// binary executable. Then, the instruction to load a constant has an address or offset pointing
// to where the value is stored in that section.
//
// Most virtual machines do something similar. For example, the Java Virtual Machine associates a
// constant pool with each compiled class. In clox each chunk will carry with it a list of values
// that appear as literals in the program.

#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)
#define AS_OBJ(value)     ((value).as.obj)   

// compound literal and designated initializer
#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)    ((Value){VAL_OBJ, {.obj = (Obj*)object}})

// The constant pool is an array of values. The instruction to load a constant looks up the value
// by index in that array.
typedef struct
{
    int capacity;
    int count;
    Value *values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);

#endif