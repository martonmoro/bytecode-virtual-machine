#ifndef clox_value_h
#define clox_value_h

#include "common.h"
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
typedef double Value;

// The constant pool is an array of values. The instruction to load a constant looks up the value
// by index in that array.
typedef struct
{
    int capacity;
    int count;
    Value *values;
} ValueArray;

void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);

#endif