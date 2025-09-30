#ifndef clox_lines_h
#define clox_lines_h

#include "common.h"

// Run-length encoded line information
typedef struct
{
    int capacity;
    int count;
    int *lines;
    int *counts;
} LineArray;

void initLineArray(LineArray *array);
void freeLineArray(LineArray *array);
void writeLineArray(LineArray *array, int line);
int getLine(LineArray *array, int instruction);

#endif