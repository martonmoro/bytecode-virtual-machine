#include <stdlib.h>

#include "line.h"
#include "memory.h"

void initLineArray(LineArray* array) {
    array->count = 0;
    array->capacity = 0;
    array->lines = NULL;
    array->counts = NULL;
}

void freeLineArray(LineArray* array) {
    FREE_ARRAY(int, array->lines, array->capacity);
    FREE_ARRAY(int, array->counts, array->capacity);
    initLineArray(array);
}

void writeLineArray(LineArray* array, int line) {
    // If this is the same line as the last entry, just increment the count
    if (array->count > 0 && array->lines[array->count - 1] == line) {
        array->counts[array->count - 1]++;
        return;
    }

    // Otherwise, add a new line entry
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->lines = GROW_ARRAY(int, array->lines, oldCapacity, array->capacity);
        array->counts = GROW_ARRAY(int, array->counts, oldCapacity, array->capacity);
    }

    array->lines[array->count] = line;
    array->counts[array->count] = 1;
    array->count++;
}

int getLine(LineArray* array, int instruction) {
    int instructionsSeen = 0;
    for (int i = 0; i < array->count; i++) {
        instructionsSeen += array->counts[i];
        if (instruction < instructionsSeen) {
            return array->lines[i];
        }
    }
    return -1;
}