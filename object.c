#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;
    object->next = vm.objects;
    vm.objects = object;
    return object;
}

// This function creates a new ObjString on the heap and then initializes its fields. Sort of like a constructor in an
// OOP language. As such, it first calls the "base class" constructor to initialize the Obj state, using a new macro.
static ObjString* allocateString(char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    // Interning all of the strings
    // Whenever we create a new unique string, we add it to the table (we are using it more like a hash set here)
    tableSet(&vm.strings, string, NIL_VAL);
    return string;
}

// FNV-1a hash calculation
static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    // If we find string in the string table we first free the memory for the string that was passed in then we return it.
    // Since ownership is being passed to this function and we no longer need the duplicate string, it’s up to us to free it.
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length);
        return interned;
    }
    return allocateString(chars, length, hash);
}

// First we allocate a new array on the heap, just big enough for the string's characters and the trailing terminator.
// Once we have the array, we copy over the characers from the lexeme and terminate it.
ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    // When copying a string into a new LoxString, we look it up in the string table first. If we find it, instead of 
    // "copying", we just return a reference to that string. Otherwise, we fall through, allocate a new string, and
    // store it in the string table
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) return interned;

    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';   // We need to terminate the string ourselves because the lexeme points at a range of characters inside the monolithic source string
    return allocateString(heapChars, length, hash);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    }
}