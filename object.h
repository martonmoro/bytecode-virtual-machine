#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum
{
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
} ObjType;

struct Obj
{
    ObjType type;
    struct Obj* next;
};

typedef struct {
    Obj obj;
    // Number of parameters the function expects
    int arity;
    Chunk chunk;
    ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

// An Obj header and a pointer to the C function that implements the native behaviour.
typedef struct {
    Obj obj;
    NativeFn function;
} ObjNative;

// C specifies that struct fields are arranged in memory in the order that they are declared.
// Also, when you nest structs, the inner struct's fields are expanded right in place. This
// means that the first bytes of ObjString exactly line up with Obj. This allows us to take
// a pointer to a struct and safely convert it to a pointer to its first field and back.
// Given an `ObjString*` we can safely cast it to `Obj*` and then access the type field from
// it. Every `ObjString` "is" and Obj in the OOP sense of "is"
// We are caching the hash for the hash table
struct ObjString
{
    Obj obj;
    int length;
    char *chars;
    uint32_t hash;
};

ObjFunction* newFunction();
ObjNative* newNative(NativeFn function);
ObjString* takeString(char* chars, int length);
ObjString *copyString(const char *chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type)
{
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif