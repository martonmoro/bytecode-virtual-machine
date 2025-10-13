#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"
#include "line.h"

// Each instrcution has a one-byte operation code (opcode).
// That number controls what kind of instruction we're dealing with
//
// We allow instructions to have operands. These are stored as binary
// data immediately after the opcode in the instrcution stream.
// ( Bytecode instruction operands are not the same as the operands 
// passed to an arithmetic operator. When we get to expressions that 
// arithmetic operand values are tracked separately. 
// Instruction operands are a lower-level notion that modify how the 
// bytecode instruction itself behaves. )
typedef enum
{
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_RETURN
} OpCode;

// Bytecode is a series of instructions.
typedef struct
{
    int count;
    int capacity;
    uint8_t *code;
    LineArray lineArray;
    ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif