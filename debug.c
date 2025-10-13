#include <stdio.h>

#include "debug.h"
#include "value.h"

// First we print a header so we can tell which chunk we are looking at.
// Then we disasseble each instruction.
// Instead of incrementing `offset` in the loop, we let `disassebleInstrcution()`
// do it for us. When we call that function, after disassembling the instruction at the give offset,
// it returns the offset of the next instruction. This is because instructions can have different sizes.
void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);

    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    // First we print the name and the index of the constant
    printf("%-16s %4d '", name, constant);
    // Then we display the value itself
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

// The compiler compiles local variables to direct slot access. The local variable’s name 
// never leaves the compiler to make it into the chunk at all. That’s great for performance, 
// but not so great for introspection. When we disassemble these instructions, we can’t show 
// the variable’s name like we could with globals. Instead, we just show the slot number.
static int byteInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2; 
}

static int jumpInstruction(const char* name, int sign, Chunk* chunk, int offset) {
    uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
}

// First, it prints the byte offset of the given instruction
// Next, it reads a single byte from the bytecode at the give noffset
int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);

    int currentLine = getLine(&chunk->lineArray, offset);
    if (offset > 0 && currentLine == getLine(&chunk->lineArray, offset - 1)) {
        // Bytecode instructions tend to be pretty fine-grained. 
        // A single line of source code often compiles to a whole sequence of instructions. 
        // To make that more visually clear, we show a | for any instruction that comes 
        // from the same source line as the preceding one. 
        printf("   | ");
    } else {
        printf("%4d ", currentLine);
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_NIL:
            return simpleInstruction("OP_NIL", offset);
        case OP_TRUE:
            return simpleInstruction("OP_TRUE", offset);
        case OP_FALSE:
            return simpleInstruction("OP_FALSE", offset);
        case OP_POP:
            return simpleInstruction("OP_POP", offset);
        case OP_GET_LOCAL:
            return byteInstruction("OP_GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return byteInstruction("OP_SET_LOCAL", chunk, offset);
        case OP_GET_GLOBAL:
            return constantInstruction("OP_GET_GLOBAL", chunk, offset);
        case OP_DEFINE_GLOBAL: 
            return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
        case OP_SET_GLOBAL:
            return constantInstruction("OP_SET_GLOBAL", chunk, offset);
        case OP_EQUAL:
            return simpleInstruction("OP_EQUAL", offset);
        case OP_GREATER:
            return simpleInstruction("OP_GREATER", offset);
        case OP_LESS:
            return simpleInstruction("OP_LESS", offset);
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        case OP_SUBTRACT:
            return simpleInstruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY:
            return simpleInstruction("OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case OP_NOT:
            return simpleInstruction("OP_NOT", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        case OP_PRINT:
            return simpleInstruction("OP_PRINT", offset);
        case OP_JUMP: 
            return jumpInstruction("OP_JUMP", 1, chunk, offset);
        case OP_JUMP_IF_FALSE:
            return jumpInstruction("OP_JUMP_IF_FAlse", 1, chunk, offset);
        case OP_LOOP:
            return jumpInstruction("OP_LOOP", -1, chunk, offset);
        case OP_CALL:
            return byteInstruction("OP_CALL", chunk, offset);
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default: 
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}