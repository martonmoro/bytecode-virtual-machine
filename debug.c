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
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        case OP_SUBTRACT:
            return simpleInstruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY:
            return simpleInstruction("OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default: 
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}