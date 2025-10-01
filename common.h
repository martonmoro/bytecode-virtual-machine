#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// When this flag is defined, we use out existing "debug" module to print out the chunk's bytecode.
#define DEBUG_PRINT_CODE
// When this flag is defined, the VM disassebles and prints each instruction right before executing it.
#define DEBUG_TRACE_EXECUTION

#endif