#include "common.h"

typedef struct Tape Tape;
struct Tape {
    u16 origin;             /* starting/origin address of program */
    u8 bytecode[64 * 1024]; /* 64 Kb */
    u16 bytecode_count;

    int error_count;
};

Tape as_emit_from_source(char *filepath, char *src);