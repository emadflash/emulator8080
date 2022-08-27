#include "common.h"

static const char *EOF_string = "EOF";

typedef struct Tape Tape;
struct Tape {
    u16 origin;             /* starting/origin address of program */
    u8 bytecode[64 * 1024]; /* 64 Kb */
    u16 bytecode_count;
    int error_count;
};

Tape make_tape();
void assemblar_init_tape(char *source_filepath, Tape *tape);