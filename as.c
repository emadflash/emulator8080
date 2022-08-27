#include "assemblar.h"
#include "common.h"

int main(int argc, char **argv) {
    if (argc < 2) {
        eprintln("error: expected source file");
        exit(1);
    }

    String source;
    Tape tape;

    source = file_as_string(argv[1]);
    if (!source) {
        eprintln("error: invaild input source file: %s", argv[1]);
        exit(1);
    }

    tape = as_emit_from_source(argv[1], &source);
    for (u16 i = 0; i < tape.bytecode_count; ++i) {
        println("0x%x", tape.bytecode[i]);
    }

    free_string(source);
    return 0;
}