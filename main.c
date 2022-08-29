#include "emulator.c"

int main(int argc, char **argv) {
    if (argc < 2) {
        eprintln("Error: expected [ROM]");
        exit(1);
    }

    String rom = file_as_string(argv[1]);
    Cpu *cpu = make_cpu(cast(u8*) rom, string_length(rom), 0x0000);

    cpu_execute(cpu);

    free_cpu(cpu);
    free_string(rom);

    return 0;
}
