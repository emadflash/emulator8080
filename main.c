#include "emulator.c"
#include "disassemblar.c"
#include "cli.c"

#define AUTHOR_NAME "madflash"
#define AUTHOR_EMAIL_ADDRESS "backbugkaught@gmail.com"
#define PROGRAM_DESC "Space invaders emulator"
#define VERSION "0.0.1"

typedef struct Cli_Config Cli_Config;
struct Cli_Config {
    char *rom_filepath;
    bool is_disassemble;
    int start_address;
};

Cli_Config create_config() {
    return (Cli_Config){
        .rom_filepath = NULL,
        .is_disassemble = false,
    };
}

void init_config_from_cli(Cli_Config *conf, int argc, char **argv) {
    Cli_Flag positionals[] = {
        Flag_CString_Positional(&conf->rom_filepath, "ROM_FILE", "filepath of rom")};

    Cli_Flag optionals[] = {
        Flag_Bool(&conf->is_disassemble, "d", "dis", "disassemble binary"),
        Flag_Int(&conf->start_address, "s", "start-addr", "starting address"),
    };

    Cli cli =
        create_cli(argc, argv, "emulator8080", positionals, array_sizeof(positionals, Cli_Flag),
                   optionals, array_sizeof(optionals, Cli_Flag));

    cli_set_extra_info(&cli, VERSION, AUTHOR_NAME, AUTHOR_EMAIL_ADDRESS, PROGRAM_DESC);
    cli_parse_args(&cli);

    if (cli_has_error(&cli)) {
        exit(EXIT_FAILURE);
    }
}

void run(Cli_Config *conf) {
    String rom = file_as_string(conf->rom_filepath);
    Cpu *cpu = make_cpu(cast(u8 *) rom, string_length(rom), cast(u16) conf->start_address);

    if (conf->is_disassemble) {
        disassemble(cpu);
    } else {
        cpu_execute(cpu);
    }

    free_cpu(cpu);
    free_string(rom);
}

int main(int argc, char **argv) {
    Cli_Config conf = create_config();
    init_config_from_cli(&conf, argc, argv);

    run(&conf);

    return 0;
}
