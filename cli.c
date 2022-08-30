#include <errno.h>
#include <limits.h>

// --------------------------------------------------------------------------
//                          - Cli Parser -
// --------------------------------------------------------------------------

/* Supported flag types */
typedef enum {
    Cli_FlagType_Bool,
    Cli_FlagType_Int,
    Cli_FlagType_CString,
} Cli_FlagType;

static char *flag_type_to_cstring[] = {
    [Cli_FlagType_Bool] = "bool",
    [Cli_FlagType_Int] = "int",
    [Cli_FlagType_CString] = "string",
};

typedef struct Cli_Flag Cli_Flag;
struct Cli_Flag {
    char *big_name;
    char *small_name;
    char *description;

    Cli_FlagType type;
    union {
        bool *_bool;
        char **cstring;
        int *integer;
    } value_ref;
};

#define cli_create_flag(ref, small, big, desc, flag_type, REF)                                     \
    (Cli_Flag) {                                                                                   \
        .small_name = small, .big_name = big, .description = desc, .type = flag_type,              \
        .value_ref.REF = ref                                                                       \
    }

#define Flag_Bool(bool_ref, small, big, description)                                               \
    cli_create_flag(bool_ref, small, big, description, Cli_FlagType_Bool, _bool)
#define Flag_Int(int_ref, small, big, description)                                                 \
    cli_create_flag(int_ref, small, big, description, Cli_FlagType_Int, integer)
#define Flag_CString(cstring_ref, big, small, description)                                         \
    cli_create_flag(cstring_ref, big, small, description, Cli_FlagType_CString, cstring)

#define Flag_Bool_Positional(bool_ref, big, description) Flag_Bool(bool_ref, NULL, big, description)
#define Flag_Int_Positional(int_ref, big, description) Flag_Int(int_ref, NULL, big, description)
#define Flag_CString_Positional(cstring_ref, big, description)                                     \
    Flag_CString(cstring_ref, NULL, big, description)

typedef struct Cli Cli;
struct Cli {
    int argc;
    char **argv;

    char *author_name;
    char *email_address;
    char *program_name;
    char *version;
    char *program_description;

    Cli_Flag *positionals;
    int positionals_count;
    Cli_Flag *optionals;
    int optionals_count;

    bool has_extra_info;
    int error_count;
};

Cli create_cli(int argc, char **argv, char *program_name, Cli_Flag *positionals,
               int positionals_count, Cli_Flag *optionals, int optionals_count) {

    return (Cli){
        .argc = argc,
        .argv = argv,
        .author_name = NULL,
        .email_address = NULL,
        .program_name = program_name,
        .program_description = NULL,
        .version = NULL,
        .positionals = positionals,
        .positionals_count = positionals_count,
        .optionals = optionals,
        .optionals_count = optionals_count,
        .has_extra_info = false,
        .error_count = 0,
    };
}

void cli_set_extra_info(Cli *cli, char *version, char *author_name, char *email_address,
                        char *program_description) {
    cli->version = version;
    cli->author_name = author_name;
    cli->email_address = email_address;
    cli->program_description = program_description;
    cli->has_extra_info = true;
}

bool cli_has_error(Cli *cli) {
    return cli->error_count != 0 ? true : false;
}

static void cli_report_error(Cli *cli, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
    cli->error_count += 1;
}

static Cli_Flag *cli_find_optional_flag(Cli *cli, char *arg_cstr) {
    for (u8 i = 0; i < cli->optionals_count; ++i) {
        if (are_cstrings_equal(arg_cstr, cli->optionals[i].big_name,
                               strlen(cli->optionals[i].big_name)))
            return &cli->optionals[i];
        if (cli->optionals[i].small_name &&
            are_cstrings_equal(arg_cstr, cli->optionals[i].small_name,
                               strlen(cli->optionals[i].small_name)))
            return &cli->optionals[i];
    }
    return NULL;
}

void cli_show_usage_message(Cli *cli, FILE *stream) {
    Cli_Flag *temp_flag;

    if (cli->has_extra_info) {
        fprintf(stream, "%s %s\n%s\n\n", cli->program_name, cli->version, cli->program_description);
        fprintf(stream, "%s <%s>\n\n", cli->author_name, cli->email_address);
    }

    fprintf(stream, "Usage: %s ", cli->program_name);
    for (int i = 0; i < cli->positionals_count; ++i) {
        fprintf(stream, "%s ", cli->positionals[i].big_name);
    }
    fputs("[opts]\n", stream);

    fprintf(stream, "\nOptions:\n");
    for (int i = 0; i < cli->optionals_count; ++i) {
        temp_flag = &cli->optionals[i];
        if (temp_flag->type == Cli_FlagType_Bool) {
            fprintf(stream, "  -%s| --%s    %s\n", temp_flag->small_name, temp_flag->big_name,
                    temp_flag->description);
        } else {
            fprintf(stream, "  -%s| --%s=<%s>    %s\n", temp_flag->small_name, temp_flag->big_name,
                    flag_type_to_cstring[temp_flag->type], temp_flag->description);
        }
    }
    fputc('\n', stream);
}

static int strip_dashes(char **arg) {
    int c = 0;
    while (**arg && *(*arg)++ == '-')
        c++;
    (*arg)--;
    return c;
}

static int cli_find_and_set_value(Cli *cli, int curr_idx, char *curr, char *flag_name,
                                  char **flag_value) {
    if (*curr) {
        /* case: --flag=something
         * case: --flag =something */
        *flag_value = curr;
    } else {
        /* case: --flag= something
         * case: --flag = something */
        if (curr_idx + 1 >= cli->argc) {
            cli_report_error(cli, "error: expected value after '=', near '%s'\n", flag_name);
        } else {
            curr_idx += 1;
            *flag_value = cli->argv[curr_idx];
        }
    }

    return curr_idx;
}

/*
 * Extracts flag name and its value if its present or flag demands it.
 *
 * For example:
 *   --flag             , flag_name = flag, flag_value = NULL
 *   --flag = something , flag_name = flag, flag_value = something
 *   --flag=something   , flag_name = flag, flag_value = something
 *   --flag =something   , flag_name = flag, flag_value = something
 *
 *   -f                 , flag_name = f, flag_value = NULL
 */
static int split_flag_name_and_value(Cli *cli, int curr_idx, char **flag_name, char **flag_value) {
    char *curr = cli->argv[curr_idx];
    int dash_count = strip_dashes(&curr);

    /*
     * if (dash_count == 1) { // single dash aka small flags
     *    return curr_idx;
     * }
     *
     * TODO(madflash) - In case of short flag, parse multiple names
     *
     * For example:
     *    -hdlf  ---> h, d, l, f are all seperate flags, maybe all booleans, h for help etc
     */

    *flag_name = curr;
    while (*curr && *curr != '=') /* find equal sign */
        curr++;

    if (*curr && *curr == '=') { /* equal sign is present in arg */
        *curr = '\0';
        curr++;
        curr_idx = cli_find_and_set_value(cli, curr_idx, curr, *flag_name, flag_value);
    } else { /* equal sign is not present in arg */
        if (curr_idx + 1 >= cli->argc ||
            *cli->argv[curr_idx + 1] != '=') { /* didn't have any value. Skip it */
            return curr_idx;
        }

        curr_idx += 1;
        curr = cli->argv[curr_idx];
        curr++;
        curr_idx = cli_find_and_set_value(cli, curr_idx, curr, *flag_name, flag_value);
    }

    return curr_idx;
}

static inline bool cli_is_help_flag(char *flag_name, int flag_name_len) {
    if (flag_name_len > 4) return false;
    return (are_cstrings_equal("help", flag_name, flag_name_len) ||
            are_cstrings_equal("help", flag_name, flag_name_len))
               ? true
               : false;
}

static inline bool cli_is_flag_value_true(char *flag_value, int flag_value_len) {
    if (flag_value_len > 4) return false;
    return (are_cstrings_equal("true", flag_value, strlen(flag_value)) ||
            are_cstrings_equal("yes", flag_value, strlen(flag_value)) ||
            are_cstrings_equal("y", flag_value, strlen(flag_value)))
               ? true
               : false;
}

static inline bool cli_is_flag_value_false(char *flag_value, int flag_value_len) {
    if (flag_value_len > 5) return false;
    return (are_cstrings_equal("false", flag_value, strlen(flag_value)) ||
            are_cstrings_equal("no", flag_value, strlen(flag_value)) ||
            are_cstrings_equal("n", flag_value, strlen(flag_value)))
               ? true
               : false;
}

static void cli_set_value_bool(Cli *cli, bool *value, char *flag_name, char *flag_value) {
    if (flag_value) {
        if (cli_is_flag_value_true(flag_value, strlen(flag_value))) {
            *value = true;
        } else if (cli_is_flag_value_false(flag_value, strlen(flag_value))) {
            *value = false;
        } else {
            cli_report_error(cli,
                             "error: expected bool( yes,true,no,false,y,n ) "
                             "for '%s', got '%s'\n",
                             flag_name, flag_value);
        }
    } else {
        *value = *value ? false : true;
    }
}

static void cli_set_value_integer(Cli *cli, int *value, char *flag_name, char *flag_value) {
    char *endptr;

    errno = 0;
    long int val = strtol(flag_value, &endptr, 10);

    if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN)) || (errno != 0 && val == 0)) {
        cli_report_error(cli, "error: expected a 64-bit signed integer for '%s'\n", flag_name);
        return;
    }

    Assert(endptr >= flag_value);
    if (cast(usize)(endptr - flag_value) != strlen(flag_value)) {
        cli_report_error(cli, "error: expected a integer for '%s', got '%s'\n", flag_name,
                         flag_value);
        return;
    }

    *value = cast(int) val;
}

static void cli_handle_help_flag(Cli *cli, char *flag_name, char *flag_value) {
    bool is_help_flag_on = false;
    cli_set_value_bool(cli, &is_help_flag_on, flag_name, flag_value);

    if (is_help_flag_on) {
        cli_show_usage_message(cli, stdout);
        exit(1);
    }
}

static void cli_report_if_missing_positionals(Cli *cli, int idx) {
    if (idx != cli->positionals_count) {
        cli_report_error(cli, "error: missing positionals\n");
        for (; idx < cli->positionals_count; ++idx) {
            cli_report_error(cli, "  %s\n", cli->positionals[idx].big_name);
        }
    }
}

int cli_parse_args(Cli *cli) {
    Cli_Flag *found_flag;
    char *flag_name, *flag_value;
    int argv_idx, positionals_idx;

    argv_idx = 1;
    positionals_idx = 0;

    while (argv_idx < cli->argc) {
        if (*cli->argv[argv_idx] != '-') { /* Positional arg */
            if (positionals_idx >= cli->positionals_count) {
                cli_report_error(cli, "warning: ignoring '%s'\n", cli->argv[argv_idx]);
            } else {
                flag_value = cli->argv[argv_idx];
                found_flag = &cli->positionals[positionals_idx];

                switch (found_flag->type) {
                case Cli_FlagType_Bool:
                    cli_set_value_bool(cli, found_flag->value_ref._bool, found_flag->big_name,
                                       flag_value);
                    break;

                case Cli_FlagType_Int:
                    cli_set_value_integer(cli, found_flag->value_ref.integer, found_flag->big_name,
                                          flag_value);
                    break;

                case Cli_FlagType_CString: *found_flag->value_ref.cstring = flag_value; break;

                default: Unreachable();
                }

                positionals_idx += 1;
            }
        } else { /* Flag */
            flag_name = NULL;
            flag_value = NULL;
            argv_idx = split_flag_name_and_value(cli, argv_idx, &flag_name, &flag_value);
            found_flag = cli_find_optional_flag(cli, flag_name);

            if (cli_is_help_flag(flag_name, strlen(flag_name))) {
                cli_handle_help_flag(cli, flag_name, flag_value);
                goto next_iter;
            }

            if (!found_flag) {
                cli_report_error(cli, "error: unknown flag: %s\n", flag_name);
            } else {
                if (found_flag->type != Cli_FlagType_Bool && !flag_value) {
                    cli_report_error(cli, "error: expected %s for %s\n",
                                     flag_type_to_cstring[found_flag->type], flag_name);
                } else {
                    switch (found_flag->type) {
                    case Cli_FlagType_Bool:
                        cli_set_value_bool(cli, found_flag->value_ref._bool, flag_name, flag_value);
                        break;

                    case Cli_FlagType_Int:
                        cli_set_value_integer(cli, found_flag->value_ref.integer, flag_name,
                                              flag_value);
                        break;

                    case Cli_FlagType_CString: *found_flag->value_ref.cstring = flag_value; break;

                    default: Unreachable();
                    }
                }
            }
        }

    next_iter:
        argv_idx += 1;
    }

    cli_report_if_missing_positionals(cli, positionals_idx);

    return argv_idx;
}
