#define CLI_MORE_INFO
#define CLI_AUTHOR_NAME "madflash"
#define CLI_AUTHOR_EMAIL_ADDRESS "backbugkaught@gmail.com"
#define CLI_PROGRAM_DESC "intel 8080 assemblar"
#define CLI_VERSION "0.0.1"

#include "basic.c"
#include "cli.c"

// --------------------------------------------------------------------------
//                          - TOKEN_Kind -
// --------------------------------------------------------------------------
#define TOKEN_KINDS                                                                                \
    TOKEN_KIND(TOKEN_FAULTY, "Faulty token!!!")                                                    \
                                                                                                   \
    TOKEN_KIND(TOKEN_NUMBER_DECIMAL, "decimal number")                                             \
    TOKEN_KIND(TOKEN_NUMBER_BINARY, "binary number")                                               \
    TOKEN_KIND(TOKEN_NUMBER_HEX, "hexadecimal number")                                             \
    TOKEN_KIND(TOKEN_NUMBER_OCTAL, "octal number")                                                 \
    TOKEN_KIND(TOKEN_NUMBER_FLOAT, "float number")                                                 \
                                                                                                   \
    TOKEN_KIND(TOKEN_COMMA, "Comma")                                                               \
    TOKEN_KIND(TOKEN_COLON, "Colon")                                                               \
    TOKEN_KIND(TOKEN_SEMICOLON, "Semicolon")                                                       \
    TOKEN_KIND(TOKEN_IDENTIFIER, "Identifier")                                                     \
                                                                                                   \
    TOKEN_KIND(TOKEN_END_OF_FILE, "end_of_file")                                                   \
    TOKEN_KIND(TOKEN_Kind_COUNT, NULL)

#define is_token_number(kind)                                                                      \
    (kind == TOKEN_NUMBER_BINARY || kind == TOKEN_NUMBER_DECIMAL || kind == TOKEN_NUMBER_OCTAL ||  \
     kind == TOKEN_NUMBER_HEX || kind == TOKEN_NUMBER_FLOAT)

typedef enum {
#define TOKEN_KIND(kind_name, ...) kind_name,
    TOKEN_KINDS
#undef TOKEN_KIND
} TOKEN_Kind;

static char const *token_kind_to_cstring[] = {
#define TOKEN_KIND(kind_name, cstring) [kind_name] = cstring,
    TOKEN_KINDS
#undef TOKEN_KIND
};

typedef struct TOKEN_Pos TOKEN_Pos;
struct TOKEN_Pos {
    usize row, col;
};

typedef struct TOKEN TOKEN;
struct TOKEN {
    char *text;
    usize text_length;
    TOKEN_Kind kind;
    char *line_start;
    TOKEN_Pos pos;
};

static TOKEN make_token(char *text, size_t text_length, TOKEN_Kind kind, TOKEN_Pos pos,
                        char *line_start) {
    return (TOKEN){
        .text = text,
        .text_length = text_length,
        .kind = kind,
        .pos = pos,
        .line_start = line_start,
    };
}

static f64 number_token_to_f64(TOKEN *token) {
    Debug_Assert(is_token_number(token->kind));

    switch (token->kind) {
    case TOKEN_NUMBER_BINARY: return base2_to_f64(token->text + 2, token->text_length - 2);
    case TOKEN_NUMBER_DECIMAL: return base10_to_f64(token->text, token->text_length);
    case TOKEN_NUMBER_OCTAL: return base8_to_f64(token->text + 2, token->text_length - 2);
    case TOKEN_NUMBER_HEX: return base16_to_f64(token->text + 2, token->text_length - 2);

    default: Unreachable();
    }
}

// --------------------------------------------------------------------------
//                          - Lexer -
// --------------------------------------------------------------------------
typedef struct Lexer Lexer;
struct Lexer {
    bool exhausted;
    char *src, *curr, *end;
    usize row, col;

    int current_char;
    char *line_start;

    char *has_error; /* Error string otherwise NULL */
};

static TOKEN_Pos current_pos(Lexer *l) {
    return (TOKEN_Pos){.row = l->row, .col = l->col};
}

static char nextchar(Lexer *l) {
    Debug_Assert(l->curr + 1 < l->end);
    l->curr += 1;
    l->col += 1;
    l->current_char = *l->curr;
    return l->current_char;
}

static char *peeknext(Lexer *l) {
    if (l->curr + 1 >= l->end) return NULL;
    return l->curr + 1;
}

static bool is_num(Lexer *l) {
    char ch = l->current_char;

    if (l->current_char == '.') {
        if (peeknext(l)) {
            ch = *peeknext(l);
        }
    }
    return is_decimal_digit(ch);
}

static void skip_whitespaces(Lexer *l) {
    while (peeknext(l) && *peeknext(l) == ' ') {
        nextchar(l);
    }
}

static void skip_comments(Lexer *l) {
    while (peeknext(l) && *peeknext(l) != '\n') {
        nextchar(l);
    }
}

static void newline(Lexer *l) {
    l->row += 1;
    l->col = 0;
    *l->curr = '\0'; /* replace '\n' with \0, so line would be printted easly
                        using `line_start` in TOKEKN */
}

static void generic_number_scan(Lexer *l, bool (*is_vaild_digit)(int)) {
    char next;
    nextchar(l);

    if (!peeknext(l)) {
        l->has_error = "Expected a character after this"; /* lexer error */
        return;
    }

    next = *peeknext(l);
    if (!is_vaild_digit(next)) {
        l->has_error = "Invaild digit in number"; /* lexer error */
        return;
    }

    while (peeknext(l)) {
        next = *peeknext(l);
        if (next == ' ' || next == '\n') break;

        if (!is_vaild_digit(next)) {
            l->has_error = "Invaild digit"; /* lexer error */
            break;
        }

        nextchar(l);
    }
}

static TOKEN scan_num(Lexer *l) {
    TOKEN_Kind kind;
    TOKEN_Pos pos = current_pos(l);
    char *save = l->curr;

    if (l->current_char == '.') {
        kind = TOKEN_NUMBER_FLOAT;
        while (peeknext(l) && is_decimal_digit(*peeknext(l))) {
            nextchar(l);
        }
    } else {
        kind = TOKEN_NUMBER_DECIMAL;

        if (l->current_char == '0') {
            char *next = peeknext(l);
            if (next) {
                switch (*next) {
                case 'b':
                    kind = TOKEN_NUMBER_BINARY;
                    generic_number_scan(l, is_binary_digit);
                    break;
                case 'x':
                    kind = TOKEN_NUMBER_HEX;
                    generic_number_scan(l, is_hex_digit);
                    break;
                case 'o':
                    kind = TOKEN_NUMBER_OCTAL;
                    generic_number_scan(l, is_octal_digit);
                    break;
                }
            }
        }

        if (kind == TOKEN_NUMBER_DECIMAL) {
            while (peeknext(l) && (is_decimal_digit(*peeknext(l)) || *peeknext(l) == '.')) {
                if (*peeknext(l) == '.') {
                    kind = TOKEN_NUMBER_FLOAT;
                    nextchar(l);

                    while (peeknext(l) && is_decimal_digit(*peeknext(l)))
                        nextchar(l);
                    break;
                }
                nextchar(l);
            }
        }
    }

    return make_token(save, l->curr - save + 1, kind, pos, l->line_start);
}

static TOKEN scan_iden(Lexer *l) {
    size_t text_length;
    char *text = l->curr;
    TOKEN_Pos pos = current_pos(l);

    while (peeknext(l) && (*peeknext(l) == '_' || is_alphanumeric(*peeknext(l)))) {
        nextchar(l);
    }

    text_length = l->curr - text + 1;
    return make_token(text, text_length, TOKEN_IDENTIFIER, pos, l->line_start);
}

static TOKEN scan_punctuation(Lexer *l) {
    TOKEN_Kind kind;
    char *save = l->curr;
    TOKEN_Pos pos = current_pos(l);

    switch (l->current_char) {
    case ',': kind = TOKEN_COMMA; break;
    case ':': kind = TOKEN_COLON; break;
    case ';': kind = TOKEN_SEMICOLON; break;

    default: {
        l->has_error = "Unknown character";
        return make_token(save, l->curr - save + 1, TOKEN_FAULTY, pos, l->line_start); /* error */
    }
    }
    return make_token(save, l->curr - save + 1, kind, pos, l->line_start);
}

static Lexer *make_lexer(String *source_string) {
    Lexer *l = xmalloc(sizeof(Lexer));
    l->exhausted = false;

    // NOTE(madflash) - Append EOF so that we can print it ....
    usize previous_length = string_length(*source_string);
    *source_string = append_cstring(*source_string, " EOF");

    l->src = *source_string;
    l->curr = l->src - 1;
    l->end = l->src + previous_length;

    l->row = 0;
    l->col = -1;
    l->current_char = -1;
    l->line_start = l->src;
    l->has_error = NULL;
    return l;
}

static TOKEN lexer_next_token(Lexer *l) {
    Debug_Assert(!l->exhausted);

    skip_whitespaces(l);

    if (peeknext(l)) {
        nextchar(l);

        if (l->current_char == '\n') {
            newline(l);
            while (peeknext(l) && *peeknext(l) == '\n') {
                newline(l);
                nextchar(l);
            }

            /* set only if there is a new line starting after '\n', so when we
             * hit EOF, we would be pointing to second last line */
            if (peeknext(l)) {
                l->line_start = peeknext(l);
            }
            return lexer_next_token(l);
        } else if (is_num(l))
            return scan_num(l);
        else if (l->current_char == '_' || is_alphabet(l->current_char))
            return scan_iden(l);
        else if (l->current_char == ';') {
            skip_comments(l);
            return lexer_next_token(l);
        }

        return scan_punctuation(l);
    }

    // HACK(madflash) - Hide EOF whlie printing line, so add '\0' before EOF
    *l->end = '\0';
    l->curr = l->end + 1;
    l->col += 1;
    l->exhausted = true;
    return make_token(l->curr, 3, TOKEN_END_OF_FILE, current_pos(l), l->line_start);
}

static Array(TOKEN) slurp_tokens(Lexer *l) {
    TOKEN tok;
    Array(TOKEN) tokens;

    init_array(tokens);

    while (tok = lexer_next_token(l), tok.kind != TOKEN_END_OF_FILE) {
        array_push(tokens, tok);
    }

    array_push(tokens, tok);
    return tokens;
}

// --------------------------------------------------------------------------
//                          - Parser -
// --------------------------------------------------------------------------
typedef struct Parser Parser;
struct Parser {
    char *filepath; /* source filepath */

    Array(TOKEN) tokens;
    TOKEN *current_token;
    usize current_token_idx;

    int error_count;
};

static void parser_next_token(Parser *p) {
    assert(p->current_token_idx + 1 < array_length(p->tokens));
    p->current_token_idx += 1;
    p->current_token = &p->tokens[p->current_token_idx];
}

static TOKEN *parser_peek_next(Parser *p) {
    if (p->current_token_idx + 1 >= array_length(p->tokens)) return NULL;

    return &p->tokens[p->current_token_idx + 1];
}

static Parser *make_parser(char *filepath, String *src) {
    Parser *parser = xmalloc(sizeof(Parser));
    Lexer *lexer = make_lexer(src);

    parser->filepath = filepath;
    parser->tokens = slurp_tokens(lexer);
    parser->current_token_idx = -1;
    parser->current_token = NULL;

    free(lexer);
    return parser;
}

static void free_parser(Parser *p) {
    free_array(p->tokens);
    free(p);
}

// --------------------------------------------------------------------------
//                          - Error Printing -
// --------------------------------------------------------------------------
static void parser_log_error(Parser *p, TOKEN *faulty_token, char *prefix, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    fprintf(stderr, "%s:%zu:%zu: %s: ", p->filepath, faulty_token->pos.row, faulty_token->pos.col,
            prefix);
    vfprintf(stderr, msg, ap);
    va_end(ap);

    putc('\n', stderr);
    fprintf(stderr, "  %s\n", faulty_token->line_start);

    /* print swiggly lines under faulty_token */
    fprintf(stderr, "  %*.s^", (int)(faulty_token->text - faulty_token->line_start), "");
    for (usize i = 0; i < faulty_token->text_length; i++)
        putc('-', stderr);

    fprintf(stderr, "\n\n");
    p->error_count += 1;
}

static void parser_expect_next(Parser *p, TOKEN_Kind kind) {
    TOKEN *n = parser_peek_next(p);
    Assert(n);

    if (n && n->kind == kind) {
        parser_next_token(p);
    } else {
        parser_log_error(p, n, "Syntax Error", "Expected %s kind, got %s",
                         token_kind_to_cstring[kind], token_kind_to_cstring[n->kind]);
        parser_next_token(p);
    }
}

#define parser_match_current(parser, cstring)                                                      \
    are_cstrings_equal(cstring, parser->current_token->text, parser->current_token->text_length)

/* parser error abstraction */
#define parser_log_error_expected_register(p, registers)                                           \
    parser_log_error(parser, (p)->current_token, "Syntax Error",                                   \
                     "Expected Register( " #registers " ) instead got '%.*s'",                     \
                     (p)->current_token->text_length, (p)->current_token->text);

#define parser_log_error_expected_register_pair(p, register_pairs)                                 \
    parser_log_error(parser, (p)->current_token, "Syntax Error",                                   \
                     "Expected Register Pair( " #register_pairs " ) instead got '%.*s'",           \
                     (p)->current_token->text_length, (p)->current_token->text);

// --------------------------------------------------------------------------
//                          - Assemblar -
// --------------------------------------------------------------------------

typedef struct Label Label;
struct Label {
    TOKEN token;
    String name;
    u16 offset;
};

void deinit_label(Label *label) {
    free_string(label->name);
}

typedef struct Assemblar Assemblar;
struct Assemblar {
    char *source_filepath;
    String source_string;
    Array(Label) label_table;
    u8 bytecode[64 * 1024]; /* 64 Kb */
    u16 current_address;
    int error_count;
};

typedef enum {
    AssemblarResult_Ok,
    AssemblarResult_FailedToReadSourceFile,
    AssemblarResult_FailedToDumpObjectCode,
} AssemblarResult;

AssemblarResult init_assemblar(Assemblar *as, char *source_filepath) {
    as->source_filepath = source_filepath;

    as->source_string = file_as_string(source_filepath);
    if (!as->source_string) return AssemblarResult_FailedToReadSourceFile;

    init_array(as->label_table);

    memset(as->bytecode, 0x0, 64 * 1024);
    as->current_address = 0x0;
    as->error_count = 0;

    return AssemblarResult_Ok;
}

void deinit_assemblar(Assemblar *as) {
    free_string(as->source_string);

    array_for_each(as->label_table, i) { deinit_label(&as->label_table[i]); }
    free_array(as->label_table);
}

Label *assemblar_find_label(Assemblar *as, char *label_name, usize len) {
    array_for_each(as->label_table, i) {
        if (are_strings_equal_length(as->label_table[i].name, label_name, len))
            return &as->label_table[i];
    }
    return NULL;
}

void assemblar_insert_label(Assemblar *as, TOKEN token, u16 offset) {
    Label label = (Label){
        .token = token, .name = make_string(token.text, token.text_length), .offset = offset};
    array_push(as->label_table, label);
}

// --------------------------------------------------------------------------
//                          - Emit bytes -
// --------------------------------------------------------------------------
static void emit_byte(Assemblar *as, u8 byte) {
    as->bytecode[as->current_address] = byte;
    as->current_address += 1;
}

#define emit_mov_ins(as, parser, byteA, byteB, byteC, byteD, byteE, byteH, byteL, byteM)           \
    do {                                                                                           \
        parser_expect_next(parser, TOKEN_COMMA);                                                   \
        emit_ins_with_one_reg(as, parser, byteA, byteB, byteC, byteD, byteE, byteH, byteL, byteM); \
    } while (0)

static void emit_ins_with_one_reg(Assemblar *as, Parser *parser, u8 byteWithA, u8 byteB, u8 byteC,
                                  u8 byteD, u8 byteE, u8 byteH, u8 byteL, u8 byteM) {
    parser_expect_next(parser, TOKEN_IDENTIFIER);
    if (parser->current_token->text_length > 1) {
        parser_log_error_expected_register(parser, a b c d e h l m);
        return;
    }

    if (parser_match_current(parser, "a"))
        emit_byte(as, byteWithA);
    else if (parser_match_current(parser, "b"))
        emit_byte(as, byteB);
    else if (parser_match_current(parser, "c"))
        emit_byte(as, byteC);
    else if (parser_match_current(parser, "d"))
        emit_byte(as, byteD);
    else if (parser_match_current(parser, "e"))
        emit_byte(as, byteE);
    else if (parser_match_current(parser, "h"))
        emit_byte(as, byteH);
    else if (parser_match_current(parser, "l"))
        emit_byte(as, byteL);
    else if (parser_match_current(parser, "m"))
        emit_byte(as, byteM);
    else {
        parser_log_error_expected_register(parser, a b c d e h l m);
    }
}

static void emit_ins_with_rp(Assemblar *as, Parser *parser, u8 bByte, u8 dByte, u8 hByte,
                             u8 spByte) {
    parser_expect_next(parser, TOKEN_IDENTIFIER);
    if (parser->current_token->text_length > 2) {
        parser_log_error_expected_register_pair(parser, b d h sp);
        return;
    }

    if (parser_match_current(parser, "b"))
        emit_byte(as, bByte);
    else if (parser_match_current(parser, "d"))
        emit_byte(as, dByte);
    else if (parser_match_current(parser, "h"))
        emit_byte(as, hByte);
    else if (parser_match_current(parser, "sp"))
        emit_byte(as, spByte);
    else {
        parser_log_error_expected_register_pair(parser, b d h sp);
    }
}

static bool assemblar_get_addr_operand(Assemblar *as, Parser *parser, u16 *addr) {
    if (parser->current_token->kind == TOKEN_IDENTIFIER) {
        Label *label = assemblar_find_label(as, parser->current_token->text,
                                            parser->current_token->text_length);
        if (!label) {
            parser_log_error(parser, parser->current_token, "Error", "Label not defined '%.*s'",
                             parser->current_token->text_length, parser->current_token->text);
            return false;
        }

        *addr = label->offset;
        return true;
    } else if (is_token_number(parser->current_token->kind)) {
        *addr = cast(u16) number_token_to_f64(parser->current_token);
        return true;
    } else {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected address or label, instead got '%.*s'",
                         parser->current_token->text_length, parser->current_token->text);
        return false;
    }
}

static void emit_ins_with_addr(Assemblar *as, Parser *parser, u8 opByte) {
    emit_byte(as, opByte);
    parser_next_token(parser);

    u16 address_operand;
    if (!assemblar_get_addr_operand(as, parser, &address_operand)) {
        return;
    }

    emit_byte(as, cast(u8)((u16)address_operand & 0xff));
    emit_byte(as, cast(u8)(((u16)address_operand >> 8) & 0xff));
}

static void emit_ins_with_imm_byte(Assemblar *as, Parser *parser, u8 emitByte) {
    emit_byte(as, emitByte);
    parser_next_token(parser);

    if (!is_token_number(parser->current_token->kind)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected immediate byte, instead got '%*.s'",
                         parser->current_token->text_length, parser->current_token->text);
    } else {
        f64 num = number_token_to_f64(parser->current_token);
        emit_byte(as, cast(u8)((uint)num & 0xff));
    }
}

static void emit_mvi_ins(Assemblar *as, Parser *parser, u8 emitByte) {
    parser_expect_next(parser, TOKEN_COMMA);
    parser_next_token(parser);

    if (!is_token_number(parser->current_token->kind)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected immediate byte, instead got '%*.s'",
                         parser->current_token->text_length, parser->current_token->text);
    } else {
        emit_byte(as, emitByte);
        f64 num = number_token_to_f64(parser->current_token);
        emit_byte(as, cast(u8)((uint)num & 0xff));
    }
}

static void emit_stack_op_ins(Assemblar *as, Parser *parser, u8 byteBC, u8 byteDE, u8 byteHL,
                              u8 bytePSW) {
    parser_expect_next(parser, TOKEN_IDENTIFIER);

    if (parser->current_token->text_length > 3) {
        parser_log_error_expected_register_pair(parser, b d h PSW);
        return;
    }

    if (parser_match_current(parser, "b"))
        emit_byte(as, byteBC);
    else if (parser_match_current(parser, "d"))
        emit_byte(as, byteDE);
    else if (parser_match_current(parser, "h"))
        emit_byte(as, byteHL);
    else if (parser_match_current(parser, "psw"))
        emit_byte(as, bytePSW);
    else
        parser_log_error_expected_register_pair(parser, b d h PSW);
}

static void emit_rst_ins(Assemblar *as, Parser *parser, u8 byte0, u8 byte1, u8 byte2, u8 byte3,
                         u8 byte4, u8 byte5, u8 byte6, u8 byte7) {
    parser_next_token(parser);

    if (!is_token_number(parser->current_token->kind)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected number(0..7), instead got '%*.s'",
                         parser->current_token->text_length, parser->current_token->text);
        return;
    }

    int num = cast(int) number_token_to_f64(parser->current_token);

    switch (num) {
    case 0: emit_byte(as, byte0);
    case 1: emit_byte(as, byte1);
    case 2: emit_byte(as, byte2);
    case 3: emit_byte(as, byte3);
    case 4: emit_byte(as, byte4);
    case 5: emit_byte(as, byte5);
    case 6: emit_byte(as, byte6);
    case 7: emit_byte(as, byte7);
    default:
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected number(0..7), instead got '%d'", cast(int) num);
    }
}

static void emit_ldax_ins(Assemblar *as, Parser *parser, u8 byteBC, u8 byteDE) {
    parser_expect_next(parser, TOKEN_IDENTIFIER);

    if (parser_match_current(parser, "b"))
        emit_byte(as, byteBC);
    else if (parser_match_current(parser, "d"))
        emit_byte(as, byteDE);
    else {
        parser_log_error_expected_register_pair(parser, b d h);
    }
}

static void emit_lxi_ins(Assemblar *as, Parser *parser, u8 byteBC, u8 byteDE, u8 byteHL,
                         u8 byteSP) {
    bool has_error = false;
    parser_expect_next(parser, TOKEN_IDENTIFIER);

    if (parser->current_token->text_length > 2) {
        has_error = true;
        parser_log_error_expected_register_pair(parser, b d h);
    } else {
        if (parser_match_current(parser, "b"))
            emit_byte(as, byteBC);
        else if (parser_match_current(parser, "d"))
            emit_byte(as, byteDE);
        else if (parser_match_current(parser, "h"))
            emit_byte(as, byteHL);
        else if (parser_match_current(parser, "sp"))
            emit_byte(as, byteSP);
        else {
            has_error = true;
            parser_log_error_expected_register_pair(parser, b d h);
        }
    }

    parser_expect_next(parser, TOKEN_COMMA);
    parser_next_token(parser);

    u16 address_operand;
    if (!assemblar_get_addr_operand(as, parser, &address_operand)) {
        return;
    }

    if (!has_error) {
        emit_byte(as, cast(u8)(address_operand & 0xff));
        emit_byte(as, cast(u8)((address_operand >> 8) & 0xff));
    }
}

static char const *label_name_not_allowed_lowercase[] = {
    "mov",
};

void assemblar_emit_object_code(Assemblar *as) {
    Parser *parser = make_parser(as->source_filepath, &as->source_string);
    TOKEN *next = parser_peek_next(parser);

    while (next && next->kind != TOKEN_END_OF_FILE) {
#define match_current(cstring)                                                                     \
    are_cstrings_equal(cstring, parser->current_token->text, parser->current_token->text_length)

        parser_next_token(parser);

        if (parser->current_token->kind == TOKEN_IDENTIFIER) {
            next = parser_peek_next(parser);

            /* label */
            if (next && next->kind == TOKEN_COLON) {
                assemblar_insert_label(as, *parser->current_token, as->current_address);
                parser_next_token(parser);
            } else {
                /* halt */
                if (match_current("hlt")) {
                    emit_byte(as, 0x76);
                }

                /* start: Mov instruction */
                else if (match_current("mov")) {
                    parser_expect_next(parser, TOKEN_IDENTIFIER);

                    if (match_current("a")) {
                        emit_mov_ins(as, parser, 0x7f, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e);
                    } else if (match_current("b")) {

                        emit_mov_ins(as, parser, 0x47, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46);
                    } else if (match_current("c")) {
                        emit_mov_ins(as, parser, 0x4f, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e);
                    } else if (match_current("d")) {
                        emit_mov_ins(as, parser, 0x57, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56);
                    } else if (match_current("e")) {
                        emit_mov_ins(as, parser, 0x5f, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e);
                    } else if (match_current("h")) {
                        emit_mov_ins(as, parser, 0x67, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66);
                    } else if (match_current("l")) {
                        emit_mov_ins(as, parser, 0x6f, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e);
                    } else if (match_current("m")) {
                        parser_expect_next(parser, TOKEN_COMMA);
                        parser_expect_next(parser, TOKEN_IDENTIFIER);

                        if (match_current("a"))
                            emit_byte(as, 0x77);
                        else if (match_current("b"))
                            emit_byte(as, 0x70);
                        else if (match_current("c"))
                            emit_byte(as, 0x71);
                        else if (match_current("d"))
                            emit_byte(as, 0x72);
                        else if (match_current("e"))
                            emit_byte(as, 0x73);
                        else if (match_current("h"))
                            emit_byte(as, 0x74);
                        else if (match_current("l"))
                            emit_byte(as, 0x75);
                        else
                            parser_log_error_expected_register(parser, a b c d e h l);
                    }
                }
                /* end: Mov instruction */

                /* start: instructions with single register operand */
                else if (match_current("add"))
                    emit_ins_with_one_reg(as, parser, 0x87, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85,
                                          0x86);
                else if (match_current("adc"))
                    emit_ins_with_one_reg(as, parser, 0x8f, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d,
                                          0x8e);
                else if (match_current("sub"))
                    emit_ins_with_one_reg(as, parser, 0x87, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95,
                                          0x96);
                else if (match_current("sbb"))
                    emit_ins_with_one_reg(as, parser, 0x9f, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d,
                                          0x9e);
                else if (match_current("ana"))
                    emit_ins_with_one_reg(as, parser, 0xa7, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5,
                                          0xa6);
                else if (match_current("xra"))
                    emit_ins_with_one_reg(as, parser, 0xaf, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad,
                                          0xae);
                else if (match_current("ora"))
                    emit_ins_with_one_reg(as, parser, 0xb7, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5,
                                          0xb6);
                else if (match_current("cmp"))
                    emit_ins_with_one_reg(as, parser, 0xbf, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd,
                                          0xbe);
                else if (match_current("inr"))
                    emit_ins_with_one_reg(as, parser, 0x3c, 0x04, 0x0c, 0x14, 0x1c, 0x24, 0x2c,
                                          0x34);
                else if (match_current("dcr"))
                    emit_ins_with_one_reg(as, parser, 0x3d, 0x05, 0x0d, 0x15, 0x1d, 0x25, 0x2d,
                                          0x35);
                /* end: instructions with single register operand */

                /* start: instruction with register pairs */
                else if (match_current("inx"))
                    emit_ins_with_rp(as, parser, 0x03, 0x13, 0x23, 0x33);
                else if (match_current("dcx"))
                    emit_ins_with_rp(as, parser, 0x0b, 0x1b, 0x2b, 0x3b);
                else if (match_current("dad"))
                    emit_ins_with_rp(as, parser, 0x09, 0x19, 0x29, 0x39);
                /* end: instruction with register pairs */

                /* start: rotation instructions */
                else if (match_current("rlc"))
                    emit_byte(as, 0x07);
                else if (match_current("rrc"))
                    emit_byte(as, 0x0f);
                else if (match_current("ral"))
                    emit_byte(as, 0x17);
                else if (match_current("rar"))
                    emit_byte(as, 0x1f);
                /* end: rotation instructions */

                else if (match_current("cma")) /* cma instruction */
                    emit_byte(as, 0x2f);

                /* start: carry instructions */
                else if (match_current("cmc"))
                    emit_byte(as, 0x3f);
                else if (match_current("stc"))
                    emit_byte(as, 0x37);
                /* end: carry instructions */

                /* start: return instructions */
                else if (match_current("ret"))
                    emit_byte(as, 0xc9);
                else if (match_current("rz"))
                    emit_byte(as, 0xc8);
                else if (match_current("rnz"))
                    emit_byte(as, 0xc0);
                else if (match_current("rc"))
                    emit_byte(as, 0xd8);
                else if (match_current("rnc"))
                    emit_byte(as, 0xd0);
                else if (match_current("rpo"))
                    emit_byte(as, 0xe0);
                else if (match_current("rpe"))
                    emit_byte(as, 0xe8);
                /* end: return instructions */

                /* start: jump instructions */
                else if (match_current("jmp"))
                    emit_ins_with_addr(as, parser, 0xc3);
                else if (match_current("jc"))
                    emit_ins_with_addr(as, parser, 0xda);
                else if (match_current("jnc"))
                    emit_ins_with_addr(as, parser, 0xd2);
                else if (match_current("jz"))
                    emit_ins_with_addr(as, parser, 0xca);
                else if (match_current("jnz"))
                    emit_ins_with_addr(as, parser, 0xc2);
                else if (match_current("jpo"))
                    emit_ins_with_addr(as, parser, 0xe2);
                else if (match_current("jpe"))
                    emit_ins_with_addr(as, parser, 0xea);
                else if (match_current("jp"))
                    emit_ins_with_addr(as, parser, 0xf2);
                else if (match_current("jm"))
                    emit_ins_with_addr(as, parser, 0xfa);
                /* end: jump instructions */

                /* start: call instructions */
                else if (match_current("call"))
                    emit_ins_with_addr(as, parser, 0xcd);
                else if (match_current("cc"))
                    emit_ins_with_addr(as, parser, 0xdc);
                else if (match_current("cnc"))
                    emit_ins_with_addr(as, parser, 0xd4);
                else if (match_current("cz"))
                    emit_ins_with_addr(as, parser, 0xcc);
                else if (match_current("cnz"))
                    emit_ins_with_addr(as, parser, 0xc4);
                else if (match_current("cpo"))
                    emit_ins_with_addr(as, parser, 0xe4);
                else if (match_current("cpe"))
                    emit_ins_with_addr(as, parser, 0xec);
                else if (match_current("cp"))
                    emit_ins_with_addr(as, parser, 0xf4);
                else if (match_current("cm"))
                    emit_ins_with_addr(as, parser, 0xfc);
                /* end: call instructions */

                /* start: store/load instructions */
                else if (match_current("shld")) /* shld */
                    emit_ins_with_addr(as, parser, 0x22);
                else if (match_current("lhld")) /* lhld */
                    emit_ins_with_addr(as, parser, 0x28);
                else if (match_current("sta")) /* sta */
                    emit_ins_with_addr(as, parser, 0x32);
                else if (match_current("lda")) /* lda */
                    emit_ins_with_addr(as, parser, 0x3a);
                else if (match_current("ldax")) /* ldax */
                    emit_ldax_ins(as, parser, 0x0a, 0x1a);
                else if (match_current("lxi")) /* lxi */
                    emit_lxi_ins(as, parser, 0x01, 0x11, 0x21, 0x31);
                /* end: store/load instructions */

                /* start: 1 byte instructions with 8-bit immediate data */
                else if (match_current("adi")) /* adi */
                    emit_ins_with_imm_byte(as, parser, 0xc6);
                else if (match_current("aci")) /* aci */
                    emit_ins_with_imm_byte(as, parser, 0xce);
                else if (match_current("sui")) /* sui */
                    emit_ins_with_imm_byte(as, parser, 0xd6);
                else if (match_current("sbi")) /* sbi */
                    emit_ins_with_imm_byte(as, parser, 0xde);
                else if (match_current("ani")) /* ani */
                    emit_ins_with_imm_byte(as, parser, 0xe6);
                else if (match_current("xri")) /* xri */
                    emit_ins_with_imm_byte(as, parser, 0xee);
                else if (match_current("ori")) /* ori */
                    emit_ins_with_imm_byte(as, parser, 0xf6);
                else if (match_current("cpi")) /* cpi */
                    emit_ins_with_imm_byte(as, parser, 0xfe);
                else if (match_current("out")) /* out */
                    emit_ins_with_imm_byte(as, parser, 0x3a);
                else if (match_current("in")) /* in */
                    emit_ins_with_imm_byte(as, parser, 0x3a);
                /* end: 1 byte instructions with 8-bit immediate data */

                /* start: mvi instruction */
                else if (match_current("mvi")) {
                    parser_expect_next(parser, TOKEN_IDENTIFIER);

                    if (match_current("a"))
                        emit_mvi_ins(as, parser, 0x3e);
                    else if (match_current("b"))
                        emit_mvi_ins(as, parser, 0x06);
                    else if (match_current("c"))
                        emit_mvi_ins(as, parser, 0x0e);
                    else if (match_current("d"))
                        emit_mvi_ins(as, parser, 0x16);
                    else if (match_current("e"))
                        emit_mvi_ins(as, parser, 0x1e);
                    else if (match_current("h"))
                        emit_mvi_ins(as, parser, 0x26);
                    else if (match_current("l"))
                        emit_mvi_ins(as, parser, 0x2e);
                    else if (match_current("m"))
                        emit_mvi_ins(as, parser, 0x36);
                    else {
                        parser_log_error_expected_register(parser, a b c d e h l m);
                    }
                }
                /* end: mvi instruction */

                /* start: stack instruction */
                else if (match_current("push"))
                    emit_stack_op_ins(as, parser, 0xc5, 0xd5, 0xe5, 0xf5);
                else if (match_current("pop"))
                    emit_stack_op_ins(as, parser, 0xc1, 0xd1, 0xe1, 0xf1);
                else if (match_current("pchl"))
                    emit_byte(as, 0xe9);
                else if (match_current("xthl"))
                    emit_byte(as, 0xe3);
                /* end: stack instruction */

                /* start: RST instructions */
                else if (match_current("rst"))
                    emit_rst_ins(as, parser, 0xc7, 0xcf, 0xd7, 0xdf, 0xe7, 0xef, 0xf7, 0xff);
                /* end: RST instructions */

                /* start: Interrupt Enable/Disable instructions */
                else if (match_current("ei"))
                    emit_byte(as, 0xfb);
                else if (match_current("di"))
                    emit_byte(as, 0xf3);
                /* end: Interrupt Enable/Disable instructions */

                /* Last error */
                else {
                    parser_log_error(
                        parser, parser->current_token, "Error", "Unknown Instruction '%.*s' ",
                        parser->current_token->text_length, parser->current_token->text);
                }
            }
        }

        next = parser_peek_next(parser);
    }

    as->error_count = parser->error_count; /* set assemblar's error_count */
    free_parser(parser);
}

AssemblarResult assemblar_dump_bytecode(Assemblar *as, char *output_filepath) {
    if (!output_filepath) {
        for (u16 i = 0; i < as->current_address; ++i)
            println("0x%x", as->bytecode[i]);
        return AssemblarResult_Ok;
    }

    FILE *out = fopen(output_filepath, "w+");
    if (!out) return AssemblarResult_FailedToDumpObjectCode;

    fwrite(as->bytecode, sizeof(u8), as->current_address, out);
    fclose(out);
    return AssemblarResult_Ok;
}

int main(int argc, char **argv) {
    char *source_filepath;
    char *output_filepath = NULL;

    Cli_Flag positionals[] = {
        Flag_CString_Positional(&source_filepath, "SOURCE_FILEPATH", "asm source filepath")};

    Cli_Flag optionals[] = {
        Flag_CString(&output_filepath, "o", "output", "output binary filepath"),
    };

    Cli cli =
        create_cli(argc, argv, "assemblar8080", positionals, array_sizeof(positionals, Cli_Flag),
                   optionals, array_sizeof(optionals, Cli_Flag));
    cli_parse_args(&cli);

    if (cli_has_error(&cli)) {
        exit(EXIT_FAILURE);
    }

    Assemblar assemblar;
    AssemblarResult result = init_assemblar(&assemblar, source_filepath);
    if (result != AssemblarResult_Ok) {
        eprintln("error: couldn't initialize assemblar for file: %s", source_filepath);
        exit(EXIT_FAILURE);
    }

    assemblar_emit_object_code(&assemblar);

    if (assemblar_dump_bytecode(&assemblar, output_filepath) != AssemblarResult_Ok) {
        eprintln("error: couldn't dump object code in file: %s", output_filepath);
        exit(EXIT_FAILURE);
    }

    array_for_each(assemblar.label_table, i) {
        debug_println("labels: ");
        debug_println("name: %s, length: %zu, addr: %d", assemblar.label_table[i].name,
                      string_length(assemblar.label_table[i].name),
                      assemblar.label_table[i].offset);
    }

    deinit_assemblar(&assemblar);
    return 0;
}
