#include "assemblar.h"
#include "common.h"

// --------------------------------------------------------------------------
//                          - TOKEN_Kind -
// --------------------------------------------------------------------------
#define TOKEN_KINDS                                                            \
    TOKEN_KIND(TOKEN_FAULTY, "Faulty token!!!")                                \
                                                                               \
    TOKEN_KIND(TOKEN_NUMBER_DECIMAL, "decimal number")                         \
    TOKEN_KIND(TOKEN_NUMBER_BINARY, "binary number")                           \
    TOKEN_KIND(TOKEN_NUMBER_HEX, "hexadecimal number")                         \
    TOKEN_KIND(TOKEN_NUMBER_OCTAL, "octal number")                             \
    TOKEN_KIND(TOKEN_NUMBER_FLOAT, "float number")                             \
                                                                               \
    TOKEN_KIND(TOKEN_COMMA, "Comma")                                           \
    TOKEN_KIND(TOKEN_COLON, "Colon")                                           \
    TOKEN_KIND(TOKEN_SEMICOLON, "Semicolon")                                   \
    TOKEN_KIND(TOKEN_IDENTIFIER, "Identifier")                                 \
                                                                               \
    TOKEN_KIND(TOKEN_END_OF_FILE, "end_of_file")                               \
    TOKEN_KIND(TOKEN_Kind_COUNT, NULL)

#define is_token_number(kind)                                                  \
    (kind == TOKEN_NUMBER_BINARY || kind == TOKEN_NUMBER_DECIMAL ||            \
     kind == TOKEN_NUMBER_OCTAL || kind == TOKEN_NUMBER_HEX ||                 \
     kind == TOKEN_NUMBER_FLOAT)

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

static TOKEN make_token(char *text, size_t text_length, TOKEN_Kind kind,
                        TOKEN_Pos pos, char *line_start) {
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
    case TOKEN_NUMBER_BINARY:
        return base2_to_f64(token->text + 2, token->text_length - 2);
    case TOKEN_NUMBER_DECIMAL:
        return base10_to_f64(token->text, token->text_length);
    case TOKEN_NUMBER_OCTAL:
        return base8_to_f64(token->text + 2, token->text_length - 2);
    case TOKEN_NUMBER_HEX:
        return base16_to_f64(token->text + 2, token->text_length - 2);

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
            while (peeknext(l) &&
                   (is_decimal_digit(*peeknext(l)) || *peeknext(l) == '.')) {
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

    while (peeknext(l) &&
           (*peeknext(l) == '_' || is_alphanumeric(*peeknext(l)))) {
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
        return make_token(save, l->curr - save + 1, TOKEN_FAULTY, pos,
                          l->line_start); /* error */
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
        } else if (is_num(l))
            return scan_num(l);
        else if (l->current_char == '_' || is_alphabet(l->current_char))
            return scan_iden(l);

        return scan_punctuation(l);
    }

    // HACK(madflash) - Hide EOF whlie printing line, so add '\0' before EOF
    *l->end = '\0';
    l->curr = l->end + 1;
    l->col += 1;
    l->exhausted = true;
    return make_token(l->curr, 3, TOKEN_END_OF_FILE, current_pos(l),
                      l->line_start);
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
static void parser_log_error(Parser *p, TOKEN *faulty_token, char *prefix,
                             char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    fprintf(stderr, "%s:%zu:%zu: %s: ", p->filepath, faulty_token->pos.row,
            faulty_token->pos.col, prefix);
    vfprintf(stderr, msg, ap);
    va_end(ap);

    putc('\n', stderr);
    fprintf(stderr, "  %s\n", faulty_token->line_start);

    /* print swiggly lines under faulty_token */
    fprintf(stderr, "  %*.s^",
            (int)(faulty_token->text - faulty_token->line_start), "");
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
                         token_kind_to_cstring[kind],
                         token_kind_to_cstring[n->kind]);
        parser_next_token(p);
    }
}

#define parser_match_current(parser, cstring)                                  \
    are_cstrings_equal(cstring, parser->current_token->text,                   \
                       parser->current_token->text_length)

/* parser error abstraction */
#define parser_log_error_expected_register(p, registers)                       \
    parser_log_error(parser, (p)->current_token, "Syntax Error",               \
                     "Expected Register( " #registers " ) instead got '%.*s'", \
                     (p)->current_token->text_length,                          \
                     (p)->current_token->text);

#define parser_log_error_expected_register_pair(p, register_pairs)             \
    parser_log_error(                                                          \
        parser, (p)->current_token, "Syntax Error",                            \
        "Expected Register Pair( " #register_pairs " ) instead got '%.*s'",    \
        (p)->current_token->text_length, (p)->current_token->text);

// --------------------------------------------------------------------------
//                          - Assemblar -
// --------------------------------------------------------------------------

typedef struct Assemblar Assemblar;
struct Assemblar {
    char *source_filepath;
    String source_string;
    Parser *parser;
    Array(TOKEN) labels;
};

Tape make_tape() {
    Tape tape;
    memset(tape.bytecode, 0x0, 64 * 1024);
    tape.bytecode_count = 0x0;
    tape.origin = 0x0;
    tape.error_count = 0;
    return tape;
}

Assemblar *make_assemblar(char *source_filepath) {
    Assemblar *as = xmalloc(sizeof(Assemblar));
    as->source_filepath = source_filepath;
    as->source_string = file_as_string(source_filepath);
    as->parser = make_parser(source_filepath, &as->source_string);
    init_array(as->labels);
    return as;
}

void free_assemblar(Assemblar *as) {
    free_string(as->source_string);
    free_parser(as->parser);
    free_array(as->labels);
    free(as);
}

// --------------------------------------------------------------------------
//                          - Emit bytes -
// --------------------------------------------------------------------------
static void emit_byte(Tape *tape, u8 byte) {
    tape->bytecode[tape->bytecode_count] = byte;
    tape->bytecode_count += 1;
}

#define ins_mov_next_reg(tape, parser, byteA, byteB, byteC, byteD, byteE,      \
                         byteH, byteL, byteM)                                  \
    do {                                                                       \
        parser_expect_next(parser, TOKEN_COMMA);                               \
        ins_with_one_register(tape, parser, byteA, byteB, byteC, byteD, byteE, \
                              byteH, byteL, byteM);                            \
    } while (0)

static void ins_with_one_register(Tape *tape, Parser *parser, u8 byteWithA,
                                  u8 byteB, u8 byteC, u8 byteD, u8 byteE,
                                  u8 byteH, u8 byteL, u8 byteM) {
    parser_expect_next(parser, TOKEN_IDENTIFIER);
    if (parser_match_current(parser, "a"))
        emit_byte(tape, byteWithA);
    else if (parser_match_current(parser, "b"))
        emit_byte(tape, byteB);
    else if (parser_match_current(parser, "c"))
        emit_byte(tape, byteC);
    else if (parser_match_current(parser, "d"))
        emit_byte(tape, byteD);
    else if (parser_match_current(parser, "e"))
        emit_byte(tape, byteE);
    else if (parser_match_current(parser, "h"))
        emit_byte(tape, byteH);
    else if (parser_match_current(parser, "l"))
        emit_byte(tape, byteL);
    else if (parser_match_current(parser, "m"))
        emit_byte(tape, byteM);
    else {
        parser_log_error_expected_register(parser, a b c d e h l m);
    }
}

static void ins_with_register_pair(Tape *tape, Parser *parser, u8 bByte,
                                   u8 dByte, u8 hByte, u8 spByte) {
    parser_expect_next(parser, TOKEN_IDENTIFIER);

    if (parser_match_current(parser, "b"))
        emit_byte(tape, bByte);
    else if (parser_match_current(parser, "d"))
        emit_byte(tape, dByte);
    else if (parser_match_current(parser, "h"))
        emit_byte(tape, hByte);
    else if (parser_match_current(parser, "sp"))
        emit_byte(tape, spByte);
    else {
        parser_log_error_expected_register_pair(parser, b d h sp);
    }
}

static void ins_with_address(Tape *tape, Parser *parser, u8 emitByte) {
    emit_byte(tape, emitByte);
    parser_next_token(parser);

    if (!is_token_number(parser->current_token->kind)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected address, "
                         "instead got '%.*s'",
                         parser->current_token->text_length,
                         parser->current_token->text);
    } else {
        f64 num = number_token_to_f64(parser->current_token);
        emit_byte(tape, cast(u8)((uint)num & 0xff));
        emit_byte(tape, cast(u8)(((uint)num >> 8) & 0xff));
    }
}

static void ins_with_imm_byte(Tape *tape, Parser *parser, u8 emitByte) {
    emit_byte(tape, emitByte);
    parser_next_token(parser);

    if (!is_token_number(parser->current_token->kind)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected immediate byte, instead got '%*.s'",
                         parser->current_token->text_length,
                         parser->current_token->text);
    } else {
        f64 num = number_token_to_f64(parser->current_token);
        emit_byte(tape, cast(u8)((uint)num & 0xff));
    }
}

static void ins_mvi(Tape *tape, Parser *parser, u8 emitByte) {
    parser_expect_next(parser, TOKEN_COMMA);
    parser_next_token(parser);

    if (!is_token_number(parser->current_token->kind)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected immediate byte, instead got '%*.s'",
                         parser->current_token->text_length,
                         parser->current_token->text);
    } else {
        emit_byte(tape, emitByte);
        f64 num = number_token_to_f64(parser->current_token);
        emit_byte(tape, cast(u8)((uint)num & 0xff));
    }
}

static void ins_stack_ops(Tape *tape, Parser *parser, u8 byteBC, u8 byteDE,
                          u8 byteHL, u8 bytePSW) {
    parser_expect_next(parser, TOKEN_IDENTIFIER);

    if (parser_match_current(parser, "b"))
        emit_byte(tape, byteBC);
    else if (parser_match_current(parser, "d"))
        emit_byte(tape, byteDE);
    else if (parser_match_current(parser, "h"))
        emit_byte(tape, byteHL);
    else if (parser_match_current(parser, "psw"))
        emit_byte(tape, bytePSW);
    else
        parser_log_error_expected_register_pair(parser, b d h PSW);
}

static void ins_rst(Tape *tape, Parser *parser, u8 byte0, u8 byte1, u8 byte2,
                    u8 byte3, u8 byte4, u8 byte5, u8 byte6, u8 byte7) {
    parser_next_token(parser);

    if (!is_token_number(parser->current_token->kind)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected number(0..7), instead got '%*.s'",
                         parser->current_token->text_length,
                         parser->current_token->text);
        return;
    }

    int num = cast(int) number_token_to_f64(parser->current_token);

    switch (num) {
    case 0: emit_byte(tape, byte0);
    case 1: emit_byte(tape, byte1);
    case 2: emit_byte(tape, byte2);
    case 3: emit_byte(tape, byte3);
    case 4: emit_byte(tape, byte4);
    case 5: emit_byte(tape, byte5);
    case 6: emit_byte(tape, byte6);
    case 7: emit_byte(tape, byte7);
    default:
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected number(0..7), instead got '%d'",
                         cast(int) num);
    }
}

static void ins_ldax(Tape *tape, Parser *parser, u8 byteBC, u8 byteDE) {
    parser_expect_next(parser, TOKEN_IDENTIFIER);

    if (parser_match_current(parser, "b"))
        emit_byte(tape, byteBC);
    else if (parser_match_current(parser, "d"))
        emit_byte(tape, byteDE);
    else {
        parser_log_error_expected_register_pair(parser, b d h);
    }
}

static void ins_lxi(Tape *tape, Parser *parser, u8 byteBC, u8 byteDE, u8 byteHL,
                    u8 byteSP) {
    bool has_error = false;
    parser_expect_next(parser, TOKEN_IDENTIFIER);

    if (parser_match_current(parser, "b"))
        emit_byte(tape, byteBC);
    else if (parser_match_current(parser, "d"))
        emit_byte(tape, byteDE);
    else if (parser_match_current(parser, "h"))
        emit_byte(tape, byteHL);
    else if (parser_match_current(parser, "sp"))
        emit_byte(tape, byteSP);
    else {
        has_error = true;
        parser_log_error_expected_register_pair(parser, b d h);
    }

    parser_expect_next(parser, TOKEN_COMMA);
    parser_next_token(parser);

    if (!is_token_number(parser->current_token->kind)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected address, "
                         "instead got '%.*s'",
                         parser->current_token->text_length,
                         parser->current_token->text);
    } else {
        f64 num = number_token_to_f64(parser->current_token);

        if (!has_error) {
            emit_byte(tape, cast(u8)((uint)num & 0xff));
            emit_byte(tape, cast(u8)(((uint)num >> 8) & 0xff));
        }
    }
}

// --------------------------------------------------------------------------
//                - Initialize tape from source file -
// --------------------------------------------------------------------------
#define match_current(cstring)                                                 \
    are_cstrings_equal(cstring, as->parser->current_token->text,               \
                       as->parser->current_token->text_length)

void assemblar_init_tape(char *source_filepath, Tape *tape) {
    Assemblar *as = make_assemblar(source_filepath);
    Parser *parser = as->parser;
    TOKEN *next = parser_peek_next(parser);

    while (next && next->kind != TOKEN_END_OF_FILE) {
        parser_next_token(parser);

        if (parser->current_token->kind == TOKEN_IDENTIFIER) {
            next = parser_peek_next(parser);

            /* label */
            if (next && next->kind == TOKEN_COLON) {
                array_push(as->labels, *parser->current_token);
                parser_next_token(parser);
            } else {
                /* halt */
                if (match_current("hlt")) {
                    emit_byte(tape, 0x76);
                }

                /* start: Mov instruction */
                else if (match_current("mov")) {
                    parser_next_token(parser);
                    parser_expect_next(parser, TOKEN_IDENTIFIER);

                    if (match_current("a")) {
                        ins_mov_next_reg(tape, parser, 0x7f, 0x78, 0x79, 0x7a,
                                         0x7b, 0x7c, 0x7d, 0x7e);
                    } else if (match_current("b")) {

                        ins_mov_next_reg(tape, parser, 0x47, 0x40, 0x41, 0x42,
                                         0x43, 0x44, 0x45, 0x46);
                    } else if (match_current("c")) {
                        ins_mov_next_reg(tape, parser, 0x4f, 0x48, 0x49, 0x4a,
                                         0x4b, 0x4c, 0x4d, 0x4e);
                    } else if (match_current("d")) {
                        ins_mov_next_reg(tape, parser, 0x57, 0x50, 0x51, 0x52,
                                         0x53, 0x54, 0x55, 0x56);
                    } else if (match_current("e")) {
                        ins_mov_next_reg(tape, parser, 0x5f, 0x58, 0x59, 0x5a,
                                         0x5b, 0x5c, 0x5d, 0x5e);
                    } else if (match_current("h")) {
                        ins_mov_next_reg(tape, parser, 0x67, 0x60, 0x61, 0x62,
                                         0x63, 0x64, 0x65, 0x66);
                    } else if (match_current("l")) {
                        ins_mov_next_reg(tape, parser, 0x6f, 0x68, 0x69, 0x6a,
                                         0x6b, 0x6c, 0x6d, 0x6e);
                    } else if (match_current("m")) {
                        parser_expect_next(parser, TOKEN_COMMA);
                        parser_expect_next(parser, TOKEN_IDENTIFIER);

                        if (match_current("a"))
                            emit_byte(tape, 0x77);
                        else if (match_current("b"))
                            emit_byte(tape, 0x70);
                        else if (match_current("c"))
                            emit_byte(tape, 0x71);
                        else if (match_current("d"))
                            emit_byte(tape, 0x72);
                        else if (match_current("e"))
                            emit_byte(tape, 0x73);
                        else if (match_current("h"))
                            emit_byte(tape, 0x74);
                        else if (match_current("l"))
                            emit_byte(tape, 0x75);
                        else
                            parser_log_error_expected_register(parser,
                                                               a b c d e h l);
                    }
                }
                /* end: Mov instruction */

                /* start: instructions with single register operand */
                else if (match_current("add"))
                    ins_with_one_register(tape, parser, 0x87, 0x80, 0x81, 0x82,
                                          0x83, 0x84, 0x85, 0x86);
                else if (match_current("adc"))
                    ins_with_one_register(tape, parser, 0x8f, 0x88, 0x89, 0x8a,
                                          0x8b, 0x8c, 0x8d, 0x8e);
                else if (match_current("sub"))
                    ins_with_one_register(tape, parser, 0x87, 0x90, 0x91, 0x92,
                                          0x93, 0x94, 0x95, 0x96);
                else if (match_current("sbb"))
                    ins_with_one_register(tape, parser, 0x9f, 0x98, 0x99, 0x9a,
                                          0x9b, 0x9c, 0x9d, 0x9e);
                else if (match_current("ana"))
                    ins_with_one_register(tape, parser, 0xa7, 0xa0, 0xa1, 0xa2,
                                          0xa3, 0xa4, 0xa5, 0xa6);
                else if (match_current("xra"))
                    ins_with_one_register(tape, parser, 0xaf, 0xa8, 0xa9, 0xaa,
                                          0xab, 0xac, 0xad, 0xae);
                else if (match_current("ora"))
                    ins_with_one_register(tape, parser, 0xb7, 0xb0, 0xb1, 0xb2,
                                          0xb3, 0xb4, 0xb5, 0xb6);
                else if (match_current("cmp"))
                    ins_with_one_register(tape, parser, 0xbf, 0xb8, 0xb9, 0xba,
                                          0xbb, 0xbc, 0xbd, 0xbe);
                else if (match_current("inr"))
                    ins_with_one_register(tape, parser, 0x3c, 0x04, 0x0c, 0x14,
                                          0x1c, 0x24, 0x2c, 0x34);
                else if (match_current("dcr"))
                    ins_with_one_register(tape, parser, 0x3d, 0x05, 0x0d, 0x15,
                                          0x1d, 0x25, 0x2d, 0x35);
                /* end: instructions with single register operand */

                /* start: instruction with register pairs */
                else if (match_current("inx"))
                    ins_with_register_pair(tape, parser, 0x03, 0x13, 0x23,
                                           0x33);
                else if (match_current("dcx"))
                    ins_with_register_pair(tape, parser, 0x0b, 0x1b, 0x2b,
                                           0x3b);
                else if (match_current("dad"))
                    ins_with_register_pair(tape, parser, 0x09, 0x19, 0x29,
                                           0x39);
                /* end: instruction with register pairs */

                /* start: rotation instructions */
                else if (match_current("rlc"))
                    emit_byte(tape, 0x07);
                else if (match_current("rrc"))
                    emit_byte(tape, 0x0f);
                else if (match_current("ral"))
                    emit_byte(tape, 0x17);
                else if (match_current("rar"))
                    emit_byte(tape, 0x1f);
                /* end: rotation instructions */

                else if (match_current("cma")) /* cma instruction */
                    emit_byte(tape, 0x2f);

                /* start: carry instructions */
                else if (match_current("cmc"))
                    emit_byte(tape, 0x3f);
                else if (match_current("stc"))
                    emit_byte(tape, 0x37);
                /* end: carry instructions */

                /* start: return instructions */
                else if (match_current("ret"))
                    emit_byte(tape, 0xc9);
                else if (match_current("rz"))
                    emit_byte(tape, 0xc8);
                else if (match_current("rnz"))
                    emit_byte(tape, 0xc0);
                else if (match_current("rc"))
                    emit_byte(tape, 0xd8);
                else if (match_current("rnc"))
                    emit_byte(tape, 0xd0);
                else if (match_current("rpo"))
                    emit_byte(tape, 0xe0);
                else if (match_current("rpe"))
                    emit_byte(tape, 0xe8);
                /* end: return instructions */

                /* start: jump instructions */
                else if (match_current("jmp"))
                    ins_with_address(tape, parser, 0xc3);
                else if (match_current("jc"))
                    ins_with_address(tape, parser, 0xda);
                else if (match_current("jnc"))
                    ins_with_address(tape, parser, 0xd2);
                else if (match_current("jz"))
                    ins_with_address(tape, parser, 0xca);
                else if (match_current("jpo"))
                    ins_with_address(tape, parser, 0xe2);
                else if (match_current("jpe"))
                    ins_with_address(tape, parser, 0xea);
                else if (match_current("jp"))
                    ins_with_address(tape, parser, 0xf2);
                else if (match_current("jm"))
                    ins_with_address(tape, parser, 0xfa);
                /* end: jump instructions */

                /* start: call instructions */
                else if (match_current("call"))
                    ins_with_address(tape, parser, 0xcd);
                else if (match_current("cc"))
                    ins_with_address(tape, parser, 0xdc);
                else if (match_current("cnc"))
                    ins_with_address(tape, parser, 0xd4);
                else if (match_current("cz"))
                    ins_with_address(tape, parser, 0xcc);
                else if (match_current("cnz"))
                    ins_with_address(tape, parser, 0xc4);
                else if (match_current("cpo"))
                    ins_with_address(tape, parser, 0xe4);
                else if (match_current("cpe"))
                    ins_with_address(tape, parser, 0xec);
                else if (match_current("cp"))
                    ins_with_address(tape, parser, 0xf4);
                else if (match_current("cm"))
                    ins_with_address(tape, parser, 0xfc);
                /* end: call instructions */

                /* start: store/load instructions */
                else if (match_current("shld")) /* shld */
                    ins_with_address(tape, parser, 0x22);
                else if (match_current("lhld")) /* lhld */
                    ins_with_address(tape, parser, 0x28);
                else if (match_current("sta")) /* sta */
                    ins_with_address(tape, parser, 0x32);
                else if (match_current("lda")) /* lda */
                    ins_with_address(tape, parser, 0x3a);
                else if (match_current("ldax")) /* ldax */
                    ins_ldax(tape, parser, 0x0a, 0x1a);
                else if (match_current("lxi")) /* lxi */
                    ins_lxi(tape, parser, 0x01, 0x11, 0x21, 0x31);
                /* end: store/load instructions */

                /* start: 1 byte instructions with 8-bit immediate data */
                else if (match_current("adi")) /* adi */
                    ins_with_imm_byte(tape, parser, 0xc6);
                else if (match_current("aci")) /* aci */
                    ins_with_imm_byte(tape, parser, 0xce);
                else if (match_current("sui")) /* sui */
                    ins_with_imm_byte(tape, parser, 0xd6);
                else if (match_current("sbi")) /* sbi */
                    ins_with_imm_byte(tape, parser, 0xde);
                else if (match_current("ani")) /* ani */
                    ins_with_imm_byte(tape, parser, 0xe6);
                else if (match_current("xri")) /* xri */
                    ins_with_imm_byte(tape, parser, 0xee);
                else if (match_current("ori")) /* ori */
                    ins_with_imm_byte(tape, parser, 0xf6);
                else if (match_current("cpi")) /* cpi */
                    ins_with_imm_byte(tape, parser, 0xfe);
                else if (match_current("out")) /* out */
                    ins_with_imm_byte(tape, parser, 0x3a);
                else if (match_current("in")) /* in */
                    ins_with_imm_byte(tape, parser, 0x3a);
                /* end: 1 byte instructions with 8-bit immediate data */

                /* start: mvi instruction */
                else if (match_current("mvi")) {
                    parser_expect_next(parser, TOKEN_IDENTIFIER);

                    if (match_current("a"))
                        ins_mvi(tape, parser, 0x3e);
                    else if (match_current("b"))
                        ins_mvi(tape, parser, 0x06);
                    else if (match_current("c"))
                        ins_mvi(tape, parser, 0x0e);
                    else if (match_current("d"))
                        ins_mvi(tape, parser, 0x16);
                    else if (match_current("e"))
                        ins_mvi(tape, parser, 0x1e);
                    else if (match_current("h"))
                        ins_mvi(tape, parser, 0x26);
                    else if (match_current("l"))
                        ins_mvi(tape, parser, 0x2e);
                    else if (match_current("m"))
                        ins_mvi(tape, parser, 0x36);
                    else {
                        parser_log_error_expected_register(parser,
                                                           a b c d e h l m);
                    }
                }
                /* end: mvi instruction */

                /* start: stack instruction */
                else if (match_current("push"))
                    ins_stack_ops(tape, parser, 0xc5, 0xd5, 0xe5, 0xf5);
                else if (match_current("pop"))
                    ins_stack_ops(tape, parser, 0xc1, 0xd1, 0xe1, 0xf1);
                else if (match_current("pchl"))
                    emit_byte(tape, 0xe9);
                else if (match_current("xthl"))
                    emit_byte(tape, 0xe3);
                /* end: stack instruction */

                /* start: RST instructions */
                else if (match_current("rst"))
                    ins_rst(tape, parser, 0xc7, 0xcf, 0xd7, 0xdf, 0xe7, 0xef,
                            0xf7, 0xff);
                /* end: RST instructions */

                /* start: Interrupt Enable/Disable instructions */
                else if (match_current("ei"))
                    emit_byte(tape, 0xfb);
                else if (match_current("di"))
                    emit_byte(tape, 0xf3);
                /* end: Interrupt Enable/Disable instructions */

                /* Last error */
                else {
                    parser_log_error(parser, parser->current_token, "Error",
                                     "Unknown Instruction '%.*s' ",
                                     parser->current_token->text_length,
                                     parser->current_token->text);
                }
            }
        }

        next = parser_peek_next(parser);
    }

    tape->error_count = parser->error_count; /* set tape's error_count */
    free_assemblar(as);
}
