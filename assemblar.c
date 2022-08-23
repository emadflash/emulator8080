#include "assemblar.h"
#include "common.h"

#include <stdio.h>

// --------------------------------------------------------------------------
//                          - TOKEN_Kind -
// --------------------------------------------------------------------------
#define TOKEN_KINDS                                                            \
    TOKEN_KIND(TOKEN_NUM_DECIMAL, "DECIMAL_NUM")                               \
    TOKEN_KIND(TOKEN_NUM_BINARY, "BINARY_NUM")                                 \
    TOKEN_KIND(TOKEN_NUM_HEX, "HEX_NUM")                                       \
    TOKEN_KIND(TOKEN_NUM_OCTAL, "OCTAL_NUM")                                   \
    TOKEN_KIND(TOKEN_NUM_FLOAT, "FLOAT_NUM")                                   \
                                                                               \
    TOKEN_KIND(TOKEN_COMMA, "COMMA")                                           \
    TOKEN_KIND(TOKEN_COLON, "COLON")                                           \
    TOKEN_KIND(TOKEN_SEMICOLON, "SEMICOLON")                                   \
    TOKEN_KIND(TOKEN_IDENTIFIER, "IDENTIFIER")                                 \
                                                                               \
    TOKEN_KIND(TOKEN_END_OF_FILE, "END_OF_FILE")                               \
    TOKEN_KIND(TOKEN_Kind_COUNT, NULL)

#define is_token_number(kind)                                                  \
    (kind == TOKEN_NUM_BINARY || kind == TOKEN_NUM_DECIMAL ||                  \
     kind == TOKEN_NUM_OCTAL || kind == TOKEN_NUM_HEX ||                       \
     kind == TOKEN_NUM_FLOAT)

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
    TOKEN_Pos pos;
};

static TOKEN_Pos make_token_pos(size_t row, size_t col) {
    return (TOKEN_Pos){.row = row, .col = col};
}

static TOKEN make_token(char *text, size_t text_length, TOKEN_Kind kind,
                        TOKEN_Pos pos) {
    return (TOKEN){
        .text = text,
        .text_length = text_length,
        .kind = kind,
        .pos = pos,
    };
}

static f64 number_token_to_f64(TOKEN *token) {
    switch (token->kind) {
    case TOKEN_NUM_BINARY:
        return base2_to_f64(token->text + 2, token->text_length - 2);
    case TOKEN_NUM_DECIMAL:
        return base10_to_f64(token->text, token->text_length);
    case TOKEN_NUM_OCTAL:
        return base8_to_f64(token->text + 2, token->text_length - 2);
    case TOKEN_NUM_HEX:
        return base16_to_f64(token->text + 2, token->text_length - 2);

    default: unreachable();
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
    usize error_count;
};

static TOKEN_Pos current_pos(Lexer *l) {
    return (TOKEN_Pos){.row = l->row, .col = l->col};
}

static char nextchar(Lexer *l) {
    assert(l->curr + 1 < l->end);
    l->curr += 1;
    l->col += 1;
    l->current_char = *l->curr;
    return l->current_char;
}

static char *peeknext(Lexer *l) {
    if (l->curr + 1 >= l->end)
        return NULL;
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
    l->line_start = l->curr;
}

static int generic_number_scan(Lexer *l, bool (*is_vaild_digit)(int)) {
    char next;
    nextchar(l);

    if (!peeknext(l)) {
        return -1;
    }

    next = *peeknext(l);
    if (!is_vaild_digit(next)) {
        return -1;
    }

    while (peeknext(l)) {
        next = *peeknext(l);
        if (next == ' ' || next == '\n')
            break;
        if (!is_vaild_digit(next))
            return -1;
        nextchar(l);
    }

    return 0;
}

static TOKEN scan_num(Lexer *l) {
    TOKEN_Kind kind;
    TOKEN_Pos pos = current_pos(l);
    char *save = l->curr;

    if (l->current_char == '.') {
        kind = TOKEN_NUM_FLOAT;
        while (peeknext(l) && is_decimal_digit(*peeknext(l))) {
            nextchar(l);
        }
    } else {
        kind = TOKEN_NUM_DECIMAL;
        int err = 0;

        if (l->current_char == '0') {
            char *next = peeknext(l);
            if (next) {
                switch (*next) {
                case 'b':
                    kind = TOKEN_NUM_BINARY;
                    err = generic_number_scan(l, is_binary_digit);
                    break;
                case 'x':
                    kind = TOKEN_NUM_HEX;
                    err = generic_number_scan(l, is_hex_digit);
                    break;
                case 'o':
                    kind = TOKEN_NUM_OCTAL;
                    err = generic_number_scan(l, is_octal_digit);
                    break;
                }
            }
        }

        if (kind == TOKEN_NUM_DECIMAL) {
            while (peeknext(l) &&
                   (is_decimal_digit(*peeknext(l)) || *peeknext(l) == '.')) {
                if (*peeknext(l) == '.') {
                    kind = TOKEN_NUM_FLOAT;
                    nextchar(l);
                    while (peeknext(l) && is_decimal_digit(*peeknext(l))) {
                        nextchar(l);
                    }
                    break;
                }
                nextchar(l);
            }
        }
    }

    return make_token(save, l->curr - save + 1, kind, pos);
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
    return make_token(text, text_length, TOKEN_IDENTIFIER, pos);
}

static bool is_punctuation(int ch) {
    switch (ch) {
    case ',':
    case ';':
    case ':': return true;
    }
    return false;
}

static TOKEN scan_punctuation(Lexer *l) {
    TOKEN_Kind kind;
    char *save = l->curr;
    TOKEN_Pos pos = current_pos(l);

    switch (l->current_char) {
    case ',': kind = TOKEN_COMMA; break;

    case ':': kind = TOKEN_COLON; break;

    case ';': kind = TOKEN_SEMICOLON; break;

    default: unreachable();
    }
    return make_token(save, l->curr - save + 1, kind, pos);
}

static Lexer *make_lexer(char *src_cstr) {
    Lexer *l = xmalloc(sizeof(Lexer));
    l->exhausted = false;
    l->src = src_cstr;
    l->curr = l->src - 1;
    l->end = l->src + strlen(l->src);
    l->row = 0;
    l->col = -1;
    l->current_char = -1;
    l->line_start = l->src;
    l->error_count = 0;
    return l;
}

static TOKEN next_token(Lexer *l) {
    assert(!l->exhausted);
    skip_whitespaces(l);

    if (peeknext(l)) {
        nextchar(l);

        if (l->current_char == '\n') {
            newline(l);
            return next_token(l);
        } else if (is_num(l))
            return scan_num(l);
        else if (l->current_char == '_' || is_alphabet(l->current_char))
            return scan_iden(l);
        else if (is_punctuation(l->current_char))
            return scan_punctuation(l);
        else {
            printf("Error: Unknow character: %c\n", l->current_char);
        }
    }

    l->curr = l->end;
    l->col += 1;
    l->exhausted = true;
    return make_token(l->curr, 0, TOKEN_END_OF_FILE, current_pos(l));
}

static Array(TOKEN) slurp_tokens(Lexer *l) {
    TOKEN tok;
    Array(TOKEN) tokens;

    init_array(tokens);

    while (tok = next_token(l), tok.kind != TOKEN_END_OF_FILE) {
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
    Array(TOKEN) tokens;
    TOKEN *current_token;
    usize current_token_idx;
};

static void parser_next_token(Parser *p) {
    assert(p->current_token_idx + 1 < array_length(p->tokens));
    p->current_token_idx += 1;
    p->current_token = &p->tokens[p->current_token_idx];
}

static TOKEN *parser_peek_next(Parser *p) {
    if (p->current_token_idx + 1 >= array_length(p->tokens))
        return NULL;

    return &p->tokens[p->current_token_idx + 1];
}

static void parser_expect_next(Parser *p, TOKEN_Kind kind) {
    TOKEN *n = parser_peek_next(p);

    if (n && n->kind == kind)
        parser_next_token(p);
    else
        unreachable();
}

static Parser *make_parser(char *src) {
    Parser *parser = (Parser *)xmalloc(sizeof(Parser));
    Lexer *lexer = make_lexer(src);

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
//                          - Assemblar -
// --------------------------------------------------------------------------

Tape make_tape() {
    Tape tape;
    memset(tape.bytecode, 0x0, 64 * 1024);
    tape.bytecode_count = 0x0;
    tape.origin = 0x0;
    return tape;
}

static void emit_byte(Tape *tape, u8 byte) {
    tape->bytecode[tape->bytecode_count] = byte;
    tape->bytecode_count += 1;
}

Tape as_emit_from_source(char *src) {
#define match_current(cstring)                                                 \
    are_cstrings_equal(cstring, parser->current_token->text,                   \
                       parser->current_token->text_length)

    Tape tape;
    Parser *parser;
    TOKEN *next;
    Array(TOKEN) labels;

    tape = make_tape();
    parser = make_parser(src);
    next = parser_peek_next(parser);

    init_array(labels);

    while (next && next->kind != TOKEN_END_OF_FILE) {
        parser_next_token(parser);

        if (parser->current_token->kind == TOKEN_IDENTIFIER) {
            next = parser_peek_next(parser);

            /* label */
            if (next && next->kind == TOKEN_COLON) {
                array_push(labels, *parser->current_token);
                parser_next_token(parser);
            } else {
                /* Instructions */

                /* halt */
                if (match_current("hlt")) {
                    emit_byte(&tape, 0x76);
                }

                /* Mov instruction */
                if (match_current("mov")) {
                    parser_next_token(parser);

                    if (parser->current_token->kind != TOKEN_IDENTIFIER) {
                        unreachable(); // raise error
                    }

                    switch (parser->current_token->text[0]) {
#define ins_mov_next_reg(byteWithA, byteWithB, byteWithC, byteWithD,           \
                         byteWithE, byteWithH, byteWithL, byteWithM)           \
    {                                                                          \
        parser_expect_next(parser, TOKEN_COMMA);                               \
        parser_next_token(parser);                                             \
                                                                               \
        if (parser->current_token->kind != TOKEN_IDENTIFIER) {                 \
            unreachable();                                                     \
        }                                                                      \
                                                                               \
        switch (parser->current_token->text[0]) {                              \
        case 'a': emit_byte(&tape, byteWithA); break;                          \
        case 'b': emit_byte(&tape, byteWithB); break;                          \
        case 'c': emit_byte(&tape, byteWithC); break;                          \
        case 'd': emit_byte(&tape, byteWithD); break;                          \
        case 'e': emit_byte(&tape, byteWithE); break;                          \
        case 'h': emit_byte(&tape, byteWithH); break;                          \
        case 'l': emit_byte(&tape, byteWithL); break;                          \
        case 'm': emit_byte(&tape, byteWithM); break;                          \
                                                                               \
        default: unreachable();                                                \
        }                                                                      \
    }

                    case 'a':
                        ins_mov_next_reg(0x7f, 0x78, 0x79, 0x7a, 0x7b, 0x7c,
                                         0x7d, 0x7e);
                        break;

                    case 'b':
                        ins_mov_next_reg(0x47, 0x40, 0x41, 0x42, 0x43, 0x44,
                                         0x45, 0x46);
                        break;

                    case 'c':
                        ins_mov_next_reg(0x4f, 0x48, 0x49, 0x4a, 0x4b, 0x4c,
                                         0x4d, 0x4e);
                        break;

                    case 'd':
                        ins_mov_next_reg(0x57, 0x50, 0x51, 0x52, 0x53, 0x54,
                                         0x55, 0x56);
                        break;

                    case 'e':
                        ins_mov_next_reg(0x5f, 0x58, 0x59, 0x5a, 0x5b, 0x5c,
                                         0x5d, 0x5e);
                        break;

                    case 'h':
                        ins_mov_next_reg(0x67, 0x60, 0x61, 0x62, 0x63, 0x64,
                                         0x65, 0x66);
                        break;

                    case 'l':
                        ins_mov_next_reg(0x6f, 0x68, 0x69, 0x6a, 0x6b, 0x6c,
                                         0x6d, 0x6e);
                        break;

                    case 'm': {
                        parser_expect_next(parser, TOKEN_COMMA);
                        parser_next_token(parser);

                        if (parser->current_token->kind != TOKEN_IDENTIFIER) {
                            unreachable(); // raise error
                        }

                        switch (parser->current_token->text[0]) {
                        case 'a': emit_byte(&tape, 0x77); break;
                        case 'b': emit_byte(&tape, 0x70); break;
                        case 'c': emit_byte(&tape, 0x71); break;
                        case 'd': emit_byte(&tape, 0x72); break;
                        case 'e': emit_byte(&tape, 0x73); break;
                        case 'h': emit_byte(&tape, 0x74); break;
                        case 'l': emit_byte(&tape, 0x75); break;

                        default: unreachable();
                        }
                    }; break;

                    default: unreachable();
                    }
#undef ins_mov_next_reg
                }

#define ins_with_one_register(insName, byteWithA, byteWithB, byteWithC,        \
                              byteWithD, byteWithE, byteWithH, byteWithL,      \
                              byteWithM)                                       \
    if (are_cstrings_equal(insName, parser->current_token->text,               \
                           parser->current_token->text_length)) {              \
        parser_next_token(parser);                                             \
                                                                               \
        if (parser->current_token->kind != TOKEN_IDENTIFIER) {                 \
            unreachable();                                                     \
        }                                                                      \
                                                                               \
        switch (parser->current_token->text[0]) {                              \
        case 'a': emit_byte(&tape, byteWithA); break;                          \
        case 'b': emit_byte(&tape, byteWithB); break;                          \
        case 'c': emit_byte(&tape, byteWithC); break;                          \
        case 'd': emit_byte(&tape, byteWithD); break;                          \
        case 'e': emit_byte(&tape, byteWithE); break;                          \
        case 'h': emit_byte(&tape, byteWithH); break;                          \
        case 'l': emit_byte(&tape, byteWithL); break;                          \
        case 'm': emit_byte(&tape, byteWithM); break;                          \
                                                                               \
        default: unreachable();                                                \
        }                                                                      \
    }
                ins_with_one_register("add", 0x87, 0x80, 0x81, 0x82, 0x83, 0x84,
                                      0x85, 0x86);
                ins_with_one_register("adc", 0x8f, 0x88, 0x89, 0x8a, 0x8b, 0x8c,
                                      0x8d, 0x8e);

                ins_with_one_register("sub", 0x87, 0x90, 0x91, 0x92, 0x93, 0x94,
                                      0x95, 0x96);
                ins_with_one_register("sbb", 0x9f, 0x98, 0x99, 0x9a, 0x9b, 0x9c,
                                      0x9d, 0x9e);
                ins_with_one_register("ana", 0xa7, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4,
                                      0xa5, 0xa6);
                ins_with_one_register("xra", 0xaf, 0xa8, 0xa9, 0xaa, 0xab, 0xac,
                                      0xad, 0xae);
                ins_with_one_register("ora", 0xb7, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4,
                                      0xb5, 0xb6);
                ins_with_one_register("cmp", 0xbf, 0xb8, 0xb9, 0xba, 0xbb, 0xbc,
                                      0xbd, 0xbe);
#undef ins_with_one_register

                /* Increment instructions */
                if (match_current("inr")) {
                    parser_next_token(parser);

                    if (parser->current_token->kind != TOKEN_IDENTIFIER) {
                        unreachable();
                    }

                    switch (parser->current_token->text[0]) {
                    case 'b': emit_byte(&tape, 0x04); break; // INR B
                    case 'c': emit_byte(&tape, 0x0c); break; // INR C
                    case 'd': emit_byte(&tape, 0x14); break; // INR D
                    case 'e': emit_byte(&tape, 0x1c); break; // INR E
                    case 'h': emit_byte(&tape, 0x24); break; // INR H
                    case 'l': emit_byte(&tape, 0x2c); break; // INR L
                    case 'm': emit_byte(&tape, 0x34); break; // INR M
                    case 'a': emit_byte(&tape, 0x3c); break; // INR A

                    default: unreachable();
                    }
                }

                if (match_current("dcr")) {
                    parser_next_token(parser);

                    if (parser->current_token->kind != TOKEN_IDENTIFIER) {
                        unreachable();
                    }

                    switch (parser->current_token->text[0]) {
                    case 'b': emit_byte(&tape, 0x05); break; // DCR B
                    case 'c': emit_byte(&tape, 0x0d); break; // DCR C
                    case 'd': emit_byte(&tape, 0x15); break; // DCR D
                    case 'e': emit_byte(&tape, 0x1d); break; // DCR E
                    case 'h': emit_byte(&tape, 0x25); break; // DCR H
                    case 'l': emit_byte(&tape, 0x2d); break; // DCR L
                    case 'm': emit_byte(&tape, 0x35); break; // DCR M
                    case 'a': emit_byte(&tape, 0x3d); break; // DCR A

                    default: unreachable();
                    }
                }

                if (match_current("inx")) {
                    parser_next_token(parser);

                    if (parser->current_token->kind != TOKEN_IDENTIFIER) {
                        unreachable();
                    }

                    switch (parser->current_token->text[0]) {
                    case 'b': emit_byte(&tape, 0x03); break; // INX B
                    case 'd': emit_byte(&tape, 0x13); break; // INX D
                    case 'h': emit_byte(&tape, 0x23); break; // INX H
                    }

                    if (match_current("sp")) {
                        emit_byte(&tape, 0x33);
                    }

                    unreachable();
                }

                if (match_current("dcx")) {
                    parser_next_token(parser);

                    if (parser->current_token->kind != TOKEN_IDENTIFIER) {
                        unreachable();
                    }

                    switch (parser->current_token->text[0]) {
                    case 'b': emit_byte(&tape, 0x0b); break; // DCX B
                    case 'd': emit_byte(&tape, 0x1b); break; // DCX D
                    case 'h': emit_byte(&tape, 0x2b); break; // DCX H
                    }

                    if (match_current("sp")) {
                        emit_byte(&tape, 0x3b);
                    }

                    unreachable();
                }

                if (match_current("dad")) {
                    parser_next_token(parser);

                    if (parser->current_token->kind != TOKEN_IDENTIFIER) {
                        unreachable();
                    }

                    switch (parser->current_token->text[0]) {
                    case 'b': emit_byte(&tape, 0x09); break; // DAD B
                    case 'd': emit_byte(&tape, 0x19); break; // DAD D
                    case 'h': emit_byte(&tape, 0x29); break; // DAD H
                    }

                    if (match_current("sp")) {
                        emit_byte(&tape, 0x39);
                    }

                    unreachable();
                }

                /* rotation instructions */
                if (match_current("rlc"))
                    emit_byte(&tape, 0x07);
                if (match_current("rrc"))
                    emit_byte(&tape, 0x0f);
                if (match_current("ral"))
                    emit_byte(&tape, 0x17);
                if (match_current("rar"))
                    emit_byte(&tape, 0x1f);

                if (match_current("cma"))
                    emit_byte(&tape, 0x2f);
                if (match_current("cmc"))
                    emit_byte(&tape, 0x3f);
                if (match_current("stc"))
                    emit_byte(&tape, 0x37);

                /* return instructions */
                if (match_current("ret"))
                    emit_byte(&tape, 0xc9);
                if (match_current("rz"))
                    emit_byte(&tape, 0xc8);
                if (match_current("rnz"))
                    emit_byte(&tape, 0xc0);
                if (match_current("rc"))
                    emit_byte(&tape, 0xd8);
                if (match_current("rnc"))
                    emit_byte(&tape, 0xd0);
                if (match_current("rpo"))
                    emit_byte(&tape, 0xe0);
                if (match_current("rpe"))
                    emit_byte(&tape, 0xe8);

                    /* Jump */
#define ins_jump_variants(variant, byte)                                       \
    if (match_current(variant)) {                                              \
        parser_next_token(parser);                                             \
        emit_byte(&tape, byte);                                                \
                                                                               \
        if (!is_token_number(parser->current_token->kind)) {                   \
            unreachable();                                                     \
        }                                                                      \
                                                                               \
        f64 num = number_token_to_f64(parser->current_token);                  \
        emit_byte(&tape, cast(u8)((uint)num & 0xff));                          \
        emit_byte(&tape, cast(u8)(((uint)num >> 8) & 0xff));                   \
    }
                ins_jump_variants("jmp", 0xc3);
                ins_jump_variants("jc", 0xda);
                ins_jump_variants("jnc", 0xd2);
                ins_jump_variants("jz", 0xca);
                ins_jump_variants("jpo", 0xe2);
                ins_jump_variants("jpe", 0xea);
                ins_jump_variants("jp", 0xf2);
                ins_jump_variants("jm", 0xfa);

#define ins_call_variants(variant, byte) ins_jump_variants(variant, byte)

                ins_call_variants("call", 0xcd);
                ins_call_variants("cc", 0xdc);
                ins_call_variants("cnc", 0xd4);
                ins_call_variants("cz", 0xcc);
                ins_call_variants("cnz", 0xc4);
                ins_call_variants("cpo", 0xe4);
                ins_call_variants("cpe", 0xec);
                ins_call_variants("cp", 0xf4);
                ins_call_variants("cm", 0xfc);

                /* shld */
                if (match_current("shld")) {
                    unreachable();
                }
            }
        }

        next = parser_peek_next(parser);
    }

    free_array(labels);
    free_parser(parser);
    return tape;
}