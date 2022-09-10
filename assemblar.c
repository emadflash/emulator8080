#define CLI_MORE_INFO
#define CLI_AUTHOR_NAME "madflash"
#define CLI_AUTHOR_EMAIL_ADDRESS "backbugkaught@gmail.com"
#define CLI_PROGRAM_DESC "intel 8080 assemblar"
#define CLI_VERSION "0.0.1"

#include "basic.c"
#include "cli.c"

// --------------------------------------------------------------------------
//                          - tokens -
// --------------------------------------------------------------------------
#define TOKEN_KINDS                                                                                \
    TOKEN_KIND(TOKEN_FAULTY, "Faulty token!!!")                                                    \
                                                                                                   \
    TOKEN_TEMP(TOKEN_CONST_NUMBER__BEGIN, "number constant begin")                                 \
    TOKEN_KIND(TOKEN_CONST_NUMBER_DECIMAL, "decimal number constant")                              \
    TOKEN_KIND(TOKEN_CONST_NUMBER_HEX, "hex number constant")                                      \
    TOKEN_KIND(TOKEN_CONST_NUMBER_OCTAL, "octal number constant")                                  \
    TOKEN_KIND(TOKEN_CONST_NUMBER_BINARY, "binary number constant")                                \
    TOKEN_KIND(TOKEN_CONST_NUMBER_FLOAT, "float number constant")                                  \
    TOKEN_TEMP(TOKEN_CONST_NUMBER__END, "number constant end")                                     \
                                                                                                   \
    TOKEN_KIND(TOKEN_CONST_CHAR, "character constant")                                             \
    TOKEN_KIND(TOKEN_CONST_STRING, "string constant")                                              \
    TOKEN_KIND(TOKEN_COMMA, "Comma")                                                               \
    TOKEN_KIND(TOKEN_COLON, "Colon")                                                               \
    TOKEN_KIND(TOKEN_SEMICOLON, "Semicolon")                                                       \
    TOKEN_KIND(TOKEN_DOLLAR, "Dollar($) sign")                                                     \
    TOKEN_KIND(TOKEN_LEFT_PAREN, "Left paren")                                                     \
    TOKEN_KIND(TOKEN_RIGHT_PAREN, "Right paren")                                                   \
    TOKEN_KIND(TOKEN_PLUS, "Plus sign")                                                            \
    TOKEN_KIND(TOKEN_MINUS, "Minus sign")                                                          \
    TOKEN_KIND(TOKEN_MUL, "Multiply sign")                                                         \
    TOKEN_KIND(TOKEN_DIVIDE, "Divide sign")                                                        \
    TOKEN_KIND(TOKEN_BITWISE_AND, "Bitwise and")                                                   \
    TOKEN_KIND(TOKEN_BITWISE_XOR, "Bitwise xor")                                                   \
    TOKEN_KIND(TOKEN_BITWISE_OR, "Bitwise or")                                                     \
    TOKEN_KIND(TOKEN_BANG, "Not(!)")                                                               \
    TOKEN_KIND(TOKEN_TILDA, "Tilda(~)")                                                            \
                                                                                                   \
    TOKEN_KIND(TOKEN_IDENTIFIER, "Identifier")                                                     \
    TOKEN_KIND(TOKEN_KW_SET, "Set")                                                                \
    TOKEN_KIND(TOKEN_KW_EQU, "Equ")                                                                \
                                                                                                   \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG__BEGIN, "General purpose register begin")                      \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG_A, "A")                                                        \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG_B, "B")                                                        \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG_C, "C")                                                        \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG_D, "D")                                                        \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG_E, "E")                                                        \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG_H, "H")                                                        \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG_L, "L")                                                        \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG_M, "M")                                                        \
    TOKEN_KIND(TOKEN_KW_GENERAL_REG__END, "General purpose register end")                          \
                                                                                                   \
    TOKEN_KIND(TOKEN_KW_SPECIAL_REG__BEGIN, "Special register begin")                              \
    TOKEN_KIND(TOKEN_KW_SPECIAL_REG_SP, "SP")                                                      \
    TOKEN_KIND(TOKEN_KW_SPECIAL_REG_PSW, "PSW")                                                    \
    TOKEN_KIND(TOKEN_KW_SPECIAL_REG__END, "Special register end")                                  \
                                                                                                   \
    TOKEN_KIND(TOKEN_KW_HLT, "Hlt")                                                                \
    TOKEN_KIND(TOKEN_KW_MOV, "Mov")                                                                \
    TOKEN_KIND(TOKEN_KW_ADD, "Add")                                                                \
    TOKEN_KIND(TOKEN_KW_ADC, "Adc")                                                                \
    TOKEN_KIND(TOKEN_KW_SUB, "Sub")                                                                \
    TOKEN_KIND(TOKEN_KW_SBB, "Sbb")                                                                \
    TOKEN_KIND(TOKEN_KW_ANA, "Ana")                                                                \
    TOKEN_KIND(TOKEN_KW_XRA, "Xra")                                                                \
    TOKEN_KIND(TOKEN_KW_ORA, "Ora")                                                                \
    TOKEN_KIND(TOKEN_KW_CMP, "Cmp")                                                                \
    TOKEN_KIND(TOKEN_KW_INR, "Inr")                                                                \
    TOKEN_KIND(TOKEN_KW_DCR, "Dcr")                                                                \
    TOKEN_KIND(TOKEN_KW_INX, "Inx")                                                                \
    TOKEN_KIND(TOKEN_KW_DCX, "Dcx")                                                                \
    TOKEN_KIND(TOKEN_KW_RLC, "Rlc")                                                                \
    TOKEN_KIND(TOKEN_KW_RRC, "Rrc")                                                                \
    TOKEN_KIND(TOKEN_KW_RAL, "Ral")                                                                \
    TOKEN_KIND(TOKEN_KW_RAR, "Rar")                                                                \
    TOKEN_KIND(TOKEN_KW_CMA, "Cma")                                                                \
    TOKEN_KIND(TOKEN_KW_CMC, "Cmc")                                                                \
    TOKEN_KIND(TOKEN_KW_STC, "Stc")                                                                \
    TOKEN_KIND(TOKEN_KW_RET, "Ret")                                                                \
    TOKEN_KIND(TOKEN_KW_RZ, "Rz")                                                                  \
    TOKEN_KIND(TOKEN_KW_RNZ, "Rnz")                                                                \
    TOKEN_KIND(TOKEN_KW_RC, "Rc")                                                                  \
    TOKEN_KIND(TOKEN_KW_RNC, "Rnc")                                                                \
    TOKEN_KIND(TOKEN_KW_RPO, "Rpo")                                                                \
    TOKEN_KIND(TOKEN_KW_RPE, "Rpe")                                                                \
    TOKEN_KIND(TOKEN_KW_RP, "Rp")                                                                  \
    TOKEN_KIND(TOKEN_KW_RM, "Rm")                                                                  \
    TOKEN_KIND(TOKEN_KW_JMP, "Jmp")                                                                \
    TOKEN_KIND(TOKEN_KW_JC, "Jc")                                                                  \
    TOKEN_KIND(TOKEN_KW_JNC, "Jnc")                                                                \
    TOKEN_KIND(TOKEN_KW_JZ, "Jz")                                                                  \
    TOKEN_KIND(TOKEN_KW_JNZ, "Jnz")                                                                \
    TOKEN_KIND(TOKEN_KW_JPO, "Jpo")                                                                \
    TOKEN_KIND(TOKEN_KW_JPE, "Jpe")                                                                \
    TOKEN_KIND(TOKEN_KW_JP, "Jp")                                                                  \
    TOKEN_KIND(TOKEN_KW_JM, "Jm")                                                                  \
    TOKEN_KIND(TOKEN_KW_CALL, "Call")                                                              \
    TOKEN_KIND(TOKEN_KW_CC, "Cc")                                                                  \
    TOKEN_KIND(TOKEN_KW_CNC, "Cnc")                                                                \
    TOKEN_KIND(TOKEN_KW_CZ, "Cz")                                                                  \
    TOKEN_KIND(TOKEN_KW_CNZ, "Cnz")                                                                \
    TOKEN_KIND(TOKEN_KW_CPO, "Cpo")                                                                \
    TOKEN_KIND(TOKEN_KW_CPE, "Cpe")                                                                \
    TOKEN_KIND(TOKEN_KW_CP, "Cp")                                                                  \
    TOKEN_KIND(TOKEN_KW_CM, "Cm")                                                                  \
    TOKEN_KIND(TOKEN_KW_SHLD, "Shld")                                                              \
    TOKEN_KIND(TOKEN_KW_LHLD, "Lhld")                                                              \
    TOKEN_KIND(TOKEN_KW_STA, "Sta")                                                                \
    TOKEN_KIND(TOKEN_KW_LDA, "Lda")                                                                \
    TOKEN_KIND(TOKEN_KW_LDAX, "Ldax")                                                              \
    TOKEN_KIND(TOKEN_KW_STAX, "Stax")                                                              \
    TOKEN_KIND(TOKEN_KW_LXI, "Lxi")                                                                \
    TOKEN_KIND(TOKEN_KW_ADI, "Adi")                                                                \
    TOKEN_KIND(TOKEN_KW_ACI, "Aci")                                                                \
    TOKEN_KIND(TOKEN_KW_SUI, "Sui")                                                                \
    TOKEN_KIND(TOKEN_KW_SBI, "Sbi")                                                                \
    TOKEN_KIND(TOKEN_KW_ANI, "Ani")                                                                \
    TOKEN_KIND(TOKEN_KW_XRI, "Xri")                                                                \
    TOKEN_KIND(TOKEN_KW_ORI, "Ori")                                                                \
    TOKEN_KIND(TOKEN_KW_CPI, "Cpi")                                                                \
    TOKEN_KIND(TOKEN_KW_OUT, "Out")                                                                \
    TOKEN_KIND(TOKEN_KW_IN, "In")                                                                  \
    TOKEN_KIND(TOKEN_KW_MVI, "Mvi")                                                                \
    TOKEN_KIND(TOKEN_KW_PUSH, "Push")                                                              \
    TOKEN_KIND(TOKEN_KW_POP, "Pop")                                                                \
    TOKEN_KIND(TOKEN_KW_PCHL, "Pchl")                                                              \
    TOKEN_KIND(TOKEN_KW_XTHL, "Xthl")                                                              \
    TOKEN_KIND(TOKEN_KW_RST, "Rst")                                                                \
    TOKEN_KIND(TOKEN_KW_EI, "Ei")                                                                  \
    TOKEN_KIND(TOKEN_KW_DI, "Di")                                                                  \
    TOKEN_KIND(TOKEN_KW_DAD, "Dad")                                                                \
                                                                                                   \
    TOKEN_KIND(TOKEN_END_OF_FILE, "End of file")                                                   \
    TOKEN_KIND(TOKEN_Kind_COUNT, NULL)

typedef enum {
#define TOKEN_KIND(kind_name, ...) kind_name,
#define TOKEN_TEMP TOKEN_KIND
    TOKEN_KINDS
#undef TOKEN_TEMP
#undef TOKEN_KIND
} Token_Kind;

static char const *token_kind_to_cstring[] = {
#define TOKEN_KIND(kind_name, cstring) [kind_name] = cstring,
#define TOKEN_TEMP(kind_name, cstring) [kind_name] = cstring,
    TOKEN_KINDS
#undef TOKEN_TEMP
#undef TOKEN_KIND
};

typedef struct Token_Pos Token_Pos;
struct Token_Pos {
    usize row, col;
};

typedef struct Token Token;
struct Token {
    char *text;
    usize text_length;
    Token_Kind kind;
    char *line_start;
    Token_Pos pos;
};

static Token make_token(char *text, size_t text_length, Token_Kind kind, Token_Pos pos,
                        char *line_start) {
    return (Token){
        .text = text,
        .text_length = text_length,
        .kind = kind,
        .pos = pos,
        .line_start = line_start,
    };
}

bool is_token_number(Token *token) {
    return (token->kind >= TOKEN_CONST_NUMBER__BEGIN && token->kind <= TOKEN_CONST_NUMBER__END);
}

f64 base2_to_f6s4(char *str, usize length) {
    f64 result = 0;

    for (usize i = length - 1; i > 0; --i, ++str) {
        result += pow(2, i) * binary_digit_to_int(*str);
    }

    result += pow(2, 0) * decimal_digit_to_int(*str);
    return result;
}

static i64 number_token_to_i64(Token *token) {
    Debug_Assert(is_token_number(token));

    char *curr;
    u8 digit;

    int base = 0;
    u8 mins_prefix = 2;
    i64 result = 0;

    switch (token->kind) {
    case TOKEN_CONST_NUMBER_BINARY: base = 2; break;

    case TOKEN_CONST_NUMBER_OCTAL: base = 8; break;

    case TOKEN_CONST_NUMBER_DECIMAL:
        base = 10;
        mins_prefix = 0;
        break;

    case TOKEN_CONST_NUMBER_HEX: base = 16; break;

    default: Unreachable();
    }

    i64 base_raise_to_power = 1;
    curr = token->text + token->text_length - 1;

    for (usize i = 0; i < token->text_length - mins_prefix; ++i) {
        digit = cast(u8) to_lowercase(*curr--);
        if (digit >= 'a' && digit <= 'f')
            digit = digit - 'a' + 10;
        else
            digit = digit - '0';

        result += base_raise_to_power * digit;
        base_raise_to_power *= base;
    }

    return result;
}

void print_token(Token *token) {
    printf("%s, ", token_kind_to_cstring[token->kind]);
    printf("text: %.*s, ", (int)token->text_length, token->text);
    printf("position: %zu:%zu", token->pos.row, token->pos.col);
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

static Token_Pos current_pos(Lexer *l) {
    return (Token_Pos){.row = l->row, .col = l->col};
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

static Token scan_num(Lexer *l) {
    Token_Kind kind;
    Token_Pos pos = current_pos(l);
    char *save = l->curr;

    if (l->current_char == '.') {
        kind = TOKEN_CONST_NUMBER_FLOAT;
        while (peeknext(l) && is_decimal_digit(*peeknext(l))) {
            nextchar(l);
        }
    } else {
        kind = TOKEN_CONST_NUMBER_DECIMAL;

        if (l->current_char == '0') {
            char *next = peeknext(l);
            if (next) {
                switch (*next) {
                case 'b':
                    kind = TOKEN_CONST_NUMBER_BINARY;
                    generic_number_scan(l, is_binary_digit);
                    break;
                case 'x':
                    kind = TOKEN_CONST_NUMBER_HEX;
                    generic_number_scan(l, is_hex_digit);
                    break;
                case 'o':
                    kind = TOKEN_CONST_NUMBER_OCTAL;
                    generic_number_scan(l, is_octal_digit);
                    break;
                }
            }
        }

        if (kind == TOKEN_CONST_NUMBER_DECIMAL) {
            while (peeknext(l) && (is_decimal_digit(*peeknext(l)) || *peeknext(l) == '.')) {
                if (*peeknext(l) == '.') {
                    kind = TOKEN_CONST_NUMBER_FLOAT;
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

bool lexer_is_keyword_instruction(char *text, usize text_length, char *kw_ins_lowercase,
                                  usize kw_ins_length) {
    if (text_length != kw_ins_length) return false;
    while (text_length-- > 0) {
        if (to_lowercase(*text) != *kw_ins_lowercase) return false;
        text++;
        kw_ins_lowercase++;
    }
    return true;
}

static Token scan_iden(Lexer *l) {
    size_t text_length;
    char *text = l->curr;
    Token_Pos pos = current_pos(l);

    while (peeknext(l) && (*peeknext(l) == '_' || is_alphanumeric(*peeknext(l)))) {
        nextchar(l);
    }

    text_length = l->curr - text + 1;
    if (text_length > 4) return make_token(text, text_length, TOKEN_IDENTIFIER, pos, l->line_start);

#define if_kw_instruction_then_return(kw, kw_cstring_lowercase, kw_cstring_lowercase_length)       \
    if (lexer_is_keyword_instruction(text, text_length, kw_cstring_lowercase,                      \
                                     kw_cstring_lowercase_length)) {                               \
        return make_token(text, text_length, kw, pos, l->line_start);                              \
    }
#define if_kw_single_char_then_return(kw, kw_char)                                                 \
    if (to_lowercase(*text) == kw_char) {                                                          \
        return make_token(text, 1, kw, pos, l->line_start);                                        \
    }

    switch (text_length) {
    case 1:
        if_kw_single_char_then_return(TOKEN_KW_GENERAL_REG_A, 'a');
        if_kw_single_char_then_return(TOKEN_KW_GENERAL_REG_B, 'b');
        if_kw_single_char_then_return(TOKEN_KW_GENERAL_REG_C, 'c');
        if_kw_single_char_then_return(TOKEN_KW_GENERAL_REG_D, 'd');
        if_kw_single_char_then_return(TOKEN_KW_GENERAL_REG_E, 'e');
        if_kw_single_char_then_return(TOKEN_KW_GENERAL_REG_H, 'h');
        if_kw_single_char_then_return(TOKEN_KW_GENERAL_REG_L, 'l');
        if_kw_single_char_then_return(TOKEN_KW_GENERAL_REG_M, 'm');
        break;

    case 2:
        if_kw_instruction_then_return(TOKEN_KW_EI, "ei", 2);
        if_kw_instruction_then_return(TOKEN_KW_DI, "di", 2);
        if_kw_instruction_then_return(TOKEN_KW_IN, "in", 2);
        if_kw_instruction_then_return(TOKEN_KW_RZ, "rz", 2);
        if_kw_instruction_then_return(TOKEN_KW_RC, "rc", 2);
        if_kw_instruction_then_return(TOKEN_KW_RP, "rp", 2);
        if_kw_instruction_then_return(TOKEN_KW_RM, "rm", 2);
        if_kw_instruction_then_return(TOKEN_KW_JZ, "jz", 2);
        if_kw_instruction_then_return(TOKEN_KW_JC, "jc", 2);
        if_kw_instruction_then_return(TOKEN_KW_JP, "jp", 2);
        if_kw_instruction_then_return(TOKEN_KW_JM, "jm", 2);
        if_kw_instruction_then_return(TOKEN_KW_CC, "cc", 2);
        if_kw_instruction_then_return(TOKEN_KW_CZ, "cz", 2);
        if_kw_instruction_then_return(TOKEN_KW_CZ, "cz", 2);
        if_kw_instruction_then_return(TOKEN_KW_CP, "cp", 2);
        if_kw_instruction_then_return(TOKEN_KW_CM, "cm", 2);
        if_kw_instruction_then_return(TOKEN_KW_SPECIAL_REG_SP, "sp", 2);
        break;

    case 3:
        if_kw_instruction_then_return(TOKEN_KW_OUT, "out", 3);
        if_kw_instruction_then_return(TOKEN_KW_HLT, "hlt", 3);
        if_kw_instruction_then_return(TOKEN_KW_MOV, "mov", 3);
        if_kw_instruction_then_return(TOKEN_KW_ADD, "add", 3);
        if_kw_instruction_then_return(TOKEN_KW_ADC, "adc", 3);
        if_kw_instruction_then_return(TOKEN_KW_SUB, "sub", 3);
        if_kw_instruction_then_return(TOKEN_KW_SBB, "sbb", 3);
        if_kw_instruction_then_return(TOKEN_KW_ANA, "ana", 3);
        if_kw_instruction_then_return(TOKEN_KW_XRA, "xra", 3);
        if_kw_instruction_then_return(TOKEN_KW_ORA, "ora", 3);
        if_kw_instruction_then_return(TOKEN_KW_CMP, "cmp", 3);
        if_kw_instruction_then_return(TOKEN_KW_INR, "inr", 3);
        if_kw_instruction_then_return(TOKEN_KW_DCR, "dcr", 3);
        if_kw_instruction_then_return(TOKEN_KW_INX, "inx", 3);
        if_kw_instruction_then_return(TOKEN_KW_DCX, "dcx", 3);
        if_kw_instruction_then_return(TOKEN_KW_RLC, "rlc", 3);
        if_kw_instruction_then_return(TOKEN_KW_RRC, "rrc", 3);
        if_kw_instruction_then_return(TOKEN_KW_RAL, "ral", 3);
        if_kw_instruction_then_return(TOKEN_KW_RAR, "rar", 3);
        if_kw_instruction_then_return(TOKEN_KW_CMA, "cma", 3);
        if_kw_instruction_then_return(TOKEN_KW_CMC, "cmc", 3);
        if_kw_instruction_then_return(TOKEN_KW_STC, "stc", 3);
        if_kw_instruction_then_return(TOKEN_KW_RET, "ret", 3);
        if_kw_instruction_then_return(TOKEN_KW_RNC, "rnc", 3);
        if_kw_instruction_then_return(TOKEN_KW_RNZ, "rnz", 3);
        if_kw_instruction_then_return(TOKEN_KW_RPO, "rpo", 3);
        if_kw_instruction_then_return(TOKEN_KW_RPE, "rpe", 3);
        if_kw_instruction_then_return(TOKEN_KW_JMP, "jmp", 3);
        if_kw_instruction_then_return(TOKEN_KW_JNC, "jnc", 3);
        if_kw_instruction_then_return(TOKEN_KW_JNZ, "jnz", 3);
        if_kw_instruction_then_return(TOKEN_KW_JPO, "jpo", 3);
        if_kw_instruction_then_return(TOKEN_KW_JPE, "jpe", 3);
        if_kw_instruction_then_return(TOKEN_KW_CNC, "cnc", 3);
        if_kw_instruction_then_return(TOKEN_KW_CNZ, "cnz", 3);
        if_kw_instruction_then_return(TOKEN_KW_CPO, "cpo", 3);
        if_kw_instruction_then_return(TOKEN_KW_CPE, "cpe", 3);
        if_kw_instruction_then_return(TOKEN_KW_STA, "sta", 3);
        if_kw_instruction_then_return(TOKEN_KW_LDA, "lda", 3);
        if_kw_instruction_then_return(TOKEN_KW_ADI, "adi", 3);
        if_kw_instruction_then_return(TOKEN_KW_ACI, "aci", 3);
        if_kw_instruction_then_return(TOKEN_KW_SUI, "sui", 3);
        if_kw_instruction_then_return(TOKEN_KW_SBI, "sbi", 3);
        if_kw_instruction_then_return(TOKEN_KW_ANI, "ani", 3);
        if_kw_instruction_then_return(TOKEN_KW_XRI, "xri", 3);
        if_kw_instruction_then_return(TOKEN_KW_ORI, "ori", 3);
        if_kw_instruction_then_return(TOKEN_KW_CPI, "cpi", 3);
        if_kw_instruction_then_return(TOKEN_KW_MVI, "mvi", 3);
        if_kw_instruction_then_return(TOKEN_KW_POP, "pop", 3);
        if_kw_instruction_then_return(TOKEN_KW_RST, "rst", 3);
        if_kw_instruction_then_return(TOKEN_KW_DAD, "dad", 3);
        if_kw_instruction_then_return(TOKEN_KW_LXI, "lxi", 3);

        if_kw_instruction_then_return(TOKEN_KW_SPECIAL_REG_PSW, "psw", 3);
        break;

    case 4:
        if_kw_instruction_then_return(TOKEN_KW_CALL, "call", 4);
        if_kw_instruction_then_return(TOKEN_KW_SHLD, "shld", 4);
        if_kw_instruction_then_return(TOKEN_KW_LHLD, "lhld", 4);
        if_kw_instruction_then_return(TOKEN_KW_LDAX, "ldax", 4);
        if_kw_instruction_then_return(TOKEN_KW_STAX, "stax", 4);
        if_kw_instruction_then_return(TOKEN_KW_PUSH, "push", 4);
        if_kw_instruction_then_return(TOKEN_KW_PCHL, "pchl", 4);
        if_kw_instruction_then_return(TOKEN_KW_XTHL, "xthl", 4);
        break;
    }
#undef if_keyword_return

    return make_token(text, text_length, TOKEN_IDENTIFIER, pos, l->line_start);
}

static Token scan_punctuation(Lexer *l) {
    Token_Kind kind;
    char *save = l->curr;
    Token_Pos pos = current_pos(l);

    switch (l->current_char) {
    case ',': kind = TOKEN_COMMA; break;
    case ':': kind = TOKEN_COLON; break;
    case ';': kind = TOKEN_SEMICOLON; break;
    case '$': kind = TOKEN_DOLLAR; break;
    case '(': kind = TOKEN_LEFT_PAREN; break;
    case ')': kind = TOKEN_RIGHT_PAREN; break;
    case '+': kind = TOKEN_PLUS; break;
    case '-': kind = TOKEN_MINUS; break;
    case '*': kind = TOKEN_MUL; break;
    case '/': kind = TOKEN_DIVIDE; break;
    case '!': kind = TOKEN_BANG; break;
    case '~': kind = TOKEN_TILDA; break;

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

static Token lexer_next_token(Lexer *l) {
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

static Array(Token) slurp_tokens(Lexer *l) {
    Token tok;
    Array(Token) tokens;

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

    Array(Token) tokens;
    Token *current_token;
    usize current_token_idx;

    int error_count;
};

static void parser_next_token(Parser *p) {
    assert(p->current_token_idx + 1 < array_length(p->tokens));
    p->current_token_idx += 1;
    p->current_token = &p->tokens[p->current_token_idx];
}

static Token *parser_peek_next(Parser *p) {
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
static void parser_log_error(Parser *p, Token *faulty_token, char *prefix, char *msg, ...) {
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

static void parser_expect_next(Parser *p, Token_Kind kind) {
    Token *n = parser_peek_next(p);
    Assert(n);

    if (n && n->kind == kind) {
        parser_next_token(p);
    } else {
        parser_log_error(p, n, "Syntax Error", "Expected %s kind, got %s",
                         token_kind_to_cstring[kind], token_kind_to_cstring[n->kind]);
        parser_next_token(p);
    }
}

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
//                          - Expression -
// --------------------------------------------------------------------------
#define EXPR_KINDS                                                                                 \
    EXPR_KIND(Invaild, "Invaild Expr", bool)                                                       \
    EXPR_KIND(                                                                                     \
        NumberLiteral, "Number Literal expr", struct {                                             \
            Token token;                                                                           \
            u16 number;                                                                            \
        })                                                                                         \
    EXPR_KIND(                                                                                     \
        Identifier, "Identifier expr", struct { Token token; })                                    \
    EXPR_KIND(                                                                                     \
        LocationCounter, "Location Counter($)", struct {                                           \
            Token token;                                                                           \
            u16 location;                                                                          \
        })                                                                                         \
    EXPR_KIND(                                                                                     \
        UnaryOp, "Unary operation", struct {                                                       \
            Token op_tok;                                                                          \
            UnaryOp_Kind op_kind;                                                                  \
            Expression *expr;                                                                      \
        })                                                                                         \
    EXPR_KIND(                                                                                     \
        BinaryOp, "Binary operation", struct {                                                     \
            Token op_tok;                                                                          \
            BinaryOp_Kind op_kind;                                                                 \
            Expression *left, *right;                                                              \
        })

typedef enum {
    UnaryOp_Plus,
    UnaryOp_Minus,
    UnaryOp_BitwiseNot,
} UnaryOp_Kind;

typedef enum {
    BinaryOp_Plus,
    BinaryOp_Minus,
    BinaryOp_Mul,
    BinaryOp_Divide,
    BinaryOp_BitwiseXor,
    BinaryOp_BitwiseOr,
    BinaryOp_BitwiseAnd,
} BinaryOp_Kind;

typedef enum {
#define EXPR_KIND(kind_name, ...) ExpressionKind_##kind_name,
    EXPR_KINDS
#undef EXPR_KIND
} ExpressionKind;

typedef struct Expression Expression;
struct Expression {
    ExpressionKind kind;
    union {
#define EXPR_KIND(kind_name, desc, ...) __VA_ARGS__ kind_name;
        EXPR_KINDS
#undef EXPR_KIND
    } type;
};

void free_expression(Expression *expr) {
    switch (expr->kind) {
    case ExpressionKind_NumberLiteral:
    case ExpressionKind_Identifier:
    case ExpressionKind_LocationCounter: break;

    case ExpressionKind_UnaryOp:
        if (expr->type.UnaryOp.expr) free_expression(expr->type.UnaryOp.expr);
        break;

    case ExpressionKind_BinaryOp:
        if (expr->type.BinaryOp.left) free_expression(expr->type.BinaryOp.left);
        if (expr->type.BinaryOp.right) free_expression(expr->type.BinaryOp.right);
        break;

    default: Unreachable();
    }

    free(expr);
}

Expression *Expr_NumberLiteral(Token token, u16 number) {
    Expression *expr = xmalloc(sizeof(Expression));
    expr->kind = ExpressionKind_NumberLiteral;
    expr->type.NumberLiteral.token = token;
    expr->type.NumberLiteral.number = number;
    return expr;
}

Expression *Expr_Identifier(Token token) {
    Expression *expr = xmalloc(sizeof(Expression));
    expr->kind = ExpressionKind_Identifier;
    expr->type.Identifier.token = token;
    return expr;
}

Expression *Expr_LocationCounter(Token token, u16 location) {
    Expression *expr = xmalloc(sizeof(Expression));
    expr->kind = ExpressionKind_LocationCounter;
    expr->type.LocationCounter.token = token;
    expr->type.LocationCounter.location = location;
    return expr;
}

Expression *Expr_UnaryOp(Token op_tok, UnaryOp_Kind op_kind, Expression *_expr) {
    Expression *expr = xmalloc(sizeof(Expression));
    expr->kind = ExpressionKind_UnaryOp;
    expr->type.UnaryOp.op_tok = op_tok;
    expr->type.UnaryOp.op_kind = op_kind;
    expr->type.UnaryOp.expr = _expr;
    return expr;
}

Expression *Expr_BinaryOp(Token op_tok, BinaryOp_Kind op_kind, Expression *left,
                          Expression *right) {
    Expression *expr = xmalloc(sizeof(Expression));
    expr->kind = ExpressionKind_BinaryOp;
    expr->type.BinaryOp.op_tok = op_tok;
    expr->type.BinaryOp.op_kind = op_kind;
    expr->type.BinaryOp.left = left;
    expr->type.BinaryOp.right = right;
    return expr;
}

typedef enum {
    Precedence_None,        // Returned when token is invaild to stop parsing i.e isn't
                            // binary, unary or ternary
    Precedence_Lowest,      // Lowest precedence
    Precedence_BitwiseOr,   // |
    Precedence_BitwiseXor,  // ^
    Precedence_BitwiseAnd,  // &
    Precedence_Equality,    // == !=
    Precedence_Comparison,  // < > <= >=
    Precedenc_BitwiseShits, // << >>
    Precedence_Term,        // - +
    Precedence_Factor,      // * /
    Precedence_Prefix,      // ! - + * & ~

    /* 	Left-to-right
     * [] 	Array subscripting
     */
    Precedence_Suffix, // :: (highest precedence)
} Precedence;

Precedence get_binary_operator_precedenc(Token_Kind kind, BinaryOp_Kind *op_kind) {
    switch (kind) {
    /* Bitwise Operations */
    case TOKEN_BITWISE_OR: *op_kind = BinaryOp_BitwiseOr; return Precedence_BitwiseOr;
    case TOKEN_BITWISE_XOR: *op_kind = BinaryOp_BitwiseXor; return Precedence_BitwiseXor;
    case TOKEN_BITWISE_AND: *op_kind = BinaryOp_BitwiseAnd; return Precedence_BitwiseAnd;

    /* Term */
    case TOKEN_PLUS: *op_kind = BinaryOp_Plus; return Precedence_Term;
    case TOKEN_MINUS: *op_kind = BinaryOp_Minus; return Precedence_Term;

    /* Factor */
    case TOKEN_MUL: *op_kind = BinaryOp_Mul; return Precedence_Factor;
    case TOKEN_DIVIDE: *op_kind = BinaryOp_Divide; return Precedence_Factor;

    default:
        return Precedence_None; /* returning None would stop parsing,
                                  since parser look for higher
                                  precedence */
    }
}

Expression *parse_expression(Parser *parser, Precedence precedence);
Expression *parse_unary_expr(Parser *parser, UnaryOp_Kind op_kind);

Expression *parse_primary_expression(Parser *parser) {
    Expression *expr;

    if (is_token_number(parser->current_token)) {
        return Expr_NumberLiteral(*parser->current_token,
                                  cast(u16) number_token_to_i64(parser->current_token));
    }

    switch (parser->current_token->kind) {
    case TOKEN_IDENTIFIER: return Expr_Identifier(*parser->current_token);
    case TOKEN_DOLLAR: return Expr_LocationCounter(*parser->current_token, 0x0);

    case TOKEN_LEFT_PAREN:
        parser_next_token(parser);
        expr = parse_expression(parser, Precedence_Lowest);
        parser_expect_next(parser, TOKEN_RIGHT_PAREN);
        return expr;

    /* Unary expressions */
    case TOKEN_PLUS: return parse_unary_expr(parser, UnaryOp_Plus);
    case TOKEN_MINUS: return parse_unary_expr(parser, UnaryOp_Minus);
    case TOKEN_TILDA: return parse_unary_expr(parser, UnaryOp_BitwiseNot);

    default: Unreachable();
    }
}

Expression *parse_unary_expr(Parser *parser, UnaryOp_Kind op_kind) {
    Token op_tok;
    Expression *expr;

    memcpy(&op_tok, parser->current_token, sizeof(Token));

    parser_next_token(parser);
    expr = parse_expression(parser, Precedence_Prefix);
    if (!expr) return NULL;

    return Expr_UnaryOp(op_tok, op_kind, expr);
}

Expression *parse_binary_expr(Parser *p, Expression *left, Precedence precedence,
                              BinaryOp_Kind binop_kind) {
    Token op_tok;
    Expression *right;

    parser_next_token(p);
    memcpy(&op_tok, p->current_token, sizeof(Token));

    parser_next_token(p);
    right = parse_expression(p, precedence);
    if (!right) return NULL;

    return Expr_BinaryOp(op_tok, binop_kind, left, right);
}

Expression *parse_expression(Parser *p, Precedence precedence) {
    Expression *left;
    Token *next;
    BinaryOp_Kind binop_kind;

    left = parse_primary_expression(p);
    if (!left) {
        return NULL;
    }

    next = parser_peek_next(p);
    while (next && next->kind != TOKEN_END_OF_FILE &&
           precedence < get_binary_operator_precedenc(next->kind, &binop_kind)) {
        left = parse_binary_expr(p, left, precedence, binop_kind);
        next = parser_peek_next(p);
    }

    return left;
}

// --------------------------------------------------------------------------
//                          - Assemblar -
// --------------------------------------------------------------------------

typedef struct Label Label;
struct Label {
    Token token;
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

void assemblar_insert_label(Assemblar *as, Token token, u16 offset) {
    Label label = (Label){
        .token = token, .name = make_string(token.text, token.text_length), .offset = offset};
    array_push(as->label_table, label);
}

typedef enum {
    OperandKind_NumberLiteral,
    OperandKind_RegisterType,
    OperandKind_Expression,
    OperandKind_LocationCounter,
    OperandKind_AsciiConstant,
    OperandKind_LabelAssignedValue,
    OperandKind_LabelInstruction,
} Assemblar_OperandKind;

u16 assemblar_evaluate_expression(Assemblar *as, Parser *parser, Expression *expr) {
    switch (expr->kind) {
    case ExpressionKind_NumberLiteral: return expr->type.NumberLiteral.number;
    case ExpressionKind_LocationCounter: return as->current_address;

    case ExpressionKind_Identifier: {
        Label *label = assemblar_find_label(as, expr->type.Identifier.token.text,
                                            expr->type.Identifier.token.text_length);
        if (!label) {
            parser_log_error(parser, parser->current_token, "Error", "Label not defined '%.*s'",
                             parser->current_token->text_length, parser->current_token->text);
        } else {
            return label->offset;
        }
    };

    case ExpressionKind_UnaryOp: {
        u16 res = assemblar_evaluate_expression(as, parser, expr->type.UnaryOp.expr);

        switch (expr->type.UnaryOp.op_kind) {
        case UnaryOp_Plus: break;

        case UnaryOp_Minus: res = -res; break;

        case UnaryOp_BitwiseNot: res = ~res; break;

        default: Unreachable();
        }

        return res;
    };

    case ExpressionKind_BinaryOp: {
        u16 left = assemblar_evaluate_expression(as, parser, expr->type.BinaryOp.left);
        u16 right = assemblar_evaluate_expression(as, parser, expr->type.BinaryOp.right);

        switch (expr->type.BinaryOp.op_kind) {
        case BinaryOp_Plus: return left + right;

        case BinaryOp_Minus: return left - right;

        case BinaryOp_Mul: return left * right;

        case BinaryOp_Divide: return left / right;

        case BinaryOp_BitwiseXor: return left ^ right;

        case BinaryOp_BitwiseOr: return left | right;

        case BinaryOp_BitwiseAnd: return left & right;

        default: Unreachable();
        }
    };

    default: Unreachable();
    }
}

static bool assemblar_get_addr_operand(Assemblar *as, Parser *parser, u16 *addr) {
    bool has_addr = true;
    Expression *expr = parse_expression(parser, Precedence_Lowest);

    if (!expr) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected address or label, instead got '%.*s'",
                         parser->current_token->text_length, parser->current_token->text);
        has_addr = false;
    } else {
        *addr = assemblar_evaluate_expression(as, parser, expr);
    }

    free_expression(expr);
    return has_addr;
}

// --------------------------------------------------------------------------
//                          - Emit bytes -
// --------------------------------------------------------------------------
static void emit_byte(Assemblar *as, u8 byte) {
    as->bytecode[as->current_address] = byte;
    as->current_address += 1;
}

static void emit_ins_with_one_reg(Assemblar *as, Parser *parser, u8 byteWithA, u8 byteB, u8 byteC,
                                  u8 byteD, u8 byteE, u8 byteH, u8 byteL, u8 byteM) {
    if (!parser_peek_next(parser)) {
        parser_log_error_expected_register(parser, a b c d e h l m);
        return;
    }

    parser_next_token(parser);
    switch (parser->current_token->kind) {
    case TOKEN_KW_GENERAL_REG_A: emit_byte(as, byteWithA); break;
    case TOKEN_KW_GENERAL_REG_B: emit_byte(as, byteB); break;
    case TOKEN_KW_GENERAL_REG_C: emit_byte(as, byteC); break;
    case TOKEN_KW_GENERAL_REG_D: emit_byte(as, byteD); break;
    case TOKEN_KW_GENERAL_REG_E: emit_byte(as, byteE); break;
    case TOKEN_KW_GENERAL_REG_H: emit_byte(as, byteH); break;
    case TOKEN_KW_GENERAL_REG_L: emit_byte(as, byteL); break;
    case TOKEN_KW_GENERAL_REG_M: emit_byte(as, byteM); break;

    default: parser_log_error_expected_register(parser, a b c d e h l m);
    }
}

static void emit_ins_with_rp(Assemblar *as, Parser *parser, u8 bByte, u8 dByte, u8 hByte,
                             u8 spByte) {
    if (!parser_peek_next(parser)) {
        parser_log_error_expected_register_pair(parser, b d h sp);
        return;
    }

    switch (parser->current_token->kind) {
    case TOKEN_KW_GENERAL_REG_B: emit_byte(as, bByte); break;
    case TOKEN_KW_GENERAL_REG_D: emit_byte(as, dByte); break;
    case TOKEN_KW_GENERAL_REG_H: emit_byte(as, hByte);
    case TOKEN_KW_SPECIAL_REG_SP: emit_byte(as, spByte); break;

    default: parser_log_error_expected_register_pair(parser, b d h sp);
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

    if (!is_token_number(parser->current_token)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected immediate byte, instead got '%*.s'",
                         parser->current_token->text_length, parser->current_token->text);
    } else {
        f64 num = number_token_to_i64(parser->current_token);
        emit_byte(as, cast(u8)((uint)num & 0xff));
    }
}

static void emit_single_mvi_ins(Assemblar *as, Parser *parser, u8 emitByte) {
    parser_expect_next(parser, TOKEN_COMMA);
    parser_next_token(parser);

    if (!is_token_number(parser->current_token)) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected immediate byte, instead got '%*.s'",
                         parser->current_token->text_length, parser->current_token->text);
    } else {
        emit_byte(as, emitByte);
        f64 num = number_token_to_i64(parser->current_token);
        emit_byte(as, cast(u8)((uint)num & 0xff));
    }
}

static void emit_stack_op_ins(Assemblar *as, Parser *parser, u8 byteBC, u8 byteDE, u8 byteHL,
                              u8 bytePSW) {
    if (!parser_peek_next(parser)) {
        parser_log_error_expected_register(parser, b d h psw);
        return;
    }

    parser_next_token(parser);
    switch (parser->current_token->kind) {
    case TOKEN_KW_GENERAL_REG_B: emit_byte(as, byteBC); break;
    case TOKEN_KW_GENERAL_REG_D: emit_byte(as, byteDE); break;
    case TOKEN_KW_GENERAL_REG_H: emit_byte(as, byteHL);
    case TOKEN_KW_SPECIAL_REG_PSW: emit_byte(as, bytePSW); break;

    default: parser_log_error_expected_register_pair(parser, b d h psw);
    }
}

static void emit_rst_ins(Assemblar *as, Parser *parser, u8 byte0, u8 byte1, u8 byte2, u8 byte3,
                         u8 byte4, u8 byte5, u8 byte6, u8 byte7) {
    parser_next_token(parser);
    Expression *expr = parse_expression(parser, Precedence_Lowest);

    if (!expr) {
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected number(0..7), instead got '%*.s'",
                         parser->current_token->text_length, parser->current_token->text);
        return;
    }

    u16 operand = assemblar_evaluate_expression(as, parser, expr);

    switch (operand) {
    case 0: emit_byte(as, byte0); break;
    case 1: emit_byte(as, byte1); break;
    case 2: emit_byte(as, byte2); break;
    case 3: emit_byte(as, byte3); break;
    case 4: emit_byte(as, byte4); break;
    case 5: emit_byte(as, byte5); break;
    case 6: emit_byte(as, byte6); break;
    case 7: emit_byte(as, byte7); break;
    default:
        parser_log_error(parser, parser->current_token, "Syntax Error",
                         "Expected number(0..7), instead got '%d'", operand);
    }

    free_expression(expr);
}

static void emit_ldax_ins(Assemblar *as, Parser *parser, u8 byteBC, u8 byteDE) {
    if (!parser_peek_next(parser)) {
        parser_log_error_expected_register_pair(parser, b d);
        return;
    }

    parser_next_token(parser);
    switch (parser->current_token->kind) {
    case TOKEN_KW_GENERAL_REG_B: emit_byte(as, byteBC); break;
    case TOKEN_KW_GENERAL_REG_D: emit_byte(as, byteDE); break;

    default: parser_log_error_expected_register_pair(parser, b d);
    }
}

static void emit_lxi_ins(Assemblar *as, Parser *parser, u8 byteBC, u8 byteDE, u8 byteHL,
                         u8 byteSP) {
    bool has_error = false;

    if (!parser_peek_next(parser)) {
        parser_log_error_expected_register_pair(parser, b d h);
        return;
    }

    parser_next_token(parser);
    switch (parser->current_token->kind) {
    case TOKEN_KW_GENERAL_REG_B: emit_byte(as, byteBC); break;
    case TOKEN_KW_GENERAL_REG_D: emit_byte(as, byteDE); break;
    case TOKEN_KW_GENERAL_REG_H: emit_byte(as, byteHL); break;
    case TOKEN_KW_SPECIAL_REG_SP: emit_byte(as, byteSP); break;

    default: has_error = true; parser_log_error_expected_register_pair(parser, b d h);
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

static inline void emit_single_mov_ins(Assemblar *as, Parser *parser, u8 byteA, u8 byteB, u8 byteC,
                                       u8 byteD, u8 byteE, u8 byteH, u8 byteL, u8 byteM) {
    parser_expect_next(parser, TOKEN_COMMA);
    emit_ins_with_one_reg(as, parser, byteA, byteB, byteC, byteD, byteE, byteH, byteL, byteM);
}

void emit_ins_mov(Assemblar *as, Parser *parser) {
    if (!parser_peek_next(parser)) {
        parser_log_error_expected_register(parser, a b c d e h l m);
        return;
    }

    parser_next_token(parser);
    switch (parser->current_token->kind) {
    case TOKEN_KW_GENERAL_REG_A:
        emit_single_mov_ins(as, parser, 0x7f, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e);
        break;

    case TOKEN_KW_GENERAL_REG_B:
        emit_single_mov_ins(as, parser, 0x47, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46);
        break;

    case TOKEN_KW_GENERAL_REG_C:
        emit_single_mov_ins(as, parser, 0x4f, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e);
        break;

    case TOKEN_KW_GENERAL_REG_D:
        emit_single_mov_ins(as, parser, 0x57, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56);
        break;

    case TOKEN_KW_GENERAL_REG_E:
        emit_single_mov_ins(as, parser, 0x5f, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e);
        break;

    case TOKEN_KW_GENERAL_REG_H:
        emit_single_mov_ins(as, parser, 0x67, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66);
        break;

    case TOKEN_KW_GENERAL_REG_L:
        emit_single_mov_ins(as, parser, 0x6f, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e);
        break;

    case TOKEN_KW_GENERAL_REG_M: {
        parser_expect_next(parser, TOKEN_COMMA);

        if (!parser_peek_next(parser)) {
            parser_log_error_expected_register(parser, a b c d e h l);
            return;
        }

        parser_next_token(parser);
        switch (parser->current_token->kind) {
        case TOKEN_KW_GENERAL_REG_A: emit_byte(as, 0x77); break;
        case TOKEN_KW_GENERAL_REG_B: emit_byte(as, 0x70); break;
        case TOKEN_KW_GENERAL_REG_C: emit_byte(as, 0x71); break;
        case TOKEN_KW_GENERAL_REG_D: emit_byte(as, 0x72); break;
        case TOKEN_KW_GENERAL_REG_E: emit_byte(as, 0x73); break;
        case TOKEN_KW_GENERAL_REG_H: emit_byte(as, 0x74); break;
        case TOKEN_KW_GENERAL_REG_L: emit_byte(as, 0x75); break;
        default: parser_log_error_expected_register(parser, a b c d e h l);
        }
    } break;

    default: parser_log_error_expected_register(parser, a b c d e h l m);
    }
}

void emit_ins_mvi(Assemblar *as, Parser *parser) {
    if (!parser_peek_next(parser)) {
        parser_log_error_expected_register(parser, a b c d e h l);
        return;
    }

    parser_next_token(parser);
    switch (parser->current_token->kind) {
    case TOKEN_KW_GENERAL_REG_A: emit_single_mvi_ins(as, parser, 0x3e); break;
    case TOKEN_KW_GENERAL_REG_B: emit_single_mvi_ins(as, parser, 0x06); break;
    case TOKEN_KW_GENERAL_REG_C: emit_single_mvi_ins(as, parser, 0x0e); break;
    case TOKEN_KW_GENERAL_REG_D: emit_single_mvi_ins(as, parser, 0x16); break;
    case TOKEN_KW_GENERAL_REG_E: emit_single_mvi_ins(as, parser, 0x1e); break;
    case TOKEN_KW_GENERAL_REG_H: emit_single_mvi_ins(as, parser, 0x26); break;
    case TOKEN_KW_GENERAL_REG_L: emit_single_mvi_ins(as, parser, 0x2e); break;
    case TOKEN_KW_GENERAL_REG_M: emit_single_mvi_ins(as, parser, 0x36); break;
    default: parser_log_error_expected_register(parser, a b c d e h l m);
    }
}

void assemblar_visit_identifier(Assemblar *as, Parser *parser) {
    Token *next = parser_peek_next(parser);

    if (!next) {
        parser_log_error(parser, parser->current_token, "Error", "Unknown parse rule '%.*s' ",
                         parser->current_token->text_length, parser->current_token->text);
        parser_next_token(parser);
    } else {
        if (next->kind == TOKEN_COLON) {
            assemblar_insert_label(as, *parser->current_token, as->current_address);
            parser_next_token(parser);
        } else {
            parser_log_error(parser, parser->current_token, "Error", "Unknown parse rule '%.*s' ",
                             parser->current_token->text_length, parser->current_token->text);
            parser_next_token(parser);
        }
    }
}

void assemblar_emit_object_code(Assemblar *as) {
    Parser *parser = make_parser(as->source_filepath, &as->source_string);
    Token *next = parser_peek_next(parser);

    while (next && next->kind != TOKEN_END_OF_FILE) {
        parser_next_token(parser);

        switch (parser->current_token->kind) {
        /* label */
        case TOKEN_IDENTIFIER: assemblar_visit_identifier(as, parser); break;

        case TOKEN_KW_HLT: emit_byte(as, 0x76); break;

        case TOKEN_KW_MOV: emit_ins_mov(as, parser); break;

        /* instructions with single register operand */
        case TOKEN_KW_ADD:
            emit_ins_with_one_reg(as, parser, 0x87, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86);
            break;
        case TOKEN_KW_ADC:
            emit_ins_with_one_reg(as, parser, 0x8f, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e);
            break;
        case TOKEN_KW_SUB:
            emit_ins_with_one_reg(as, parser, 0x87, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96);
            break;
        case TOKEN_KW_SBB:
            emit_ins_with_one_reg(as, parser, 0x9f, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e);
            break;
        case TOKEN_KW_ANA:
            emit_ins_with_one_reg(as, parser, 0xa7, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6);
            break;
        case TOKEN_KW_XRA:
            emit_ins_with_one_reg(as, parser, 0xaf, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae);
            break;
        case TOKEN_KW_ORA:
            emit_ins_with_one_reg(as, parser, 0xb7, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6);
            break;
        case TOKEN_KW_CMP:
            emit_ins_with_one_reg(as, parser, 0xbf, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe);
            break;
        case TOKEN_KW_INR:
            emit_ins_with_one_reg(as, parser, 0x3c, 0x04, 0x0c, 0x14, 0x1c, 0x24, 0x2c, 0x34);
            break;
        case TOKEN_KW_DCR:
            emit_ins_with_one_reg(as, parser, 0x3d, 0x05, 0x0d, 0x15, 0x1d, 0x25, 0x2d, 0x35);
            break;

        /* instruction with register pairs */
        case TOKEN_KW_INX: emit_ins_with_rp(as, parser, 0x03, 0x13, 0x23, 0x33); break;
        case TOKEN_KW_DCX: emit_ins_with_rp(as, parser, 0x0b, 0x1b, 0x2b, 0x3b); break;
        case TOKEN_KW_DAD: emit_ins_with_rp(as, parser, 0x09, 0x19, 0x29, 0x39); break;

        /* rotation instructions */
        case TOKEN_KW_RLC: emit_byte(as, 0x07); break;
        case TOKEN_KW_RRC: emit_byte(as, 0x0f); break;
        case TOKEN_KW_RAL: emit_byte(as, 0x17); break;
        case TOKEN_KW_RAR: emit_byte(as, 0x1f); break;

        case TOKEN_KW_CMA: emit_byte(as, 0x2f); break;

        /* carry instructions */
        case TOKEN_KW_CMC: emit_byte(as, 0x3f); break;
        case TOKEN_KW_STC: emit_byte(as, 0x37); break;

        /* return instructions */
        case TOKEN_KW_RET: emit_byte(as, 0xc9); break;
        case TOKEN_KW_RZ: emit_byte(as, 0xc8); break;
        case TOKEN_KW_RNZ: emit_byte(as, 0xc0); break;
        case TOKEN_KW_RC: emit_byte(as, 0xd8); break;
        case TOKEN_KW_RNC: emit_byte(as, 0xd0); break;
        case TOKEN_KW_RPO: emit_byte(as, 0xe0); break;
        case TOKEN_KW_RPE: emit_byte(as, 0xe8); break;

        /* jump instructions */
        case TOKEN_KW_JMP: emit_ins_with_addr(as, parser, 0xc3); break;
        case TOKEN_KW_JC: emit_ins_with_addr(as, parser, 0xda); break;
        case TOKEN_KW_JNC: emit_ins_with_addr(as, parser, 0xd2); break;
        case TOKEN_KW_JZ: emit_ins_with_addr(as, parser, 0xca); break;
        case TOKEN_KW_JNZ: emit_ins_with_addr(as, parser, 0xc2); break;
        case TOKEN_KW_JPO: emit_ins_with_addr(as, parser, 0xe2); break;
        case TOKEN_KW_JPE: emit_ins_with_addr(as, parser, 0xea); break;
        case TOKEN_KW_JP: emit_ins_with_addr(as, parser, 0xf2); break;
        case TOKEN_KW_JM: emit_ins_with_addr(as, parser, 0xfa); break;

        /* call instructions */
        case TOKEN_KW_CALL: emit_ins_with_addr(as, parser, 0xcd); break;
        case TOKEN_KW_CC: emit_ins_with_addr(as, parser, 0xdc); break;
        case TOKEN_KW_CNC: emit_ins_with_addr(as, parser, 0xd4); break;
        case TOKEN_KW_CZ: emit_ins_with_addr(as, parser, 0xcc); break;
        case TOKEN_KW_CNZ: emit_ins_with_addr(as, parser, 0xc4); break;
        case TOKEN_KW_CPO: emit_ins_with_addr(as, parser, 0xe4); break;
        case TOKEN_KW_CPE: emit_ins_with_addr(as, parser, 0xec); break;
        case TOKEN_KW_CP: emit_ins_with_addr(as, parser, 0xf4); break;
        case TOKEN_KW_CM: emit_ins_with_addr(as, parser, 0xfc); break;

        /* store/load instructions */
        case TOKEN_KW_SHLD: emit_ins_with_addr(as, parser, 0x22); break;
        case TOKEN_KW_LHLD: emit_ins_with_addr(as, parser, 0x28); break;
        case TOKEN_KW_STA: emit_ins_with_addr(as, parser, 0x32); break;
        case TOKEN_KW_LDA: emit_ins_with_addr(as, parser, 0x3a); break;
        case TOKEN_KW_LDAX: emit_ldax_ins(as, parser, 0x0a, 0x1a); break;
        case TOKEN_KW_STAX: emit_ldax_ins(as, parser, 0x02, 0x12); break;
        case TOKEN_KW_LXI: emit_lxi_ins(as, parser, 0x01, 0x11, 0x21, 0x31); break;

        /* 1 byte instructions with 8-bit immediate data */
        case TOKEN_KW_ADI: emit_ins_with_imm_byte(as, parser, 0xc6); break;
        case TOKEN_KW_ACI: emit_ins_with_imm_byte(as, parser, 0xce); break;
        case TOKEN_KW_SUI: emit_ins_with_imm_byte(as, parser, 0xd6); break;
        case TOKEN_KW_SBI: emit_ins_with_imm_byte(as, parser, 0xde); break;
        case TOKEN_KW_ANI: emit_ins_with_imm_byte(as, parser, 0xe6); break;
        case TOKEN_KW_XRI: emit_ins_with_imm_byte(as, parser, 0xee); break;
        case TOKEN_KW_ORI: emit_ins_with_imm_byte(as, parser, 0xf6); break;
        case TOKEN_KW_CPI: emit_ins_with_imm_byte(as, parser, 0xfe); break;
        case TOKEN_KW_OUT: emit_ins_with_imm_byte(as, parser, 0x3a); break;
        case TOKEN_KW_IN: emit_ins_with_imm_byte(as, parser, 0x3a); break;

        case TOKEN_KW_MVI: emit_ins_mvi(as, parser); break;

        /* stack instruction */
        case TOKEN_KW_PUSH: emit_stack_op_ins(as, parser, 0xc5, 0xd5, 0xe5, 0xf5); break;
        case TOKEN_KW_POP: emit_stack_op_ins(as, parser, 0xc1, 0xd1, 0xe1, 0xf1); break;
        case TOKEN_KW_PCHL: emit_byte(as, 0xe9); break;
        case TOKEN_KW_XTHL: emit_byte(as, 0xe3); break;

        /* RST instructions */
        case TOKEN_KW_RST:
            emit_rst_ins(as, parser, 0xc7, 0xcf, 0xd7, 0xdf, 0xe7, 0xef, 0xf7, 0xff);
            break;

        /* Interrupt Enable/Disable instructions */
        case TOKEN_KW_EI: emit_byte(as, 0xfb); break;
        case TOKEN_KW_DI: emit_byte(as, 0xf3); break;

        /* Last error */
        default:
            parser_log_error(parser, parser->current_token, "Error", "Unknown Instruction '%.*s' ",
                             parser->current_token->text_length, parser->current_token->text);
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

#ifdef Debug
void cli_dump_tokens(char *source_filepath) {
    char *source_string = file_as_string(source_filepath);
    if (!source_string) {
        eprintln("error: couldn't read input file: %s", source_filepath);
        return;
    }

    Lexer *l = make_lexer(&source_string);
    Array(Token) tokens = slurp_tokens(l);
    array_for_each(tokens, i) {
        print_token(&tokens[i]);
        printf("\n");
    }
    free_string(source_string);
    free_array(tokens);
    free(l);
}
#endif

int main(int argc, char **argv) {
    char *source_filepath;
    char *output_filepath = NULL;

#ifdef Debug
    bool is_lex_tokens = false;
#endif

    Cli_Flag positionals[] = {
        Flag_CString_Positional(&source_filepath, "SOURCE_FILEPATH", "asm source filepath")};

    Cli_Flag optionals[] = {
        Flag_CString(&output_filepath, "o", "output", "output binary filepath"),
#ifdef Debug
        Flag_Bool(&is_lex_tokens, "l", "lex-tokens", "output lexical tokens"),
#endif
    };

    Cli cli =
        create_cli(argc, argv, "assemblar8080", positionals, array_sizeof(positionals, Cli_Flag),
                   optionals, array_sizeof(optionals, Cli_Flag));

    cli_parse_args(&cli);
    if (cli_has_error(&cli)) {
        exit(EXIT_FAILURE);
    }

#ifdef Debug
    if (is_lex_tokens) {
        cli_dump_tokens(source_filepath);
        return 0;
    }
#endif

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

#if 0
    array_for_each(assemblar.label_table, i) {
        debug_println("labels: ");
        debug_println("name: %s, length: %zu, addr: %d", assemblar.label_table[i].name,
                      string_length(assemblar.label_table[i].name),
                      assemblar.label_table[i].offset);
    }
#endif

    deinit_assemblar(&assemblar);
    return 0;
}
