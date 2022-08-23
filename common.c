#include "common.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void die(const char *fmt) {
    perror(fmt);
    exit(1);
}

void *xmalloc(usize size) {
    void *ptr = malloc(size);
    if (!ptr)
        die("malloc");
    return ptr;
}

void *xrealloc(void *ptr, usize size) {
    void *_ptr = realloc(ptr, size);
    if (!_ptr)
        die("realloc");
    return _ptr;
}

char *file_to_string(char *filepath) {
    FILE *f;
    char *content;
    u32 content_size;

    f = fopen(filepath, "r");
    if (!f) {
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    content_size = ftell(f);
    rewind(f);

    content = (char *)xmalloc(content_size + 1);
    fread(content, 1, content_size, f);
    content[content_size] = '\0';

    fclose(f);
    return content;
}

// --------------------------------------------------------------------------
//                          - Character -
// --------------------------------------------------------------------------
bool is_binary_digit(int ch) { return (ch == '0' || ch == '1'); }
bool is_octal_digit(int ch) { return ((ch >= '1' && ch <= '8')); }
bool is_decimal_digit(int ch) { return (ch >= '0' && ch <= '9'); }
bool is_hex_digit(int ch) {
    return (is_decimal_digit(ch) || (ch >= 'a' && ch <= 'f') ||
            (ch >= 'A' && ch <= 'F'));
}
bool is_alphabet(int ch) {
    return ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'));
}
bool is_alphanumeric(int ch) {
    return ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
            (ch >= '0' && ch <= '9'));
}

f64 base2_to_f64(char *str, usize length) {
    f64 result = 0;

    for (usize i = length - 1; i > 0; --i, ++str) {
        result += pow(2, i) * binary_digit_to_int(*str);
    }

    result += pow(2, 0) * decimal_digit_to_int(*str);
    return result;
}

f64 base8_to_f64(char *str, usize length) {
    f64 result = 0;

    for (usize i = length - 1; i >= 0; --i, ++str) {
        result += pow(8, i) * octal_digit_to_int(*str);
    }

    result += pow(8, 0) * decimal_digit_to_int(*str);
    return result;
}

f64 base10_to_f64(char *str, usize length) {
    f64 result = 0;

    for (usize i = length - 1; i > 0; --i, ++str) {
        result += pow(10, i) * decimal_digit_to_int(*str);
    }

    result += pow(10, 0) * decimal_digit_to_int(*str);
    return result;
}

f64 base16_to_f64(char *str, usize length) {
    f64 result = 0;

    for (usize i = length - 1; i > 0; --i, ++str) {
        result += pow(16, i) * hex_digit_to_int(*str);
    }

    result += pow(16, 0) * hex_digit_to_int(*str);
    return result;
}

// --------------------------------------------------------------------------
//                          - String -
// --------------------------------------------------------------------------
String string_reserve(usize cap) {
    StringHeader *h;

    h = (StringHeader *)malloc(sizeof(StringHeader) + cap + 1);
    h->length = 0;
    h->capacity = cap;

    return (String)(h + 1);
}

String make_string_empty() { return string_reserve(0); }

String make_string(char *str, usize len) {
    String s = string_reserve(len);
    memcpy(s, str, len);
    s[len] = '\0';
    return s;
}

String append_string_length(String s, char *str, usize len) {
    usize cap;
    usize rem = string_capacity(s) - string_length(s);

    if (rem < len) {
        StringHeader *h = STRING_HEADER(s);
        cap = h->capacity + len + (1 << 6);
        h = (StringHeader *)realloc(h, sizeof(StringHeader) + cap + 1);
        h->capacity = cap;
        s = (String)(h + 1);
    }

    memcpy(&s[string_length(s)], str, len);
    string_length(s) += len;
    s[string_length(s)] = '\0';

    return s;
}

bool are_equal_strings(String lhs, String rhs) {
    if (string_length(lhs) != string_length(rhs))
        return false;
    string_for_each(lhs, i) {
        if (lhs[i] != rhs[i])
            return false;
    }
    return true;
}

bool are_equal_cstring(String lhs, char *rhs) {
    if (string_length(lhs) != strlen(rhs))
        return false;
    string_for_each(lhs, i) {
        if (lhs[i] != rhs[i])
            return false;
    }
    return true;
}