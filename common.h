#ifndef __common_h__
#define __common_h__

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// --------------------------------------------------------------------------
//                          - Basic -
// --------------------------------------------------------------------------

typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;

typedef float f32;
typedef double f64;

typedef uintptr_t uintptr;
typedef intptr_t intptr;

typedef size_t usize;
typedef ptrdiff_t isize;

#define cast(Type) (Type)
#define unreachable() assert(false && "unreachable")
#define todo() assert(false && "todo")
#define swap(a, b, Type)                                                       \
    do {                                                                       \
        Type tmp = a;                                                          \
        a = b;                                                                 \
        b = tmp;                                                               \
    } while (0);

#define array_sizeof(arr, Type) (sizeof(arr) / sizeof(Type))
#define are_cstrings_equal(cstr, str, length) (strncmp(cstr, str, length) == 0)

#define println(...)                                                           \
    fprintf(stdout, __VA_ARGS__);                                              \
    putc('\n', stdout);
#define eprintln(...)                                                          \
    fprintf(stderr, __VA_ARGS__);                                              \
    putc('\n', stderr);

void die(const char *fmt);
void *xmalloc(usize size);
void *xrealloc(void *ptr, usize size);
char *file_to_string(char *filepath);

// --------------------------------------------------------------------------
//                          - Character -
// --------------------------------------------------------------------------

bool is_binary_digit(int ch);
bool is_octal_digit(int ch);
bool is_decimal_digit(int ch);
bool is_hex_digit(int ch);
bool is_alphabet(int ch);
bool is_alphanumeric(int ch);

#define binary_digit_to_int(digit) (digit - '0')
#define octal_digit_to_int(digit) (digit - '0')
#define decimal_digit_to_int(digit) (digit - '0')
#define hex_digit_to_int(digit)                                                \
    ((digit >= 'a' && digit <= 'f')                                            \
         ? (digit - 'a' + 10)                                                  \
         : ((digit >= 'A' && digit <= 'F') ? (digit - 'A' + 10)                \
                                           : (digit - '0')))

f64 base2_to_f64(char *str, usize length);
f64 base8_to_f64(char *str, usize length);
f64 base10_to_f64(char *str, usize length);
f64 base16_to_f64(char *str, usize length);

// --------------------------------------------------------------------------
//                          - Array -
// --------------------------------------------------------------------------

#if 0 // Array Example
void main(void) {
    Array(int) a;

    init_array(a);

    array_push(a, 1);
    array_push(a, 2);
    array_push(a, 3);
    array_push(a, 4);

    array_for_each(a, i) {
        printf("%d\n", a[i]);
    }

    free_array(a);
}
#endif

typedef struct ArrayHeader ArrayHeader;
struct ArrayHeader {
    usize length;
    usize capacity;
};

#define ARRAY_GROW_FORMULA(x) (2 * (x) + 8)
#define ARRAY_HEADER(a) ((ArrayHeader *)(a)-1)

#define Array(Type) Type *
#define array_length(a) ARRAY_HEADER(a)->length
#define array_capacity(a) ARRAY_HEADER(a)->capacity
#define free_array(a) free(ARRAY_HEADER(a))
#define array_for_each(a, counter)                                             \
    for (usize(counter) = 0; (counter) < array_length(a); ++(counter))

#define array_reserve(a, cap)                                                  \
    do {                                                                       \
        void **__array = (void **)&(a);                                        \
        ArrayHeader *h = (ArrayHeader *)xmalloc(sizeof(ArrayHeader) +          \
                                                (sizeof(*(a)) * (cap)));       \
        h->capacity = cap;                                                     \
        h->length = 0;                                                         \
        *__array = (void *)(h + 1);                                            \
    } while (0);

#define init_array(a) array_reserve(a, ARRAY_GROW_FORMULA(0))
#define array_push(a, VAL)                                                     \
    do {                                                                       \
        if (array_length(a) >= array_capacity(a)) {                            \
            void **__array = (void **)&(a);                                    \
            ArrayHeader *h = ARRAY_HEADER(a);                                  \
            ArrayHeader *nh = (ArrayHeader *)xmalloc(                          \
                sizeof(ArrayHeader) +                                          \
                sizeof(*(a)) * ARRAY_GROW_FORMULA(array_capacity(a)));         \
            memmove(nh, h,                                                     \
                    sizeof(ArrayHeader) + sizeof(*(a)) * array_length(a));     \
            nh->capacity = ARRAY_GROW_FORMULA(array_length(a));                \
            nh->length = h->length;                                            \
            free(h);                                                           \
            *__array = (void *)(nh + 1);                                       \
        }                                                                      \
        a[array_length(a)] = VAL;                                              \
        array_length(a) += 1;                                                  \
    } while (0);

// --------------------------------------------------------------------------
//                          - String -
// --------------------------------------------------------------------------

#if 0 // String Example
void main(void) {
    String s = make_string_empty();

    s = append_string("Hello");
    s = append_string(", World");

    printf("%s", s);

    free_string(a);
}
#endif

typedef char *String;

#define STRING_HEADER(s) ((StringHeader *)(s)-1)
#define free_string(s) free(STRING_HEADER(s))
#define string_length(s) (STRING_HEADER(s)->length)
#define string_capacity(s) (STRING_HEADER(s)->capacity)
#define string_for_each(s, i) for (usize i = 0; i < string_length(s); ++i)
#define append_string(s, str) append_string_length(s, str, strlen(str))

typedef struct StringHeader StringHeader;
struct StringHeader {
    usize length;
    usize capacity;
};

String string_reserve(usize cap);
String make_string_empty();
String make_string(char *str, usize len);
String append_string_length(String s, char *str, usize len);
bool are_equal_strings(String lhs, String rhs);
bool are_equal_cstring(String lhs, char *rhs);

#endif