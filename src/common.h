#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define cast(Type) (Type)
#define unreachable() assert(false && "unreachable")
#define swap(a, b, Type)\
    do {\
        Type tmp = a;\
        a = b;\
        b = tmp;\
    } while (0);

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