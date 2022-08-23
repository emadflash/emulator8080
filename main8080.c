#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// --------------------------------------------------------------------------
//
//
//                          - Utils -
//
//
// --------------------------------------------------------------------------

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
#define match_string(a, b, len)                                                \
    ((strlen(a) == len) ? (strncmp(a, b, len) == 0) : 0)

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

typedef unsigned int uint;
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

char *file_to_string(char *filepath) {
    FILE *f;
    char *content;
    u32 content_size;

    f = fopen(filepath, "r");

    fseek(f, 0, SEEK_END);
    content_size = ftell(f);
    rewind(f);

    content = xmalloc(content_size + 1);
    fread(content, 1, content_size, f);
    content[content_size] = '\0';

    fclose(f);
    return content;
}

// --------------------------------------------------------------------------
//                          - CharInfo -
// --------------------------------------------------------------------------
bool char_is_binary(int ch) { return (ch == '0' || ch == '1'); }
bool char_is_octal(int ch) { return ((ch >= '1' && ch <= '8')); }
bool char_is_decimal(int ch) { return (ch >= '0' && ch <= '9'); }
bool char_is_hex(int ch) {
    return (char_is_decimal(ch) || (ch >= 'a' && ch <= 'f') ||
            (ch >= 'A' && ch <= 'F'));
}
bool char_is_alpha(int ch) {
    return ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'));
}
bool char_is_alnum(int ch) {
    return ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
            (ch >= '0' && ch <= '9'));
}

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
    usize length, capacity;
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
//
//
//                          - Emulator -
//
//
// --------------------------------------------------------------------------

// --------------------------------------------------------------------------
//                          - Cpu -
// --------------------------------------------------------------------------

typedef struct Cpu Cpu;
struct Cpu {
    u8 *memory;

    u16 pc;
    u16 sp;

    u8 acc;
    u8 b;
    u8 c;
    u8 d;
    u8 e;
    u8 h;
    u8 l;

    u8 cy;
    u8 parity;
    u8 auxillary_cy;
    u8 zero;
    u8 sign;

    u8 halt;
};

Cpu *make_cpu(u8 *memory) {
    Cpu *cpu = xmalloc(sizeof(Cpu));

    cpu->pc = 0x0;
    cpu->sp = 0x0;

    cpu->acc = 0x0;
    cpu->b = 0x0;
    cpu->c = 0x0;
    cpu->d = 0x0;
    cpu->e = 0x0;
    cpu->h = 0x0;
    cpu->l = 0x0;

    cpu->cy = 0x0;
    cpu->parity = 0x0;
    cpu->auxillary_cy = 0x0;
    cpu->zero = 0x0;
    cpu->sign = 0x0;

    cpu->memory = memory;

    return cpu;
}

void free_cpu(Cpu *cpu) { free(cpu); }

// Flag 8-bit register would look like
// [Sign] [Zero] [X] [AC] [X] [Parity] [X] [Carry]
u8 flag_register(Cpu *cpu) {
    return (cpu->sign << 7) | (cpu->zero << 6) | (cpu->auxillary_cy << 4) |
           (cpu->parity << 2) | cpu->cy;
}

u8 has_parity(u8 n) {
    /* Brian kernighan's algorithm to count set bits */
    u8 count = 0;

    while (n) {
        n &= (n - 1);
        count += 1;
    }

    return !(count & 0x1);
}

u8 next_byte(Cpu *cpu) {
    cpu->pc += 1;
    return cpu->memory[cpu->pc];
}

u16 next_address(Cpu *cpu) {
    return (cast(u16)(cpu->memory[cpu->pc + 2] << 8)) |
           cast(u16) cpu->memory[cpu->pc + 1];
}

void run_cpu(Cpu *cpu) {
#define make_addr(a, b) cast(u16)(0x0000 & (a)) | (cast(u16)(b) << 8)

    while (cpu->pc < 65535) {
        u8 opcode = cpu->memory[cpu->pc];

        switch (opcode) {
        case 0x00: break; // NOP
        case 0x01: {      // LXI B
            cpu->b = cpu->memory[cpu->pc + 2];
            cpu->c = cpu->memory[cpu->pc + 1];
            cpu->pc += 2;
        } break;

        case 0x02: { // STAX B
            cpu->memory[make_addr(cpu->b, cpu->c)] = cpu->acc;
        } break;

        /* Increment instructions */

        // NOTE(madflash) - INR ins does not change the carray flag
        // that is why sum is not u16, since we don't need to check d8 bit
#define increment(a, by)                                                       \
    do {                                                                       \
        u8 sum = cast(u8)(a + by);                                             \
        cpu->parity = has_parity(sum);                                         \
        cpu->zero = (sum == 0x0);                                              \
        cpu->sign = ((sum & 0x80) == 0x80);                                    \
        a = sum;                                                               \
    } while (0);

#define inr(a) increment(a, 0x1)
#define dcr(a) increment(a, -0x1)
        case 0x04: inr(cpu->b); break;                                 // INR B
        case 0x0c: inr(cpu->c); break;                                 // INR C
        case 0x14: inr(cpu->d); break;                                 // INR D
        case 0x1c: inr(cpu->e); break;                                 // INR E
        case 0x24: inr(cpu->h); break;                                 // INR H
        case 0x2c: inr(cpu->l); break;                                 // INR L
        case 0x34: inr(cpu->memory[make_addr(cpu->h, cpu->l)]); break; // INR M
        case 0x3c: inr(cpu->acc); break;                               // INR A

        case 0x05: dcr(cpu->b); break;                                 // DCR B
        case 0x0d: dcr(cpu->c); break;                                 // DCR C
        case 0x15: dcr(cpu->d); break;                                 // DCR D
        case 0x1d: dcr(cpu->e); break;                                 // DCR E
        case 0x25: dcr(cpu->h); break;                                 // DCR H
        case 0x2d: dcr(cpu->l); break;                                 // DCR L
        case 0x35: dcr(cpu->memory[make_addr(cpu->h, cpu->l)]); break; // DCR M
        case 0x3d: dcr(cpu->acc); break;                               // DCR A

#define inxcrement(a, b, by)                                                   \
    do {                                                                       \
        u16 res = make_addr(a, b) + by;                                        \
        a = cast(u8)((res & 0xff00) >> 8);                                     \
        b = cast(u8)(res & 0xff);                                              \
    } while (0);
#define inx(a, b) inxcrement(a, b, 0x1)
#define dcx(a, b) inxcrement(a, b, -0x1)
        case 0x03: inx(cpu->b, cpu->c); break;
        case 0x13: inx(cpu->d, cpu->e); break;
        case 0x23: inx(cpu->h, cpu->l); break;
        case 0x33: cpu->sp = cast(u16)(cpu->sp + 0x1); break;

        case 0x0b: dcx(cpu->b, cpu->c); break;
        case 0x1b: dcx(cpu->d, cpu->e); break;
        case 0x2b: dcx(cpu->h, cpu->l); break;
        case 0x3b: cpu->sp = cast(u16)(cpu->sp - 0x1); break;

        /* Mov */
        case 0x40: cpu->b = cpu->b; break;
        case 0x41: cpu->b = cpu->c; break;
        case 0x42: cpu->b = cpu->d; break;
        case 0x43: cpu->b = cpu->e; break;
        case 0x44: cpu->b = cpu->h; break;
        case 0x45: cpu->b = cpu->l; break;
        case 0x46: cpu->b = cpu->memory[make_addr(cpu->h, cpu->l)]; break;
        case 0x47: cpu->b = cpu->acc; break;
        case 0x48: cpu->c = cpu->b; break;
        case 0x49: cpu->c = cpu->c; break;
        case 0x4a: cpu->c = cpu->d; break;
        case 0x4b: cpu->c = cpu->e; break;
        case 0x4c: cpu->c = cpu->h; break;
        case 0x4d: cpu->c = cpu->l; break;
        case 0x4e: cpu->c = cpu->memory[make_addr(cpu->h, cpu->l)]; break;
        case 0x4f: cpu->c = cpu->acc; break;
        case 0x50: cpu->d = cpu->b; break;
        case 0x51: cpu->d = cpu->c; break;
        case 0x52: cpu->d = cpu->d; break;
        case 0x53: cpu->d = cpu->e; break;
        case 0x54: cpu->d = cpu->h; break;
        case 0x55: cpu->d = cpu->l; break;
        case 0x56: cpu->d = cpu->memory[make_addr(cpu->h, cpu->l)]; break;
        case 0x57: cpu->d = cpu->acc; break;
        case 0x58: cpu->d = cpu->b; break;
        case 0x59: cpu->d = cpu->c; break;
        case 0x5a: cpu->b = cpu->d; break;
        case 0x5b: cpu->b = cpu->e; break;
        case 0x5c: cpu->b = cpu->h; break;
        case 0x5d: cpu->b = cpu->l; break;
        case 0x5e: cpu->b = cpu->memory[make_addr(cpu->h, cpu->l)]; break;
        case 0x5f: cpu->b = cpu->acc; break;
        case 0x60: cpu->h = cpu->b; break;
        case 0x61: cpu->h = cpu->c; break;
        case 0x62: cpu->h = cpu->d; break;
        case 0x63: cpu->h = cpu->e; break;
        case 0x64: cpu->h = cpu->h; break;
        case 0x65: cpu->h = cpu->l; break;
        case 0x66: cpu->h = cpu->memory[make_addr(cpu->h, cpu->l)]; break;
        case 0x67: cpu->h = cpu->acc; break;
        case 0x68: cpu->l = cpu->b; break;
        case 0x69: cpu->l = cpu->c; break;
        case 0x6a: cpu->l = cpu->d; break;
        case 0x6b: cpu->l = cpu->e; break;
        case 0x6c: cpu->l = cpu->h; break;
        case 0x6d: cpu->l = cpu->l; break;
        case 0x6e: cpu->l = cpu->memory[make_addr(cpu->h, cpu->l)]; break;
        case 0x6f: cpu->l = cpu->acc; break;
        case 0x70: cpu->memory[make_addr(cpu->h, cpu->l)] = cpu->b; break;
        case 0x71: cpu->memory[make_addr(cpu->h, cpu->l)] = cpu->c; break;
        case 0x72: cpu->memory[make_addr(cpu->h, cpu->l)] = cpu->d; break;
        case 0x73: cpu->memory[make_addr(cpu->h, cpu->l)] = cpu->e; break;
        case 0x74: cpu->memory[make_addr(cpu->h, cpu->l)] = cpu->h; break;
        case 0x75: cpu->memory[make_addr(cpu->h, cpu->l)] = cpu->l; break;
        case 0x77: cpu->memory[make_addr(cpu->h, cpu->l)] = cpu->acc; break;
        case 0x78: cpu->acc = cpu->b; break;
        case 0x79: cpu->acc = cpu->c; break;
        case 0x7a: cpu->acc = cpu->d; break;
        case 0x7b: cpu->acc = cpu->e; break;
        case 0x7c: cpu->acc = cpu->h; break;
        case 0x7d: cpu->acc = cpu->l; break;
        case 0x7e: cpu->acc = cpu->memory[make_addr(cpu->h, cpu->l)]; break;
        case 0x7f: cpu->acc = cpu->acc; break;

        /* Mvi */
        case 0x06: cpu->b = next_byte(cpu); break;
        case 0x0e: cpu->c = next_byte(cpu); break;
        case 0x16: cpu->d = next_byte(cpu); break;
        case 0x1e: cpu->e = next_byte(cpu); break;
        case 0x26: cpu->h = next_byte(cpu); break;
        case 0x2e: cpu->l = next_byte(cpu); break;
        case 0x36:
            cpu->memory[make_addr(cpu->h, cpu->l)] = next_byte(cpu);
            break;
        case 0x3e:
            cpu->acc = next_byte(cpu);
            break;

            /* Add */
#define add(a, b)                                                              \
    do {                                                                       \
        u16 sum = cast(u16)((a) + (b));                                        \
        cpu->cy = (sum > 0xff);                                                \
        cpu->parity = has_parity(sum & 0xff);                                  \
        cpu->zero = (sum == 0x0);                                              \
        cpu->sign = ((sum & 0x80) == 0x80);                                    \
        cpu->acc = cast(u8)(sum & 0xff);                                       \
    } while (0);
#define adc(a, b) add(a, b + 1)

        case 0x80: add(cpu->acc, cpu->b) break; // ADD B
        case 0x81: add(cpu->acc, cpu->c) break; // ADD C
        case 0x82: add(cpu->acc, cpu->d) break; // ADD D
        case 0x83: add(cpu->acc, cpu->e) break; // ADD E
        case 0x84: add(cpu->acc, cpu->h) break; // ADD H
        case 0x85: add(cpu->acc, cpu->l) break; // ADD L
        case 0x86:
            add(cpu->acc,
                cpu->memory[make_addr(cpu->h, cpu->l)]) break; // ADD M
        case 0x87: add(cpu->acc, cpu->acc) break;              // ADD A
        case 0xc6: add(cpu->acc, next_byte(cpu)) break;        // ACI [d8]

        case 0x88: adc(cpu->acc, cpu->b) break; // ADC B
        case 0x89: adc(cpu->acc, cpu->c) break; // ADC C
        case 0x8a: adc(cpu->acc, cpu->d) break; // ADC D
        case 0x8b: adc(cpu->acc, cpu->e) break; // ADC E
        case 0x8c: adc(cpu->acc, cpu->h) break; // ADC H
        case 0x8d: adc(cpu->acc, cpu->l) break; // ADC L
        case 0x8e:
            adc(cpu->acc,
                cpu->memory[make_addr(cpu->h, cpu->l)]) break; // ADC M
        case 0x8f: adc(cpu->acc, cpu->acc) break;              // ADC A
        case 0xce:
            adc(cpu->acc, next_byte(cpu)) break; // ACI [d8]

            /* Sub */
#define sub(a, b)                                                              \
    do {                                                                       \
        u8 diff = cast(u8)((a) - (b));                                         \
        cpu->cy = ((a) < (b));                                                 \
        cpu->parity = has_parity(diff);                                        \
        cpu->zero = (diff == 0x0);                                             \
        cpu->sign = (diff == 0x80);                                            \
        cpu->acc = diff;                                                       \
    } while (0);
#define sbb(a, b) sub(a, b - 1)

        case 0x90: sub(cpu->acc, cpu->b) break; // SUB B
        case 0x91: sub(cpu->acc, cpu->c) break; // SUB C
        case 0x92: sub(cpu->acc, cpu->d) break; // SUB D
        case 0x93: sub(cpu->acc, cpu->e) break; // SUB E
        case 0x94: sub(cpu->acc, cpu->h) break; // SUB H
        case 0x95: sub(cpu->acc, cpu->l) break; // SUB L
        case 0x96:
            sub(cpu->acc,
                cpu->memory[make_addr(cpu->h, cpu->l)]) break; // SUB M
        case 0x97: sub(cpu->acc, cpu->acc) break;              // SUB A
        case 0xd6: sub(cpu->acc, next_byte(cpu)) break;        // SUI [d8]

        case 0x98: sbb(cpu->acc, cpu->b) break; // SUB A
        case 0x99: sbb(cpu->acc, cpu->c) break; // SUB A
        case 0x9a: sbb(cpu->acc, cpu->d) break; // SUB A
        case 0x9b: sbb(cpu->acc, cpu->e) break; // SUB A
        case 0x9c: sbb(cpu->acc, cpu->h) break; // SUB A
        case 0x9d: sbb(cpu->acc, cpu->l) break; // SUB A
        case 0x9e:
            sbb(cpu->acc,
                cpu->memory[make_addr(cpu->h, cpu->l)]) break; // SUB M
        case 0x9f: sbb(cpu->acc, cpu->acc) break;              // SUB A
        case 0xde:
            sbb(cpu->acc, next_byte(cpu)) break; // SBI [d8]

            /* And */
#define ana(a, b)                                                              \
    do {                                                                       \
        u8 res = cast(u8)((a) & (b));                                          \
        cpu->cy = 0;                                                           \
        cpu->parity = has_parity(res);                                         \
        cpu->zero = (res == 0x0);                                              \
        cpu->sign = (res == 0x80);                                             \
    } while (0);

        case 0xa0: ana(cpu->acc, cpu->b) break; // ANA B
        case 0xa1: ana(cpu->acc, cpu->c) break; // ANA C
        case 0xa2: ana(cpu->acc, cpu->d) break; // ANA D
        case 0xa3: ana(cpu->acc, cpu->e) break; // ANA E
        case 0xa4: ana(cpu->acc, cpu->h) break; // ANA H
        case 0xa5: ana(cpu->acc, cpu->l) break; // ANA L
        case 0xa6:
            ana(cpu->acc,
                cpu->memory[make_addr(cpu->h, cpu->l)]) break; // ANA M
        case 0xa7: ana(cpu->acc, cpu->acc) break;              // ANA A

        case 0xe6:
            ana(cpu->acc, next_byte(cpu)) break; // ANI [d8]

            /* Exclusive-OR */
#define xra(a, b)                                                              \
    do {                                                                       \
        u8 res = cast(u8)((a) ^ (b));                                          \
        cpu->cy = 0;                                                           \
        cpu->parity = has_parity(res);                                         \
        cpu->zero = (res == 0x0);                                              \
        cpu->sign = (res == 0x80);                                             \
    } while (0);

        case 0xa8: xra(cpu->acc, cpu->b) break; // XRA B
        case 0xa9: xra(cpu->acc, cpu->c) break; // XRA C
        case 0xaa: xra(cpu->acc, cpu->d) break; // XRA D
        case 0xab: xra(cpu->acc, cpu->e) break; // XRA E
        case 0xac: xra(cpu->acc, cpu->h) break; // XRA H
        case 0xad: xra(cpu->acc, cpu->l) break; // XRA L
        case 0xae:
            xra(cpu->acc,
                cpu->memory[make_addr(cpu->h, cpu->l)]) break; // XRA M
        case 0xaf: xra(cpu->acc, cpu->acc) break;              // XRA A

        case 0xee:
            xra(cpu->acc, next_byte(cpu)) break; // XRI [d8]

            /* Or */
#define ora(a, b)                                                              \
    do {                                                                       \
        u8 res = cast(u8)((a) | (b));                                          \
        cpu->cy = 0;                                                           \
        cpu->parity = has_parity(res);                                         \
        cpu->zero = (res == 0x0);                                              \
        cpu->sign = (res == 0x80);                                             \
    } while (0);

        case 0xb0: ora(cpu->acc, cpu->b) break; // ORA B
        case 0xb1: ora(cpu->acc, cpu->c) break; // ORA C
        case 0xb2: ora(cpu->acc, cpu->d) break; // ORA D
        case 0xb3: ora(cpu->acc, cpu->e) break; // ORA E
        case 0xb4: ora(cpu->acc, cpu->h) break; // ORA H
        case 0xb5: ora(cpu->acc, cpu->l) break; // ORA L
        case 0xb6:
            ora(cpu->acc,
                cpu->memory[make_addr(cpu->h, cpu->l)]) break; // ORA M
        case 0xb7: ora(cpu->acc, cpu->acc) break;              // ORA A
        case 0xf6:
            ora(cpu->acc, next_byte(cpu)) break; // ORI [d8]

            /* comparison */
#define cmp(a, b)                                                              \
    do {                                                                       \
        u8 diff = cast(u8)((a) - (b));                                         \
        cpu->cy = ((a) < (b));                                                 \
        cpu->parity = has_parity(diff);                                        \
        cpu->zero = (diff == 0x0);                                             \
        cpu->sign = (diff == 0x80);                                            \
    } while (0);

        case 0xb8: cmp(cpu->acc, cpu->b) break; // CMP B
        case 0xb9: cmp(cpu->acc, cpu->c) break; // CMP C
        case 0xba: cmp(cpu->acc, cpu->d) break; // CMP D
        case 0xbb: cmp(cpu->acc, cpu->e) break; // CMP E
        case 0xbc: cmp(cpu->acc, cpu->h) break; // CMP H
        case 0xbd: cmp(cpu->acc, cpu->l) break; // CMP L
        case 0xbe:
            cmp(cpu->acc,
                cpu->memory[make_addr(cpu->h, cpu->l)]) break; // CMP M
        case 0xbf: cmp(cpu->acc, cpu->acc) break;              // CMP A
        case 0xfe:
            cmp(cpu->acc, next_byte(cpu)) break; // CPI [d8]

        /* Set/Reset Carry */
        case 0x3f: cpu->cy = !cpu->cy; break; // CMC
        case 0x37:
            cpu->cy = 0x1;
            break; // STC

        /* Rotation */
        case 0x07: { // RLC
            u8 msb = (cpu->acc >> 7) & 1;
            cpu->acc = (cpu->acc << 1) | msb;
            cpu->cy = msb;
        } break;

        case 0x0f: { // RRC
            u8 lsb = cpu->acc & 1;
            cpu->acc = (cpu->acc >> 1) | (lsb << 7);
            cpu->cy = lsb;
        } break;

        case 0x17: { // RAL
            u8 msb = (cpu->acc >> 7) & 1;
            cpu->acc = (cpu->acc << 1) | cpu->cy;
            cpu->cy = msb;
        } break;

        case 0x1f: { // RAR
            u8 lsb = cpu->acc & 1;
            cpu->acc = (cpu->acc >> 1) | cpu->cy;
            cpu->cy = lsb;
        } break;

        /* Jump */
#define jump(If)                                                               \
    do {                                                                       \
        if (If)                                                                \
            cpu->pc = next_address(cpu);                                       \
        else                                                                   \
            cpu->pc += 2;                                                      \
    } while (0);

        case 0xca: jump(cpu->zero == 0x1); break;   // JZ [Addr]
        case 0xc2: jump(cpu->zero == 0x0); break;   // JNZ [Addr]
        case 0xc3: jump(1); break;                  // JMP [Addr]
        case 0xda: jump(cpu->cy == 0x1); break;     // JC [Addr]
        case 0xd2: jump(cpu->cy == 0x0); break;     // JNC [Addr]
        case 0xea: jump(cpu->parity == 0x1); break; // JPE [Addr]
        case 0xe2: jump(cpu->parity == 0x0); break; // JPO [Addr]
        case 0xf2: jump(cpu->zero == 0x0); break;   // JP [Addr]
        case 0xfa:
            jump(cpu->sign == 0x1);
            break; // JM [Addr]

            /* Call */
#define call(If)                                                               \
    do {                                                                       \
        if (If) {                                                              \
            u16 return_address = cpu->pc + 3;                                  \
            cpu->memory[cpu->sp - 1] =                                         \
                cast(u8)((return_address & 0xff00) >> 8);                      \
            cpu->memory[cpu->sp - 2] = cast(u8)(return_address & 0x00ff);      \
            cpu->sp -= 2;                                                      \
            cpu->pc = next_address(cpu);                                       \
        } else {                                                               \
            cpu->pc += 2;                                                      \
        }                                                                      \
    } while (0);

        case 0xcd: call(1); break;                  // CALL [Addr]
        case 0xcc: call(cpu->zero == 0x1); break;   // CZ [Addr]
        case 0xc4: call(cpu->zero == 0x0); break;   // CNZ [Addr]
        case 0xdc: call(cpu->cy == 0x1); break;     // CC [Addr]
        case 0xd4: call(cpu->cy == 0x0); break;     // CNC [Addr]
        case 0xf4: call(cpu->sign == 0x0); break;   // CP [Addr]
        case 0xfc: call(cpu->sign == 0x1); break;   // CP [Addr]
        case 0xe4: call(cpu->parity == 0x0); break; // CPO [Addr]
        case 0xec:
            call(cpu->parity == 0x1);
            break; // CPE [Addr]

            /* Return */
#define ret(If)                                                                \
    do {                                                                       \
        if (If) {                                                              \
            cpu->pc = (cast(u16) cpu->memory[cpu->sp + 1] << 8) |              \
                      cpu->memory[cpu->sp];                                    \
            cpu->sp += 2;                                                      \
        }                                                                      \
    } while (0);

        case 0xc9: ret(1); break;                  // RET
        case 0xc8: ret(cpu->zero == 0x1); break;   // RZ
        case 0xc0: ret(cpu->zero == 0x0); break;   // RNZ
        case 0xd8: ret(cpu->cy == 0x1); break;     // RR
        case 0xd0: ret(cpu->cy == 0x0); break;     // RNR
        case 0xf8: ret(cpu->sign == 0x1); break;   // RP
        case 0xf0: ret(cpu->sign == 0x0); break;   // RP
        case 0xe8: ret(cpu->parity == 0x1); break; // RPE
        case 0xe0: ret(cpu->parity == 0x0); break; // RPO

        case 0xe9:
            cpu->pc = make_addr(cpu->h, cpu->l);
            break; // PCHL

        /* RST [0-7] */
        case 0xc7: todo();

        /* Halt */
        case 0x76:
            cpu->halt = 0x1;
            break;

            /* Stack push */
#define push(a, b)                                                             \
    do {                                                                       \
        cpu->memory[cpu->sp - 1] = (a);                                        \
        cpu->memory[cpu->sp - 2] = (b);                                        \
        cpu->sp -= 2;                                                          \
    } while (0);

        case 0xf5: push(cpu->acc, flag_register(cpu)); break; // PUSH PSW
        case 0xc5: push(cpu->b, cpu->c); break;               // PUSH B
        case 0xd5: push(cpu->d, cpu->e); break;               // PUSH D
        case 0xe5:
            push(cpu->h, cpu->l);
            break; // PUSH H

            /* stack pop */
#define pop(a, b)                                                              \
    do {                                                                       \
        b = cpu->memory[cpu->sp];                                              \
        a = cpu->memory[cpu->sp + 1];                                          \
        cpu->sp += 2;                                                          \
    } while (0);

        case 0xf1: { // POP PSW
            u8 flags = cpu->memory[cpu->sp];
            cpu->cy = flags & 1;
            cpu->parity = (flags >> 2) & 1;
            cpu->auxillary_cy = (flags >> 4) & 1;
            cpu->zero = (flags >> 6) & 1;
            cpu->sign = (flags >> 7) & 1;

            cpu->acc = cpu->memory[cpu->sp + 1];
            cpu->sp += 2;
        } break;

        case 0xc1: pop(cpu->b, cpu->c); break; // POP B
        case 0xd1: pop(cpu->d, cpu->e); break; // POP D
        case 0xe1: pop(cpu->h, cpu->l); break; // POP H

        case 0xf9: cpu->sp = make_addr(cpu->h, cpu->l); break; // SPHL
        case 0xe3: {                                           // XTHL
            swap(cpu->memory[cpu->sp], cpu->l, u8);
            swap(cpu->memory[cpu->sp - 1], cpu->h, u8);
        } break;

        default: unreachable();

#undef make_addr
#undef increment
#undef inr
#undef dcr
#undef inxcrement
#undef inx
#undef dcx
#undef add
#undef adc
#undef sub
#undef sbb
#undef ana
#undef xra
#undef ora
#undef cmp
#undef jump
#undef call
#undef ret
#undef push
#undef pop
        }

        cpu->pc += 1;
    }
}

// --------------------------------------------------------------------------
//
//
//                          - Argument Parser -
//
//
// --------------------------------------------------------------------------

typedef enum : u8 {
    ArgumentKind_Optional = 1 << 1,
    ArgumentKind_Required = 1 << 2,

    ArgumentKind_Flag = 1 << 3, /* can be On or Off */
    ArgumentKind_Atom = 1 << 4, /* single value after argument */
    ArgumentKind_Command = 1 << 5,
} ArgumentKind;

typedef struct Argument Argument;
struct Argument {
    char *arg;
    char *arg_second; /* We can match atleast two args, one could be short other
                         long */
    char *value;
    char *description;

    ArgumentKind kind;
    Argument **command_args;
};

#define arg_has_value(a) a.value

Argument Arg(char *arg, char *arg_second, ArgumentKind kind,
             char *description) {
    assert(arg);
    assert(description);

    return (Argument){
        .arg = arg,
        .arg_second = arg_second,
        .value = NULL,
        .description = description,
        .kind = kind,
    };
}

Argument ArgCommand(char *arg, char *arg_second, ArgumentKind kind,
                    Argument **command_args, char *description) {
    assert(arg);
    assert((kind & ArgumentKind_Command) == ArgumentKind_Command &&
           "expected a command kind");
    assert(description);

    return (Argument){
        .arg = arg,
        .arg_second = arg_second,
        .value = NULL,
        .description = description,
        .kind = kind,
        .command_args = command_args,
    };
}

Argument *match_arg(char *arg_cstr, Argument **args) {
    for (u8 i = 0; i <= cast(u8) array_sizeof(args, Argument *); ++i) {
        if (match_string(arg_cstr, args[i]->arg, strlen(args[i]->arg)))
            return args[i];
        if (args[i]->arg_second && match_string(arg_cstr, args[i]->arg_second,
                                                strlen(args[i]->arg_second)))
            return args[i];
    }
    return NULL;
}

/* Assume the command args are at the top */
void help_message(FILE *stream, Argument **args) {
    for (u8 i = 0; i <= cast(u8) array_sizeof(args, Argument *); ++i) {
        fprintf(stream, "  %s", args[i]->arg);
        if (args[i]->arg_second) {
            fprintf(stream, "| %s", args[i]->arg_second);
        }

        switch (args[i]->kind &
                ~(ArgumentKind_Required | ArgumentKind_Optional)) {
        case ArgumentKind_Atom: fprintf(stream, " [value]"); break;

        case ArgumentKind_Command: fprintf(stream, " [command]"); break;

        case ArgumentKind_Flag: fprintf(stream, " [flag]"); break;

        default: unreachable();
        }

        fprintf(stream, ": %s ", args[i]->description);
        if ((args[i]->kind & ArgumentKind_Required) == ArgumentKind_Required) {
            fprintf(stream, "(required)");
        }
        fprintf(stream, "\n");
    }
    fprintf(stream, "  --help| -h: show help message\n");
}

int parse_args(int argc, char **argv, Argument **args) {
    assert(array_sizeof(args, Argument *) > 0);

    Argument *arg;
    int i = 1;

    while (i < argc) {
        /* Default help flag */
        if (match_string("-h", argv[i], strlen(argv[i])) ||
            match_string("--help", argv[i], strlen(argv[i]))) {
            help_message(stdout, args);
            exit(0);
        }

        arg = match_arg(argv[i], args);
        if (!arg) {
            fprintf(stderr, "Error: invaild '%s'\n", argv[i]);
            exit(1);
        }

        /* Remove attributes, so we can directly match on flag, atom etc */
        switch (arg->kind & ~(ArgumentKind_Required | ArgumentKind_Optional)) {
        case ArgumentKind_Flag: arg->value = argv[i]; break;

        case ArgumentKind_Atom:
            if (i + 1 >= argc) {
                printf("Error: Expected a value after '%s'\n", argv[i]);
                exit(1);
            }
            arg->value = argv[i + 1];
            i += 1;
            break;

        case ArgumentKind_Command:
            arg->value = argv[i];
            i += parse_args(argc - i, &argv[i], arg->command_args);
            break;

        default: unreachable();
        }

        i += 1;
    }

    /* Check if requirements are satisfied */
    for (u8 j = 0; j <= cast(u8) array_sizeof(args, Argument *); ++j) {
        if ((args[j]->kind & ArgumentKind_Required) == ArgumentKind_Required &&
            !args[j]->value) {
            fprintf(stderr, "Error: Missing required '%s", args[j]->arg);
            if (args[j]->arg_second) {
                fprintf(stderr, "| %s'\n", args[j]->arg_second);
            }
            exit(1);
        }
    }

    return i;
}

// --------------------------------------------------------------------------
//                          - Main -
// --------------------------------------------------------------------------

int main(int argc, char **argv) {
    /* NOTE(madflash) - Kind which is not ArgumentKind_Required is optional */

    /* Emulator command */
    Argument cmd_emulator_binary =
        Arg("--binary", "-b", ArgumentKind_Atom | ArgumentKind_Required,
            "binary file to emulate");
    Argument cmd_emulator_start_addr =
        Arg("--start-addr", "-a", ArgumentKind_Atom, "starting address");
    Argument *cmd_emulator_subcmds[] = {&cmd_emulator_binary,
                                        &cmd_emulator_start_addr};
    Argument cmd_emulator = ArgCommand("emu", NULL, ArgumentKind_Command,
                                       cmd_emulator_subcmds, "run emulator");

    /* Assemblar command */
    Argument cmd_assemblar_source_file =
        Arg("--source", "-s", ArgumentKind_Atom | ArgumentKind_Required,
            "source assembly file");
    /*
     * Phases
     * 0 lexical analysis
     * 1 dumping bytecode
     */
    Argument cmd_assemblar_phases =
        Arg("--phase", "-p", ArgumentKind_Atom, "run phases");
    Argument *cmd_assemblar_subcmds[] = {&cmd_assemblar_source_file,
                                         &cmd_assemblar_phases};
    Argument cmd_assemblar = ArgCommand("asm", NULL, ArgumentKind_Command,
                                        cmd_assemblar_subcmds, "run assemblar");

    /* Command args should be at the top */
    Argument *args[] = {
        &cmd_emulator,
        &cmd_assemblar,
    };
    parse_args(argc, argv, args);

    if (arg_has_value(cmd_emulator)) {
        todo();
    }

    // if (arg_has_value(cmd_assemblar)) {
    //     char *source_file_path = cmd_assemblar_source_file.value;

    //     if (arg_has_value(cmd_assemblar_phases)) {
    //         switch (*cmd_assemblar_phases.value) {
    //         case '0': {
    //             char *src = file_to_string(source_file_path);
    //             Lexer *l = make_lexer(src);
    //             Array(TOKEN) tokens = slurp_tokens(l);

    //             array_for_each(tokens, i) {
    //                 print_token(&tokens[i]);
    //                 printf("\n");
    //             }

    //             free(src);
    //             free(l);
    //         } break;

    //         case '1': {
    //             char *src = file_to_string(source_file_path);
    //             emit_binary_from_program(src);
    //             free(src);
    //         } break;

    //         default: {
    //             fprintf(stderr,
    //                     "Error: invaild phase supplied, know are 0, 1\n");
    //             exit(1);
    //         }
    //         }
    //     }
    // }

    return 0;
}