typedef struct Cpu Cpu;
struct Cpu {
    u8 memory[0xffff];

    u16 pc;
    u16 sp;

    u8 accmulator;
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

Cpu *make_cpu(u8 *program, u16 program_size, u16 starting_address) {
    Debug_Assert_Message(program_size <= 0xffff, "Program size should less than 0xffff");
    Debug_Assert_Message(starting_address >= 0x0 && starting_address <= 0xffff,
                         "starting address is not between 0x0 and 0xffff");

    Cpu *cpu = xmalloc(sizeof(Cpu));

    cpu->pc = starting_address;
    cpu->sp = 0x0;

    cpu->accmulator = 0x0;
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

    memcpy(cpu->memory + starting_address, program, program_size);
    return cpu;
}

void free_cpu(Cpu *cpu) {
    free(cpu);
}

/* [Sign] [Zero] [X] [AC] [X] [Parity] [X] [Carry] */
u8 cpu_get_flag_reg(Cpu *cpu) {
    return (cpu->sign << 7) | (cpu->zero << 6) | (cpu->auxillary_cy << 4) | (cpu->parity << 2) |
           cpu->cy;
}

void cpu_set_flag_reg(Cpu *cpu, u8 flags) {
    cpu->cy = flags & 1;
    cpu->parity = (flags >> 2) & 1;
    cpu->auxillary_cy = (flags >> 4) & 1;
    cpu->zero = (flags >> 6) & 1;
    cpu->sign = (flags >> 7) & 1;
}

static u8 has_parity(u8 n) {
    /* Brian kernighan's algorithm to count set bits */
    u8 count = 0;

    while (n) {
        n &= (n - 1);
        count += 1;
    }

    return !(count & 0x1);
}

u8 cpu_mem_access(Cpu *cpu, u16 addr) {
    Assert_Message(addr >= 0x0 && addr <= 0xffff, "Invaild memory access");
    return cpu->memory[addr];
}

void cpu_mem_set(Cpu *cpu, u16 addr, u8 value) {
    Assert_Message(addr >= 0x0 && addr <= 0xffff, "Invaild memory access");
    cpu->memory[addr] = value;
}

u16 u16_from_hi_lo_byte(u8 a, u8 b) {
    return cast(u16)(0x0000 & (a)) | (cast(u16)(b) << 8);
}

u16 cpu_reg_pair_hl(Cpu *cpu) {
    return u16_from_hi_lo_byte(cpu->h, cpu->l);
}
u16 cpu_reg_pair_bc(Cpu *cpu) {
    return u16_from_hi_lo_byte(cpu->b, cpu->c);
}
u16 cpu_reg_pair_de(Cpu *cpu) {
    return u16_from_hi_lo_byte(cpu->d, cpu->e);
}

void cpu_set_reg_pair_hl(Cpu *cpu, u16 hl) {
    cpu->h = cast(u8)((hl & 0xff00) >> 8);
    cpu->l = cast(u8)((hl & 0x00ff) >> 8);
}

u8 cpu_fetch_next_byte(Cpu *cpu) {
    cpu->pc += 1;
    return cpu_mem_access(cpu, cpu->pc);
}

u16 cpu_fetch_next_address(Cpu *cpu) {
    return (cast(u16)(cpu_mem_access(cpu, cpu->pc + 2) << 8)) |
           cast(u16) cpu_mem_access(cpu, cpu->pc + 1);
}

void cpu_jump_to_address(Cpu *cpu, u16 target_address) {
    cpu_mem_set(cpu, cpu_mem_access(cpu, cpu->sp - 1), cast(u8)((cpu->pc & 0xff00) >> 8));
    cpu_mem_set(cpu, cpu_mem_access(cpu, cpu->sp - 2), cast(u8)(cpu->pc & 0x00ff));
    cpu->sp -= 2;
    cpu->pc = target_address;
}

void cpu_swap_memory_with_val(Cpu *cpu, u16 addr, u8 *value) {
    u8 temp = cpu_mem_access(cpu, addr);
    cpu_mem_set(cpu, addr, *value);
    *value = temp;
}

// --------------------------------------------------------------------------
//                          - Instructions -
// --------------------------------------------------------------------------
#define inr(a) increment(a, 0x1)
#define dcr(a) increment(a, -0x1)
#define increment(a, by)                                                                           \
    do {                                                                                           \
        u8 sum = cast(u8)(a + by);                                                                 \
        cpu->parity = has_parity(sum);                                                             \
        cpu->zero = (sum == 0x0);                                                                  \
        cpu->sign = ((sum & 0x80) == 0x80);                                                        \
        a = sum;                                                                                   \
    } while (0);

#define inx(a, b) inxcrement(a, b, 0x1)
#define dcx(a, b) inxcrement(a, b, -0x1)
#define inxcrement(a, b, by)                                                                       \
    do {                                                                                           \
        u16 res = u16_from_hi_lo_byte(a, b) + by;                                                  \
        a = cast(u8)((res & 0xff00) >> 8);                                                         \
        b = cast(u8)(res & 0xff);                                                                  \
    } while (0);

#define adc(a, b) add(a, b + cpu->cy)
#define add(a, b)                                                                                  \
    do {                                                                                           \
        u16 sum = cast(u16)((a) + (b));                                                            \
        cpu->cy = (sum > 0xff);                                                                    \
        cpu->parity = has_parity(sum & 0xff);                                                      \
        cpu->zero = (sum == 0x0);                                                                  \
        cpu->sign = ((sum & 0x80) == 0x80);                                                        \
        cpu->accmulator = cast(u8)(sum & 0xff);                                                    \
    } while (0);

#define sbb(a, b) sub(a, b - cpu->cy)
#define sub(a, b)                                                                                  \
    do {                                                                                           \
        u8 diff = cast(u8)((a) - (b));                                                             \
        cpu->cy = ((a) < (b));                                                                     \
        cpu->parity = has_parity(diff);                                                            \
        cpu->zero = (diff == 0x0);                                                                 \
        cpu->sign = (diff == 0x80);                                                                \
        cpu->accmulator = diff;                                                                    \
    } while (0);

#define ana(a, b)                                                                                  \
    do {                                                                                           \
        u8 res = cast(u8)((a) & (b));                                                              \
        cpu->cy = 0;                                                                               \
        cpu->parity = has_parity(res);                                                             \
        cpu->zero = (res == 0x0);                                                                  \
        cpu->sign = (res == 0x80);                                                                 \
    } while (0);

#define xra(a, b)                                                                                  \
    do {                                                                                           \
        u8 res = cast(u8)((a) ^ (b));                                                              \
        cpu->cy = 0;                                                                               \
        cpu->parity = has_parity(res);                                                             \
        cpu->zero = (res == 0x0);                                                                  \
        cpu->sign = (res == 0x80);                                                                 \
    } while (0);

#define ora(a, b)                                                                                  \
    do {                                                                                           \
        u8 res = cast(u8)((a) | (b));                                                              \
        cpu->cy = 0;                                                                               \
        cpu->parity = has_parity(res);                                                             \
        cpu->zero = (res == 0x0);                                                                  \
        cpu->sign = (res == 0x80);                                                                 \
    } while (0);

#define cmp(a, b)                                                                                  \
    do {                                                                                           \
        u8 diff = cast(u8)((a) - (b));                                                             \
        cpu->cy = ((a) < (b));                                                                     \
        cpu->parity = has_parity(diff);                                                            \
        cpu->zero = (diff == 0x0);                                                                 \
        cpu->sign = (diff == 0x80);                                                                \
    } while (0);

#define jump(If)                                                                                   \
    do {                                                                                           \
        if (If) {                                                                                  \
            cpu->pc = cpu_fetch_next_address(cpu);                                                 \
        } else {                                                                                   \
            cpu->pc += 2;                                                                          \
        }                                                                                          \
    } while (0);

#define call(If)                                                                                   \
    do {                                                                                           \
        if (If) {                                                                                  \
            cpu_jump_to_address(cpu, cpu_fetch_next_address(cpu));                                 \
        } else {                                                                                   \
            cpu->pc += 2;                                                                          \
        }                                                                                          \
    } while (0);

#define ret(If)                                                                                    \
    do {                                                                                           \
        if (If) {                                                                                  \
            cpu->pc =                                                                              \
                (cast(u16) cpu_mem_access(cpu, cpu->sp + 1) << 8) | cpu_mem_access(cpu, cpu->sp);  \
            cpu->sp += 2;                                                                          \
        }                                                                                          \
    } while (0);

#define push(a, b)                                                                                 \
    do {                                                                                           \
        cpu_mem_set(cpu, cpu->sp - 1, a);                                                          \
        cpu_mem_set(cpu, cpu->sp - 2, b);                                                          \
        cpu->sp -= 2;                                                                              \
    } while (0);

#define pop(a, b)                                                                                  \
    do {                                                                                           \
        b = cpu_mem_access(cpu, cpu->sp);                                                          \
        a = cpu_mem_access(cpu, cpu->sp + 1);                                                      \
        cpu->sp += 2;                                                                              \
    } while (0);

#define dad(regp)                                                                                  \
    do {                                                                                           \
        u32 sum = cast(u32) cpu_reg_pair_hl(cpu) + regp;                                           \
        cpu->cy = ((sum & 0x10000) == 0x10000);                                                    \
    } while (0);

#define lxi(a, b)                                                                                  \
    do {                                                                                           \
        a = cpu_mem_access(cpu, cpu->pc + 2);                                                      \
        b = cpu_mem_access(cpu, cpu->pc + 1);                                                      \
        cpu->pc += 2;                                                                              \
    } while (0);

void cpu_execute(Cpu *cpu) {
    while (cpu->pc < 0xffff) {
        u8 opcode = cpu_mem_access(cpu, cpu->pc);
        debug_println("debug: opcode: 0x%x", opcode);

        switch (opcode) {
        /* Empty instructions */
        case 0x08:
        case 0x10:
        case 0x18:
        case 0x20:
        case 0x28:
        case 0x30:
        case 0x38:
        case 0xcb:
        case 0xd9:
        case 0xdd:
        case 0xed:
        case 0xfd: break;

        case 0x00: break; // NOP
        case 0x76:
            cpu->halt = 0x1;
            break; // HALT

        /* lxi */
        case 0x01: lxi(cpu->b, cpu->c); break; // LXI B
        case 0x11: lxi(cpu->d, cpu->e); break; // LXI D
        case 0x21: lxi(cpu->h, cpu->l); break; // LXI H
        case 0x31:
            cpu->sp = u16_from_hi_lo_byte(cpu_mem_access(cpu, cpu->pc + 2),
                                          cpu_mem_access(cpu, cpu->pc + 1));
            break; // LXI SP

        /* lda */
        case 0x3a:
            cpu->accmulator = cpu_fetch_next_address(cpu);
            cpu += 2;
            break;

        /* sta */
        case 0x32: {
            u16 addr = cpu_fetch_next_address(cpu);
            cpu_mem_set(cpu, addr, cpu->accmulator);
            cpu->pc += 2;
        } break;

        /* ldax */
        case 0x0a: cpu->accmulator = cpu_mem_access(cpu, cpu_reg_pair_bc(cpu)); break; // LDAX B
        case 0x1a:
            cpu->accmulator = cpu_mem_access(cpu, cpu_reg_pair_de(cpu));
            break; // LDAX D

        /* stax */
        case 0x02: cpu_mem_set(cpu, cpu_reg_pair_bc(cpu), cpu->accmulator); break; // STAX B
        case 0x12:
            cpu_mem_set(cpu, cpu_reg_pair_de(cpu), cpu->accmulator);
            break; // STAX D

        /* lhld */
        case 0x2a: {
            u16 addr = cpu_fetch_next_address(cpu);
            cpu->h = cpu_mem_access(cpu, addr + 1);
            cpu->l = cpu_mem_access(cpu, addr);
            cpu->pc += 2;
        } break;

        /* shld */
        case 0x22: {
            u16 addr = cpu_fetch_next_address(cpu);
            cpu_mem_set(cpu, cpu_mem_access(cpu, addr + 1), cpu->h);
            cpu_mem_set(cpu, cpu_mem_access(cpu, addr), cpu->l);
            cpu->pc += 2;
        } break;

        /* xchg */
        case 0xeb:
            swap(cpu->h, cpu->d, u8);
            swap(cpu->l, cpu->e, u8);
            break;

        /* cma */
        case 0x2f: cpu->accmulator = !cpu->accmulator; break;

        /* inr */
        case 0x04: inr(cpu->b); break;                            // INR B
        case 0x0c: inr(cpu->c); break;                            // INR C
        case 0x14: inr(cpu->d); break;                            // INR D
        case 0x1c: inr(cpu->e); break;                            // INR E
        case 0x24: inr(cpu->h); break;                            // INR H
        case 0x2c: inr(cpu->l); break;                            // INR L
        case 0x34: inr(cpu->memory[cpu_reg_pair_hl(cpu)]); break; // INR M
        case 0x3c:
            inr(cpu->accmulator);
            break; // INR A

        /* dcr */
        case 0x05: dcr(cpu->b); break;                            // DCR B
        case 0x0d: dcr(cpu->c); break;                            // DCR C
        case 0x15: dcr(cpu->d); break;                            // DCR D
        case 0x1d: dcr(cpu->e); break;                            // DCR E
        case 0x25: dcr(cpu->h); break;                            // DCR H
        case 0x2d: dcr(cpu->l); break;                            // DCR L
        case 0x35: dcr(cpu->memory[cpu_reg_pair_hl(cpu)]); break; // DCR M
        case 0x3d:
            dcr(cpu->accmulator);
            break; // DCR A

        /* inx */
        case 0x03: inx(cpu->b, cpu->c); break;
        case 0x13: inx(cpu->d, cpu->e); break;
        case 0x23: inx(cpu->h, cpu->l); break;
        case 0x33: cpu->sp = cpu->sp + 0x1; break;

        /* dcx */
        case 0x0b: dcx(cpu->b, cpu->c); break;
        case 0x1b: dcx(cpu->d, cpu->e); break;
        case 0x2b: dcx(cpu->h, cpu->l); break;
        case 0x3b: cpu->sp = cast(u16)(cpu->sp - 0x1); break;

        /* mov instructions */
        case 0x40: cpu->b = cpu->b; break;
        case 0x41: cpu->b = cpu->c; break;
        case 0x42: cpu->b = cpu->d; break;
        case 0x43: cpu->b = cpu->e; break;
        case 0x44: cpu->b = cpu->h; break;
        case 0x45: cpu->b = cpu->l; break;
        case 0x46: cpu->b = cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)); break;
        case 0x47: cpu->b = cpu->accmulator; break;
        case 0x48: cpu->c = cpu->b; break;
        case 0x49: cpu->c = cpu->c; break;
        case 0x4a: cpu->c = cpu->d; break;
        case 0x4b: cpu->c = cpu->e; break;
        case 0x4c: cpu->c = cpu->h; break;
        case 0x4d: cpu->c = cpu->l; break;
        case 0x4e: cpu->c = cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)); break;
        case 0x4f: cpu->c = cpu->accmulator; break;
        case 0x50: cpu->d = cpu->b; break;
        case 0x51: cpu->d = cpu->c; break;
        case 0x52: cpu->d = cpu->d; break;
        case 0x53: cpu->d = cpu->e; break;
        case 0x54: cpu->d = cpu->h; break;
        case 0x55: cpu->d = cpu->l; break;
        case 0x56: cpu->d = cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)); break;
        case 0x57: cpu->d = cpu->accmulator; break;
        case 0x58: cpu->d = cpu->b; break;
        case 0x59: cpu->d = cpu->c; break;
        case 0x5a: cpu->b = cpu->d; break;
        case 0x5b: cpu->b = cpu->e; break;
        case 0x5c: cpu->b = cpu->h; break;
        case 0x5d: cpu->b = cpu->l; break;
        case 0x5e: cpu->b = cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)); break;
        case 0x5f: cpu->b = cpu->accmulator; break;
        case 0x60: cpu->h = cpu->b; break;
        case 0x61: cpu->h = cpu->c; break;
        case 0x62: cpu->h = cpu->d; break;
        case 0x63: cpu->h = cpu->e; break;
        case 0x64: cpu->h = cpu->h; break;
        case 0x65: cpu->h = cpu->l; break;
        case 0x66: cpu->h = cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)); break;
        case 0x67: cpu->h = cpu->accmulator; break;
        case 0x68: cpu->l = cpu->b; break;
        case 0x69: cpu->l = cpu->c; break;
        case 0x6a: cpu->l = cpu->d; break;
        case 0x6b: cpu->l = cpu->e; break;
        case 0x6c: cpu->l = cpu->h; break;
        case 0x6d: cpu->l = cpu->l; break;
        case 0x6e: cpu->l = cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)); break;
        case 0x6f: cpu->l = cpu->accmulator; break;
        case 0x70: cpu_mem_set(cpu, cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)), cpu->b); break;
        case 0x71: cpu_mem_set(cpu, cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)), cpu->c); break;
        case 0x72: cpu_mem_set(cpu, cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)), cpu->d); break;
        case 0x73: cpu_mem_set(cpu, cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)), cpu->e); break;
        case 0x74: cpu_mem_set(cpu, cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)), cpu->h); break;
        case 0x75: cpu_mem_set(cpu, cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)), cpu->l); break;
        case 0x77:
            cpu_mem_set(cpu, cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)), cpu->accmulator);
            break;
        case 0x78: cpu->accmulator = cpu->b; break;
        case 0x79: cpu->accmulator = cpu->c; break;
        case 0x7a: cpu->accmulator = cpu->d; break;
        case 0x7b: cpu->accmulator = cpu->e; break;
        case 0x7c: cpu->accmulator = cpu->h; break;
        case 0x7d: cpu->accmulator = cpu->l; break;
        case 0x7e: cpu->accmulator = cpu_mem_access(cpu, cpu_reg_pair_hl(cpu)); break;
        case 0x7f: cpu->accmulator = cpu->accmulator; break;

        /* mvi instructions */
        case 0x06: cpu->b = cpu_fetch_next_byte(cpu); break;
        case 0x0e: cpu->c = cpu_fetch_next_byte(cpu); break;
        case 0x16: cpu->d = cpu_fetch_next_byte(cpu); break;
        case 0x1e: cpu->e = cpu_fetch_next_byte(cpu); break;
        case 0x26: cpu->h = cpu_fetch_next_byte(cpu); break;
        case 0x2e: cpu->l = cpu_fetch_next_byte(cpu); break;
        case 0x36: cpu->memory[cpu_reg_pair_hl(cpu)] = cpu_fetch_next_byte(cpu); break;
        case 0x3e: cpu->accmulator = cpu_fetch_next_byte(cpu); break;

        /* add instructions */
        case 0x80: add(cpu->accmulator, cpu->b) break; // ADD B
        case 0x81: add(cpu->accmulator, cpu->c) break; // ADD C
        case 0x82: add(cpu->accmulator, cpu->d) break; // ADD D
        case 0x83: add(cpu->accmulator, cpu->e) break; // ADD E
        case 0x84: add(cpu->accmulator, cpu->h) break; // ADD H
        case 0x85: add(cpu->accmulator, cpu->l) break; // ADD L
        case 0x86:
            add(cpu->accmulator,
                cpu_mem_access(cpu, u16_from_hi_lo_byte(cpu->h, cpu->l))) break; // ADD M
        case 0x87: add(cpu->accmulator, cpu->accmulator) break;                  // ADD A
        case 0xc6: add(cpu->accmulator, cpu_fetch_next_byte(cpu)) break;         // ADI [d8]

        case 0x88: adc(cpu->accmulator, cpu->b) break; // ADC B
        case 0x89: adc(cpu->accmulator, cpu->c) break; // ADC C
        case 0x8a: adc(cpu->accmulator, cpu->d) break; // ADC D
        case 0x8b: adc(cpu->accmulator, cpu->e) break; // ADC E
        case 0x8c: adc(cpu->accmulator, cpu->h) break; // ADC H
        case 0x8d: adc(cpu->accmulator, cpu->l) break; // ADC L
        case 0x8e:
            adc(cpu->accmulator,
                cpu_mem_access(cpu, u16_from_hi_lo_byte(cpu->h, cpu->l))) break; // ADC M
        case 0x8f: adc(cpu->accmulator, cpu->accmulator) break;                  // ADC A
        case 0xce:
            adc(cpu->accmulator, cpu_fetch_next_byte(cpu)) break; // ACI [d8]

        /* sub instructions */
        case 0x90: sub(cpu->accmulator, cpu->b) break; // SUB B
        case 0x91: sub(cpu->accmulator, cpu->c) break; // SUB C
        case 0x92: sub(cpu->accmulator, cpu->d) break; // SUB D
        case 0x93: sub(cpu->accmulator, cpu->e) break; // SUB E
        case 0x94: sub(cpu->accmulator, cpu->h) break; // SUB H
        case 0x95: sub(cpu->accmulator, cpu->l) break; // SUB L
        case 0x96:
            sub(cpu->accmulator,
                cpu_mem_access(cpu, u16_from_hi_lo_byte(cpu->h, cpu->l))) break; // SUB M
        case 0x97: sub(cpu->accmulator, cpu->accmulator) break;                  // SUB A
        case 0xd6: sub(cpu->accmulator, cpu_fetch_next_byte(cpu)) break;         // SUI [d8]

        case 0x98: sbb(cpu->accmulator, cpu->b) break; // SBB B
        case 0x99: sbb(cpu->accmulator, cpu->c) break; // SBB C
        case 0x9a: sbb(cpu->accmulator, cpu->d) break; // SBB D
        case 0x9b: sbb(cpu->accmulator, cpu->e) break; // SBB E
        case 0x9c: sbb(cpu->accmulator, cpu->h) break; // SBB H
        case 0x9d: sbb(cpu->accmulator, cpu->l) break; // SBB L
        case 0x9e:
            sbb(cpu->accmulator,
                cpu_mem_access(cpu, u16_from_hi_lo_byte(cpu->h, cpu->l))) break; // SUB M
        case 0x9f: sbb(cpu->accmulator, cpu->accmulator) break;                  // SUB A
        case 0xde:
            sbb(cpu->accmulator, cpu_fetch_next_byte(cpu)) break; // SBI [d8]

        /* ana instructions */
        case 0xa0: ana(cpu->accmulator, cpu->b) break; // ANA B
        case 0xa1: ana(cpu->accmulator, cpu->c) break; // ANA C
        case 0xa2: ana(cpu->accmulator, cpu->d) break; // ANA D
        case 0xa3: ana(cpu->accmulator, cpu->e) break; // ANA E
        case 0xa4: ana(cpu->accmulator, cpu->h) break; // ANA H
        case 0xa5: ana(cpu->accmulator, cpu->l) break; // ANA L
        case 0xa6:
            ana(cpu->accmulator,
                cpu_mem_access(cpu, u16_from_hi_lo_byte(cpu->h, cpu->l))) break; // ANA M
        case 0xa7: ana(cpu->accmulator, cpu->accmulator) break;                  // ANA A

        case 0xe6:
            ana(cpu->accmulator, cpu_fetch_next_byte(cpu)) break; // ANI [d8]

        /* Exclusive-OR instructions */
        case 0xa8: xra(cpu->accmulator, cpu->b) break; // XRA B
        case 0xa9: xra(cpu->accmulator, cpu->c) break; // XRA C
        case 0xaa: xra(cpu->accmulator, cpu->d) break; // XRA D
        case 0xab: xra(cpu->accmulator, cpu->e) break; // XRA E
        case 0xac: xra(cpu->accmulator, cpu->h) break; // XRA H
        case 0xad: xra(cpu->accmulator, cpu->l) break; // XRA L
        case 0xae:
            xra(cpu->accmulator,
                cpu_mem_access(cpu, u16_from_hi_lo_byte(cpu->h, cpu->l))) break; // XRA M
        case 0xaf: xra(cpu->accmulator, cpu->accmulator) break;                  // XRA A

        case 0xee:
            xra(cpu->accmulator, cpu_fetch_next_byte(cpu)) break; // XRI [d8]

        /* Or instructions */
        case 0xb0: ora(cpu->accmulator, cpu->b) break; // ORA B
        case 0xb1: ora(cpu->accmulator, cpu->c) break; // ORA C
        case 0xb2: ora(cpu->accmulator, cpu->d) break; // ORA D
        case 0xb3: ora(cpu->accmulator, cpu->e) break; // ORA E
        case 0xb4: ora(cpu->accmulator, cpu->h) break; // ORA H
        case 0xb5: ora(cpu->accmulator, cpu->l) break; // ORA L
        case 0xb6:
            ora(cpu->accmulator,
                cpu_mem_access(cpu, u16_from_hi_lo_byte(cpu->h, cpu->l))) break; // ORA M
        case 0xb7: ora(cpu->accmulator, cpu->accmulator) break;                  // ORA A
        case 0xf6:
            ora(cpu->accmulator, cpu_fetch_next_byte(cpu)) break; // ORI [d8]

        /* cmp instructions */
        case 0xb8: cmp(cpu->accmulator, cpu->b) break; // CMP B
        case 0xb9: cmp(cpu->accmulator, cpu->c) break; // CMP C
        case 0xba: cmp(cpu->accmulator, cpu->d) break; // CMP D
        case 0xbb: cmp(cpu->accmulator, cpu->e) break; // CMP E
        case 0xbc: cmp(cpu->accmulator, cpu->h) break; // CMP H
        case 0xbd: cmp(cpu->accmulator, cpu->l) break; // CMP L
        case 0xbe:
            cmp(cpu->accmulator,
                cpu_mem_access(cpu, u16_from_hi_lo_byte(cpu->h, cpu->l))) break; // CMP M
        case 0xbf: cmp(cpu->accmulator, cpu->accmulator) break;                  // CMP A
        case 0xfe:
            cmp(cpu->accmulator, cpu_fetch_next_byte(cpu)) break; // CPI [d8]

        /* Set/Reset Carry */
        case 0x3f: cpu->cy = !cpu->cy; break; // CMC
        case 0x37:
            cpu->cy = 0x1;
            break; // STC

        /* bitshift instructions */
        case 0x07: { // RLC
            u8 msb = (cpu->accmulator >> 7) & 1;
            cpu->accmulator = (cpu->accmulator << 1) | msb;
            cpu->cy = msb;
        } break;

        case 0x0f: { // RRC
            u8 lsb = cpu->accmulator & 1;
            cpu->accmulator = (cpu->accmulator >> 1) | (lsb << 7);
            cpu->cy = lsb;
        } break;

        case 0x17: { // RAL
            u8 msb = (cpu->accmulator >> 7) & 1;
            cpu->accmulator = (cpu->accmulator << 1) | cpu->cy;
            cpu->cy = msb;
        } break;

        case 0x1f: { // RAR
            u8 lsb = cpu->accmulator & 1;
            cpu->accmulator = (cpu->accmulator >> 1) | cpu->cy;
            cpu->cy = lsb;
        } break;

        /* jump instructions */
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

        /* call instructions */
        case 0xcd: call(1); break;                  // CALL [Addr]
        case 0xcc: call(cpu->zero == 0x1); break;   // CZ [Addr]
        case 0xc4: call(cpu->zero == 0x0); break;   // CNZ [Addr]
        case 0xdc: call(cpu->cy == 0x1); break;     // CC [Addr]
        case 0xd4: call(cpu->cy == 0x0); break;     // CNC [Addr]
        case 0xf4: call(cpu->sign == 0x0); break;   // CP [Addr]
        case 0xfc: call(cpu->sign == 0x1); break;   // CM [Addr]
        case 0xe4: call(cpu->parity == 0x0); break; // CPO [Addr]
        case 0xec:
            call(cpu->parity == 0x1);
            break; // CPE [Addr]

        /* return instructions */
        case 0xc9: ret(1); break;                  // RET
        case 0xc8: ret(cpu->zero == 0x1); break;   // RZ
        case 0xc0: ret(cpu->zero == 0x0); break;   // RNZ
        case 0xd8: ret(cpu->cy == 0x1); break;     // RR
        case 0xd0: ret(cpu->cy == 0x0); break;     // RNR
        case 0xf8: ret(cpu->sign == 0x1); break;   // RM
        case 0xf0: ret(cpu->sign == 0x0); break;   // RP
        case 0xe8: ret(cpu->parity == 0x1); break; // RPE
        case 0xe0: ret(cpu->parity == 0x0); break; // RPO

        case 0xe9:
            cpu->pc = cpu_reg_pair_hl(cpu);
            break; // PCHL

        /* RST [0-7] */
        case 0xc7: cpu_jump_to_address(cpu, 0x0000); break;
        case 0xcf: cpu_jump_to_address(cpu, 0x0008); break;
        case 0xd7: cpu_jump_to_address(cpu, 0x0010); break;
        case 0xdf: cpu_jump_to_address(cpu, 0x0018); break;
        case 0xe7: cpu_jump_to_address(cpu, 0x0020); break;
        case 0xef: cpu_jump_to_address(cpu, 0x0028); break;
        case 0xf7: cpu_jump_to_address(cpu, 0x0030); break;
        case 0xff: cpu_jump_to_address(cpu, 0x0038); break;

        /* Stack push */
        case 0xf5: push(cpu->accmulator, cpu_get_flag_reg(cpu)); break; // PUSH PSW
        case 0xc5: push(cpu->b, cpu->c); break;                         // PUSH B
        case 0xd5: push(cpu->d, cpu->e); break;                         // PUSH D
        case 0xe5:
            push(cpu->h, cpu->l);
            break; // PUSH H

        /* stack pop */
        case 0xf1: { // POP PSW
            cpu_set_reg_pair_hl(cpu, cpu_mem_access(cpu, cpu->sp));
            cpu->accmulator = cpu_mem_access(cpu, cpu->sp + 1);
            cpu->sp += 2;
        } break;

        case 0xc1: pop(cpu->b, cpu->c); break; // POP B
        case 0xd1: pop(cpu->d, cpu->e); break; // POP D
        case 0xe1: pop(cpu->h, cpu->l); break; // POP H

        case 0xf9: cpu->sp = cpu_reg_pair_hl(cpu); break; // SPHL
        case 0xe3: {                                      // XTHL
            cpu_swap_memory_with_val(cpu, cpu->sp, &cpu->l);
            cpu_swap_memory_with_val(cpu, cpu->sp - 1, &cpu->h);
        } break;

        /* Dad */
        case 0x09: dad(cpu_reg_pair_bc(cpu)); break; // DAD BC
        case 0x19: dad(cpu_reg_pair_de(cpu)); break; // DAD DE
        case 0x29: dad(cpu_reg_pair_hl(cpu)); break; // DAD HL
        case 0x39: dad(cpu->sp); break;              // DAD SP

        case 0xdb: // IN [D8]
        case 0xd3: // OUT [D8]
        case 0xfb: // EI
        case 0xf3: // DI
        case 0x27: // DAA
        default: Todo_Message("Unimplemented: 0x%x", opcode);
        }

        cpu->pc += 1;
    }
}
