/*
 * NOTE(madflash) - These functions don't modify Cpu
 * They uses Cpu for its pc, cpu_fetch_next_byte, cpu_fetch_next_address
 */
void disassemblar_pretty_print_ins(Cpu *cpu, u8 ins_size, char *fmt, ...) {
    Assert_Message(ins_size > 0 && ins_size <= 3, "ins_size should be greater than 0 and less than "
                                                  "4, since max instructions can be of 3 bytes");

    u8 byte, idx;

    idx = 0;
    printf("%x:    ", cpu->pc);

    switch (ins_size) {
#define print_next_ins_byte()                                                                      \
    byte = cpu_mem_access(cpu, cpu->pc + idx);                                                     \
    printf("%x ", byte);                                                                           \
    if (byte < 0x10) {                                                                             \
        putc(' ', stdout);                                                                         \
    }                                                                                              \
    idx += 1;

    case 3: print_next_ins_byte();
    case 2: print_next_ins_byte();
    case 1: print_next_ins_byte(); break;
    default: Unreachable();
    }

    for (; idx < 3; ++idx)
        m_puts("   ");
    m_puts("          "); // padding

    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    putc('\n', stdout);
    va_end(ap);
}

void disassemble(Cpu *cpu) {
    while (cpu->pc < 0xffff) {
        u8 opcode = cpu_mem_access(cpu, cpu->pc);

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
            disassemblar_pretty_print_ins(cpu, 1, "hlt");
            break; // HALT

        /* lxi */
        case 0x01: disassemblar_pretty_print_ins(cpu, 1, "lxi B"); break; // LXI B
        case 0x11: disassemblar_pretty_print_ins(cpu, 1, "lxi D"); break; // LXI D
        case 0x21: disassemblar_pretty_print_ins(cpu, 1, "lxi H"); break; // LXI H
        case 0x31:
            disassemblar_pretty_print_ins(cpu, 1, "lxi SP");
            break; // LXI SP

        /* lda */
        case 0x3a:
            disassemblar_pretty_print_ins(cpu, 3, "lda  0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break;

        /* sta */
        case 0x32:
            disassemblar_pretty_print_ins(cpu, 3, "sta 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break;

        /* ldax */
        case 0x0a: disassemblar_pretty_print_ins(cpu, 1, "ldax B"); break; // LDAX B
        case 0x1a:
            disassemblar_pretty_print_ins(cpu, 1, "ldax D");
            break; // LDAX D

        /* stax */
        case 0x02: disassemblar_pretty_print_ins(cpu, 1, "stax B"); break; // STAX B
        case 0x12:
            disassemblar_pretty_print_ins(cpu, 1, "stax D");
            break; // STAX D

        /* lhld */
        case 0x2a: {
            disassemblar_pretty_print_ins(cpu, 3, "lhld 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
        } break;

        /* shld */
        case 0x22: {
            disassemblar_pretty_print_ins(cpu, 3, "shld 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
        } break;

        /* xchg */
        case 0xeb: disassemblar_pretty_print_ins(cpu, 1, "xchg"); break;

        /* cma */
        case 0x2f: disassemblar_pretty_print_ins(cpu, 1, "cma"); break;

        /* inr */
        case 0x04: disassemblar_pretty_print_ins(cpu, 1, "inr B"); break; // INR B
        case 0x0c: disassemblar_pretty_print_ins(cpu, 1, "inr C"); break; // INR C
        case 0x14: disassemblar_pretty_print_ins(cpu, 1, "inr D"); break; // INR D
        case 0x1c: disassemblar_pretty_print_ins(cpu, 1, "inr E"); break; // INR E
        case 0x24: disassemblar_pretty_print_ins(cpu, 1, "inr H"); break; // INR H
        case 0x2c: disassemblar_pretty_print_ins(cpu, 1, "inr L"); break; // INR L
        case 0x34: disassemblar_pretty_print_ins(cpu, 1, "inr M"); break; // INR M
        case 0x3c:
            disassemblar_pretty_print_ins(cpu, 1, "inr A");
            break; // INR A

        /* dcr */
        case 0x05: disassemblar_pretty_print_ins(cpu, 1, "dcr B"); break; // DCR B
        case 0x0d: disassemblar_pretty_print_ins(cpu, 1, "dcr C"); break; // DCR C
        case 0x15: disassemblar_pretty_print_ins(cpu, 1, "dcr D"); break; // DCR D
        case 0x1d: disassemblar_pretty_print_ins(cpu, 1, "dcr E"); break; // DCR E
        case 0x25: disassemblar_pretty_print_ins(cpu, 1, "dcr H"); break; // DCR H
        case 0x2d: disassemblar_pretty_print_ins(cpu, 1, "dcr L"); break; // DCR L
        case 0x35: disassemblar_pretty_print_ins(cpu, 1, "dcr M"); break; // DCR M
        case 0x3d:
            disassemblar_pretty_print_ins(cpu, 1, "dcr A");
            break; // DCR A

        /* inx */
        case 0x03: disassemblar_pretty_print_ins(cpu, 1, "inx BC"); break;
        case 0x13: disassemblar_pretty_print_ins(cpu, 1, "inx DE"); break;
        case 0x23: disassemblar_pretty_print_ins(cpu, 1, "inx HL"); break;
        case 0x33: disassemblar_pretty_print_ins(cpu, 1, "inx SP"); break;

        /* dcx */
        case 0x0b: disassemblar_pretty_print_ins(cpu, 1, "dcx BC"); break;
        case 0x1b: disassemblar_pretty_print_ins(cpu, 1, "dcx DE"); break;
        case 0x2b: disassemblar_pretty_print_ins(cpu, 1, "dcx HL"); break;
        case 0x3b: disassemblar_pretty_print_ins(cpu, 1, "dcx SP"); break;

        /* mov instructions */
        case 0x40: disassemblar_pretty_print_ins(cpu, 1, "mov B, B"); break;
        case 0x41: disassemblar_pretty_print_ins(cpu, 1, "mov B, C"); break;
        case 0x42: disassemblar_pretty_print_ins(cpu, 1, "mov B, D"); break;
        case 0x43: disassemblar_pretty_print_ins(cpu, 1, "mov B, E"); break;
        case 0x44: disassemblar_pretty_print_ins(cpu, 1, "mov B, H"); break;
        case 0x45: disassemblar_pretty_print_ins(cpu, 1, "mov B, L"); break;
        case 0x46: disassemblar_pretty_print_ins(cpu, 1, "mov B, M"); break;
        case 0x47: disassemblar_pretty_print_ins(cpu, 1, "mov B, A"); break;
        case 0x48: disassemblar_pretty_print_ins(cpu, 1, "mov C, B"); break;
        case 0x49: disassemblar_pretty_print_ins(cpu, 1, "mov C, C"); break;
        case 0x4a: disassemblar_pretty_print_ins(cpu, 1, "mov C, D"); break;
        case 0x4b: disassemblar_pretty_print_ins(cpu, 1, "mov C, E"); break;
        case 0x4c: disassemblar_pretty_print_ins(cpu, 1, "mov C, H"); break;
        case 0x4d: disassemblar_pretty_print_ins(cpu, 1, "mov C, L"); break;
        case 0x4e: disassemblar_pretty_print_ins(cpu, 1, "mov C, M"); break;
        case 0x4f: disassemblar_pretty_print_ins(cpu, 1, "mov C, A"); break;
        case 0x50: disassemblar_pretty_print_ins(cpu, 1, "mov D, B"); break;
        case 0x51: disassemblar_pretty_print_ins(cpu, 1, "mov D, C"); break;
        case 0x52: disassemblar_pretty_print_ins(cpu, 1, "mov D, D"); break;
        case 0x53: disassemblar_pretty_print_ins(cpu, 1, "mov D, E"); break;
        case 0x54: disassemblar_pretty_print_ins(cpu, 1, "mov D, H"); break;
        case 0x55: disassemblar_pretty_print_ins(cpu, 1, "mov D, L"); break;
        case 0x56: disassemblar_pretty_print_ins(cpu, 1, "mov D, M"); break;
        case 0x57: disassemblar_pretty_print_ins(cpu, 1, "mov D, A"); break;
        case 0x58: disassemblar_pretty_print_ins(cpu, 1, "mov E, B"); break;
        case 0x59: disassemblar_pretty_print_ins(cpu, 1, "mov E, C"); break;
        case 0x5a: disassemblar_pretty_print_ins(cpu, 1, "mov E, D"); break;
        case 0x5b: disassemblar_pretty_print_ins(cpu, 1, "mov E, E"); break;
        case 0x5c: disassemblar_pretty_print_ins(cpu, 1, "mov E, H"); break;
        case 0x5d: disassemblar_pretty_print_ins(cpu, 1, "mov E, L"); break;
        case 0x5e: disassemblar_pretty_print_ins(cpu, 1, "mov E, M"); break;
        case 0x5f: disassemblar_pretty_print_ins(cpu, 1, "mov E, A"); break;
        case 0x60: disassemblar_pretty_print_ins(cpu, 1, "mov H, B"); break;
        case 0x61: disassemblar_pretty_print_ins(cpu, 1, "mov H, C"); break;
        case 0x62: disassemblar_pretty_print_ins(cpu, 1, "mov H, D"); break;
        case 0x63: disassemblar_pretty_print_ins(cpu, 1, "mov H, E"); break;
        case 0x64: disassemblar_pretty_print_ins(cpu, 1, "mov H, H"); break;
        case 0x65: disassemblar_pretty_print_ins(cpu, 1, "mov H, L"); break;
        case 0x66: disassemblar_pretty_print_ins(cpu, 1, "mov H, M"); break;
        case 0x67: disassemblar_pretty_print_ins(cpu, 1, "mov H, A"); break;
        case 0x68: disassemblar_pretty_print_ins(cpu, 1, "mov L, B"); break;
        case 0x69: disassemblar_pretty_print_ins(cpu, 1, "mov L, C"); break;
        case 0x6a: disassemblar_pretty_print_ins(cpu, 1, "mov L, D"); break;
        case 0x6b: disassemblar_pretty_print_ins(cpu, 1, "mov L, E"); break;
        case 0x6c: disassemblar_pretty_print_ins(cpu, 1, "mov L, H"); break;
        case 0x6d: disassemblar_pretty_print_ins(cpu, 1, "mov L, L"); break;
        case 0x6e: disassemblar_pretty_print_ins(cpu, 1, "mov L, M"); break;
        case 0x6f: disassemblar_pretty_print_ins(cpu, 1, "mov L, A"); break;
        case 0x70: disassemblar_pretty_print_ins(cpu, 1, "mov M, B"); break;
        case 0x71: disassemblar_pretty_print_ins(cpu, 1, "mov M, C"); break;
        case 0x72: disassemblar_pretty_print_ins(cpu, 1, "mov M, C"); break;
        case 0x73: disassemblar_pretty_print_ins(cpu, 1, "mov M, D"); break;
        case 0x74: disassemblar_pretty_print_ins(cpu, 1, "mov M, E"); break;
        case 0x75: disassemblar_pretty_print_ins(cpu, 1, "mov M, H"); break;
        case 0x77: disassemblar_pretty_print_ins(cpu, 1, "mov M, A"); break;
        case 0x78: disassemblar_pretty_print_ins(cpu, 1, "mov A, B"); break;
        case 0x79: disassemblar_pretty_print_ins(cpu, 1, "mov A, C"); break;
        case 0x7a: disassemblar_pretty_print_ins(cpu, 1, "mov A, D"); break;
        case 0x7b: disassemblar_pretty_print_ins(cpu, 1, "mov A, E"); break;
        case 0x7c: disassemblar_pretty_print_ins(cpu, 1, "mov A, H"); break;
        case 0x7d: disassemblar_pretty_print_ins(cpu, 1, "mov A, L"); break;
        case 0x7e: disassemblar_pretty_print_ins(cpu, 1, "mov A, M"); break;
        case 0x7f: disassemblar_pretty_print_ins(cpu, 1, "mov A, A"); break;

        /* mvi instructions */
        case 0x06:
            disassemblar_pretty_print_ins(cpu, 2, "mvi B, 0x%x", cpu_fetch_next_byte(cpu));
            break;
        case 0x0e:
            disassemblar_pretty_print_ins(cpu, 2, "mvi B, 0x%x", cpu_fetch_next_byte(cpu));
            break;
        case 0x16:
            disassemblar_pretty_print_ins(cpu, 2, "mvi B, 0x%x", cpu_fetch_next_byte(cpu));
            break;
        case 0x1e:
            disassemblar_pretty_print_ins(cpu, 2, "mvi B, 0x%x", cpu_fetch_next_byte(cpu));
            break;
        case 0x26:
            disassemblar_pretty_print_ins(cpu, 2, "mvi B, 0x%x", cpu_fetch_next_byte(cpu));
            break;
        case 0x2e:
            disassemblar_pretty_print_ins(cpu, 2, "mvi B, 0x%x", cpu_fetch_next_byte(cpu));
            break;
        case 0x36:
            disassemblar_pretty_print_ins(cpu, 2, "mvi M, 0x%x", cpu_fetch_next_byte(cpu));
            break;
        case 0x3e:
            disassemblar_pretty_print_ins(cpu, 2, "mvi A, 0x%x", cpu_fetch_next_byte(cpu));
            break;

        /* add instructions */
        case 0x80: disassemblar_pretty_print_ins(cpu, 1, "add B"); break; // ADD B
        case 0x81: disassemblar_pretty_print_ins(cpu, 1, "add C"); break; // ADD C
        case 0x82: disassemblar_pretty_print_ins(cpu, 1, "add D"); break; // ADD D
        case 0x83: disassemblar_pretty_print_ins(cpu, 1, "add E"); break; // ADD E
        case 0x84: disassemblar_pretty_print_ins(cpu, 1, "add H"); break; // ADD H
        case 0x85: disassemblar_pretty_print_ins(cpu, 1, "add L"); break; // ADD L
        case 0x86: disassemblar_pretty_print_ins(cpu, 1, "add M"); break; // ADD M
        case 0x87: disassemblar_pretty_print_ins(cpu, 1, "add A"); break; // ADD A
        case 0xc6:
            disassemblar_pretty_print_ins(cpu, 2, "adi 0x%x", cpu_fetch_next_byte(cpu));
            break; // ADI [d8]

        case 0x88: disassemblar_pretty_print_ins(cpu, 1, "adc B"); break; // ADC B
        case 0x89: disassemblar_pretty_print_ins(cpu, 1, "adc C"); break; // ADC C
        case 0x8a: disassemblar_pretty_print_ins(cpu, 1, "adc D"); break; // ADC D
        case 0x8b: disassemblar_pretty_print_ins(cpu, 1, "adc E"); break; // ADC E
        case 0x8c: disassemblar_pretty_print_ins(cpu, 1, "adc H"); break; // ADC H
        case 0x8d: disassemblar_pretty_print_ins(cpu, 1, "adc L"); break; // ADC L
        case 0x8e: disassemblar_pretty_print_ins(cpu, 1, "adc M"); break; // ADC M
        case 0x8f: disassemblar_pretty_print_ins(cpu, 1, "adc A"); break; // ADC A
        case 0xce:
            disassemblar_pretty_print_ins(cpu, 2, "aci 0x%x", cpu_fetch_next_byte(cpu));
            break; // ACI [d8]

        /* sub instructions */
        case 0x90: disassemblar_pretty_print_ins(cpu, 1, "sub B"); break; // SUB B
        case 0x91: disassemblar_pretty_print_ins(cpu, 1, "sub C"); break; // SUB C
        case 0x92: disassemblar_pretty_print_ins(cpu, 1, "sub D"); break; // SUB D
        case 0x93: disassemblar_pretty_print_ins(cpu, 1, "sub E"); break; // SUB E
        case 0x94: disassemblar_pretty_print_ins(cpu, 1, "sub H"); break; // SUB H
        case 0x95: disassemblar_pretty_print_ins(cpu, 1, "sub L"); break; // SUB L
        case 0x96: disassemblar_pretty_print_ins(cpu, 1, "sub M"); break; // SUB M
        case 0x97: disassemblar_pretty_print_ins(cpu, 1, "sub A"); break; // SUB A
        case 0xd6:
            disassemblar_pretty_print_ins(cpu, 2, "sui 0x%x", cpu_fetch_next_byte(cpu));
            break; // SUI [d8]

        case 0x98: disassemblar_pretty_print_ins(cpu, 1, "sbb B"); break; // SBB B
        case 0x99: disassemblar_pretty_print_ins(cpu, 1, "sbb C"); break; // SBB C
        case 0x9a: disassemblar_pretty_print_ins(cpu, 1, "sbb D"); break; // SBB D
        case 0x9b: disassemblar_pretty_print_ins(cpu, 1, "sbb E"); break; // SBB E
        case 0x9c: disassemblar_pretty_print_ins(cpu, 1, "sbb H"); break; // SBB H
        case 0x9d: disassemblar_pretty_print_ins(cpu, 1, "sbb L"); break; // SBB L
        case 0x9e: disassemblar_pretty_print_ins(cpu, 1, "sbb M"); break; // SUB M
        case 0x9f: disassemblar_pretty_print_ins(cpu, 1, "sbb A"); break; // SUB A
        case 0xde:
            disassemblar_pretty_print_ins(cpu, 2, "sbi 0x%x", cpu_fetch_next_byte(cpu));
            break; // SBI [d8]

        /* ana instructions */
        case 0xa0: disassemblar_pretty_print_ins(cpu, 1, "ana B"); break; // ANA B
        case 0xa1: disassemblar_pretty_print_ins(cpu, 1, "ana C"); break; // ANA C
        case 0xa2: disassemblar_pretty_print_ins(cpu, 1, "ana D"); break; // ANA D
        case 0xa3: disassemblar_pretty_print_ins(cpu, 1, "ana E"); break; // ANA E
        case 0xa4: disassemblar_pretty_print_ins(cpu, 1, "ana H"); break; // ANA H
        case 0xa5: disassemblar_pretty_print_ins(cpu, 1, "ana L"); break; // ANA L
        case 0xa6: disassemblar_pretty_print_ins(cpu, 1, "ana M"); break; // ANA M
        case 0xa7: disassemblar_pretty_print_ins(cpu, 1, "ana A"); break; // ANA A
        case 0xe6:
            disassemblar_pretty_print_ins(cpu, 2, "ani 0x%x", cpu_fetch_next_byte(cpu));
            break; // ANI [d8]

        /* Exclusive-OR instructions */
        case 0xa8: disassemblar_pretty_print_ins(cpu, 1, "xra B"); break; // XRA B
        case 0xa9: disassemblar_pretty_print_ins(cpu, 1, "xra C"); break; // XRA C
        case 0xaa: disassemblar_pretty_print_ins(cpu, 1, "xra D"); break; // XRA D
        case 0xab: disassemblar_pretty_print_ins(cpu, 1, "xra E"); break; // XRA E
        case 0xac: disassemblar_pretty_print_ins(cpu, 1, "xra H"); break; // XRA H
        case 0xad: disassemblar_pretty_print_ins(cpu, 1, "xra L"); break; // XRA L
        case 0xae: disassemblar_pretty_print_ins(cpu, 1, "xra M"); break; // XRA M
        case 0xaf: disassemblar_pretty_print_ins(cpu, 1, "xra A"); break; // XRA A
        case 0xee:
            disassemblar_pretty_print_ins(cpu, 2, "xri 0x%x", cpu_fetch_next_byte(cpu));
            break; // XRI [d8]

        /* Or instructions */
        case 0xb0: disassemblar_pretty_print_ins(cpu, 1, "ora B"); break; // ORA B
        case 0xb1: disassemblar_pretty_print_ins(cpu, 1, "ora C"); break; // ORA C
        case 0xb2: disassemblar_pretty_print_ins(cpu, 1, "ora D"); break; // ORA D
        case 0xb3: disassemblar_pretty_print_ins(cpu, 1, "ora E"); break; // ORA E
        case 0xb4: disassemblar_pretty_print_ins(cpu, 1, "ora H"); break; // ORA H
        case 0xb5: disassemblar_pretty_print_ins(cpu, 1, "ora L"); break; // ORA L
        case 0xb6: disassemblar_pretty_print_ins(cpu, 1, "ora M"); break; // ORA M
        case 0xb7: disassemblar_pretty_print_ins(cpu, 1, "ora A"); break; // ORA A
        case 0xf6:
            disassemblar_pretty_print_ins(cpu, 2, "ori 0x%x", cpu_fetch_next_byte(cpu));
            break; // ORI [d8]

        /* cmp instructions */
        case 0xb8: disassemblar_pretty_print_ins(cpu, 1, "cmp B"); break; // CMP B
        case 0xb9: disassemblar_pretty_print_ins(cpu, 1, "cmp C"); break; // CMP C
        case 0xba: disassemblar_pretty_print_ins(cpu, 1, "cmp D"); break; // CMP D
        case 0xbb: disassemblar_pretty_print_ins(cpu, 1, "cmp E"); break; // CMP E
        case 0xbc: disassemblar_pretty_print_ins(cpu, 1, "cmp H"); break; // CMP H
        case 0xbd: disassemblar_pretty_print_ins(cpu, 1, "cmp L"); break; // CMP L
        case 0xbe: disassemblar_pretty_print_ins(cpu, 1, "cmp M"); break; // CMP M
        case 0xbf: disassemblar_pretty_print_ins(cpu, 1, "cmp A"); break; // CMP A
        case 0xfe:
            disassemblar_pretty_print_ins(cpu, 2, "cpi 0x%x", cpu_fetch_next_byte(cpu));
            break; // CPI [d8]

        /* Set/Reset Carry */
        case 0x3f: disassemblar_pretty_print_ins(cpu, 1, "cmc"); break; // CMC
        case 0x37:
            disassemblar_pretty_print_ins(cpu, 1, "stc");
            break; // STC

        /* bitshift instructions */
        case 0x07: disassemblar_pretty_print_ins(cpu, 1, "rlc"); break; // RLC
        case 0x0f: disassemblar_pretty_print_ins(cpu, 1, "rlc"); break; // RRC
        case 0x17: disassemblar_pretty_print_ins(cpu, 1, "ral"); break; // RAL
        case 0x1f:
            disassemblar_pretty_print_ins(cpu, 1, "rar");
            break; // RAR

        /* jump instructions */
        case 0xca:
            disassemblar_pretty_print_ins(cpu, 3, "jz 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JZ [Addr]
        case 0xc2:
            disassemblar_pretty_print_ins(cpu, 3, "jnz 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JNZ [Addr]
        case 0xc3:
            disassemblar_pretty_print_ins(cpu, 3, "jmp 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JMP [Addr]
        case 0xda:
            disassemblar_pretty_print_ins(cpu, 3, "jc 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JC [Addr]
        case 0xd2:
            disassemblar_pretty_print_ins(cpu, 3, "jnc 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JNC [Addr]
        case 0xea:
            disassemblar_pretty_print_ins(cpu, 3, "jpe 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JPE [Addr]
        case 0xe2:
            disassemblar_pretty_print_ins(cpu, 3, "jpo 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JPO [Addr]
        case 0xf2:
            disassemblar_pretty_print_ins(cpu, 3, "jp 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JP [Addr]
        case 0xfa:
            disassemblar_pretty_print_ins(cpu, 3, "jm 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // JM [Addr]

        /* call instructions */
        case 0xcd:
            disassemblar_pretty_print_ins(cpu, 3, "call 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CALL [Addr]
        case 0xcc:
            disassemblar_pretty_print_ins(cpu, 3, "cz 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CZ [Addr]
        case 0xc4:
            disassemblar_pretty_print_ins(cpu, 3, "cnz 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CNZ [Addr]
        case 0xdc:
            disassemblar_pretty_print_ins(cpu, 3, "cc 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CC [Addr]
        case 0xd4:
            disassemblar_pretty_print_ins(cpu, 3, "cnc 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CNC [Addr]
        case 0xf4:
            disassemblar_pretty_print_ins(cpu, 3, "cp 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CP [Addr]
        case 0xfc:
            disassemblar_pretty_print_ins(cpu, 3, "cm 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CM [Addr]
        case 0xe4:
            disassemblar_pretty_print_ins(cpu, 3, "cpo 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CPO [Addr]
        case 0xec:
            disassemblar_pretty_print_ins(cpu, 3, "cpe 0x%x", cpu_fetch_next_address(cpu));
            cpu->pc += 2;
            break; // CPE [Addr]

        /* return instructions */
        case 0xc9: disassemblar_pretty_print_ins(cpu, 1, "ret"); break; // RET
        case 0xc8: disassemblar_pretty_print_ins(cpu, 1, "rz"); break;  // RZ
        case 0xc0: disassemblar_pretty_print_ins(cpu, 1, "rnz"); break; // RNZ
        case 0xd8: disassemblar_pretty_print_ins(cpu, 1, "rr"); break;  // RR
        case 0xd0: disassemblar_pretty_print_ins(cpu, 1, "rnr"); break; // RNR
        case 0xf8: disassemblar_pretty_print_ins(cpu, 1, "rm"); break;  // RM
        case 0xf0: disassemblar_pretty_print_ins(cpu, 1, "rp"); break;  // RP
        case 0xe8: disassemblar_pretty_print_ins(cpu, 1, "rpe"); break; // RPE
        case 0xe0: disassemblar_pretty_print_ins(cpu, 1, "rpo"); break; // RPO

        case 0xe9:
            disassemblar_pretty_print_ins(cpu, 1, "pchl");
            break; // PCHL

        /* RST [0-7] */
        case 0xc7: disassemblar_pretty_print_ins(cpu, 1, "rst 0"); break;
        case 0xcf: disassemblar_pretty_print_ins(cpu, 1, "rst 1"); break;
        case 0xd7: disassemblar_pretty_print_ins(cpu, 1, "rst 2"); break;
        case 0xdf: disassemblar_pretty_print_ins(cpu, 1, "rst 3"); break;
        case 0xe7: disassemblar_pretty_print_ins(cpu, 1, "rst 4"); break;
        case 0xef: disassemblar_pretty_print_ins(cpu, 1, "rst 5"); break;
        case 0xf7: disassemblar_pretty_print_ins(cpu, 1, "rst 6"); break;
        case 0xff: disassemblar_pretty_print_ins(cpu, 1, "rst 7"); break;

        /* Stack push */
        case 0xf5: disassemblar_pretty_print_ins(cpu, 1, "push PSW"); break; // PUSH PSW
        case 0xc5: disassemblar_pretty_print_ins(cpu, 1, "push B"); break;   // PUSH B
        case 0xd5: disassemblar_pretty_print_ins(cpu, 1, "push D"); break;   // PUSH D
        case 0xe5:
            disassemblar_pretty_print_ins(cpu, 1, "push H");
            break; // PUSH H

        /* stack pop */
        case 0xf1: disassemblar_pretty_print_ins(cpu, 1, "pop PSW"); break; // POP PSW

        case 0xc1: disassemblar_pretty_print_ins(cpu, 1, "pop B"); break; // POP B
        case 0xd1: disassemblar_pretty_print_ins(cpu, 1, "pop D"); break; // POP D
        case 0xe1: disassemblar_pretty_print_ins(cpu, 1, "pop H"); break; // POP H

        case 0xf9: disassemblar_pretty_print_ins(cpu, 1, "sphl"); break; // SPHL
        case 0xe3:
            disassemblar_pretty_print_ins(cpu, 1, "xthl");
            break; // XTHL

        /* Dad */
        case 0x09: disassemblar_pretty_print_ins(cpu, 1, "DAD BC"); break; // DAD BC
        case 0x19: disassemblar_pretty_print_ins(cpu, 1, "DAD DE"); break; // DAD DE
        case 0x29: disassemblar_pretty_print_ins(cpu, 1, "DAD HL"); break; // DAD HL
        case 0x39: disassemblar_pretty_print_ins(cpu, 1, "DAD SP"); break; // DAD SP

        case 0xdb:
            disassemblar_pretty_print_ins(cpu, 2, "IN 0x%x", cpu_fetch_next_byte(cpu));
            break; // IN [D8]
        case 0xd3:
            disassemblar_pretty_print_ins(cpu, 2, "OUT 0x%x", cpu_fetch_next_byte(cpu));
            break;                                                     // OUT [D8]
        case 0xfb: disassemblar_pretty_print_ins(cpu, 1, "EI"); break; // EI
        case 0xf3: disassemblar_pretty_print_ins(cpu, 1, "DI"); break; // DI
        case 0x27:
            disassemblar_pretty_print_ins(cpu, 1, "DAA");
            break; // DAA
                   //
        default: Todo_Message("Unimplemented: 0x%x", opcode);
        }

        cpu->pc += 1;
    }
}
