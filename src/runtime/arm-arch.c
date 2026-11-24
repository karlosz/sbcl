/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMUCL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */
#include <stdio.h>

#include "genesis/sbcl.h"
#include "runtime.h"
#include "arch.h"
#include "globals.h"
#include "os.h"
#include "lispregs.h"
#include <signal.h>
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"

void arch_skip_instruction(os_context_t *context)
{
    /* KLUDGE: Other platforms check for trap codes and skip inlined
     * trap/error parameters.  We should too. */
    OS_CONTEXT_PC(context) += 4;
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)(OS_CONTEXT_PC(context) + 5);
}

bool arch_pseudo_atomic_atomic(struct thread __attribute__((unused)) *thread) {
    return SYMBOL(PSEUDO_ATOMIC_ATOMIC)->value != NIL;
}

void arch_set_pseudo_atomic_interrupted(struct thread __attribute__((unused)) *thread)
{
    extern void do_pending_interrupt();
    SYMBOL(PSEUDO_ATOMIC_INTERRUPTED)->value = (lispobj)do_pending_interrupt;
}

void arch_clear_pseudo_atomic_interrupted(struct thread __attribute__((unused)) *thread)
{
    SYMBOL(PSEUDO_ATOMIC_INTERRUPTED)->value = 0;
}

/* Breakpoint support: a BKPT instruction replaces the one a
 * breakpoint marks, and the displaced one is executed either by
 * interpreting it, when its own position matters to it, or from a
 * trampoline page, when it does not. */

unsigned int arch_install_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *)pc;
    unsigned int result = *ptr;

    *ptr = 0xE1200070; /* BKPT #0 */

    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));

    return result;
}

void arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    *(unsigned int *) pc = orig_inst;

    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
}

#define N_BIT 31
#define Z_BIT 30
#define C_BIT 29
#define V_BIT 28

/* The condition flags of the interrupted context. */
static unsigned int
os_context_flags(os_context_t *context)
{
#ifdef LISP_FEATURE_LINUX
    return context->uc_mcontext.arm_cpsr;
#else
    lose("Reading the condition flags is not implemented.");
    return 0;
#endif
}

static bool
condition_holds(os_context_t *context, unsigned int cond)
{
    unsigned int flags = os_context_flags(context);
    unsigned int result;

    switch (cond >> 1) {
    case 0: /* EQ / NE */
        result = (flags >> Z_BIT) & 1;
        break;
    case 1: /* CS / CC */
        result = (flags >> C_BIT) & 1;
        break;
    case 2: /* MI / PL */
        result = (flags >> N_BIT) & 1;
        break;
    case 3: /* VS / VC */
        result = (flags >> V_BIT) & 1;
        break;
    case 4: /* HI / LS */
        result = ((flags >> C_BIT) & 1) && !((flags >> Z_BIT) & 1);
        break;
    case 5: /* GE / LT */
        result = ((flags >> N_BIT) & 1) == ((flags >> V_BIT) & 1);
        break;
    case 6: /* GT / LE */
        result = (((flags >> N_BIT) & 1) == ((flags >> V_BIT) & 1))
            && !((flags >> Z_BIT) & 1);
        break;
    case 7: /* AL */
        result = 1;
        break;
    }
    return cond & 1 ? !result : result;
}

static void
set_condition_flags(os_context_t *context, bool n, bool z, bool c, bool v)
{
#ifdef LISP_FEATURE_LINUX
    unsigned int flags = context->uc_mcontext.arm_cpsr;
    flags = (flags & ~((1 << N_BIT) | (1 << Z_BIT) | (1 << C_BIT) | (1 << V_BIT)))
        | (n << N_BIT) | (z << Z_BIT) | (c << C_BIT) | (v << V_BIT);
    context->uc_mcontext.arm_cpsr = flags;
#else
    lose("Writing the condition flags is not implemented.");
#endif
}

/* The second operand of a data processing instruction, and the carry
 * it produces, following an immediate or a register shift. */
static uword_t
arm_operand2(os_context_t *context, unsigned int inst, bool *carry)
{
    unsigned int value;
    if (inst & 0x02000000) { /* Immediate */
        unsigned int rotate = (inst >> 8) & 0xF;
        value = inst & 0xFF;
        *carry = (value >> (rotate ? 2 * rotate - 1 : 31)) & 1;
        value = value >> 2 * rotate | (rotate ? value << (32 - 2 * rotate) : 0);
    } else {
        unsigned int shift = (inst >> 7) & 0x1F;
        value = *os_context_register_addr(context, inst & 0xF);
        switch ((inst >> 5) & 3) {
        case 0: /* LSL */
            *carry = (value << shift) >> 31;
            value = value << shift;
            break;
        case 1: /* LSR */
            *carry = shift ? value >> (shift - 1) & 1 : (value >> 31) & 1;
            value = shift ? value >> shift : 0;
            break;
        case 2: /* ASR */
            *carry = shift ? (uword_t)((sword_t)value >> (shift - 1)) & 1
                : (value >> 31) & 1;
            value = shift ? (uword_t)((sword_t)value >> shift)
                : (uword_t)((sword_t)value >> 31);
            break;
        case 3: /* ROR, RRX with a shift of zero */
            if (shift) {
                *carry = value >> (shift - 1) & 1;
                value = value >> shift | value << (32 - shift);
            } else {
                bool c = (os_context_flags(context) >> C_BIT) & 1;
                *carry = c;
                value = value >> 1 | c << 31;
            }
            break;
        }
    }
    return value;
}

/* Execute a data processing instruction; the S variant updates the
 * condition flags of the context.  Returns whether the instruction
 * branched by writing PC. */
static bool
arm_data_processing(os_context_t *context, unsigned int inst)
{
    unsigned int opcode = (inst >> 21) & 0xF;
    bool s = inst & 0x00100000;
    unsigned int rd = (inst >> 12) & 0xF;
    uword_t rn = *os_context_register_addr(context, (inst >> 16) & 0xF);
    bool carry_out = 0;
    uword_t operand2 = arm_operand2(context, inst, &carry_out);
    uword_t result;
    bool c = 0, v = 0;

    switch (opcode) {
    case 0: /* AND */
        result = rn & operand2; c = carry_out;
        break;
    case 1: /* EOR */
        result = rn ^ operand2; c = carry_out;
        break;
    case 2: /* SUB */
        result = rn - operand2;
        c = rn >= operand2;
        v = ((sword_t)(operand2 ^ rn) & (sword_t)(result ^ rn)) < 0;
        break;
    case 3: /* RSB */
        result = operand2 - rn;
        c = operand2 >= rn;
        v = ((sword_t)(operand2 ^ rn) & (sword_t)(result ^ operand2)) < 0;
        break;
    case 4: /* ADD */
        result = rn + operand2;
        c = result < rn;
        v = ((sword_t)~(rn ^ operand2) & (sword_t)(result ^ rn)) < 0;
        break;
    case 5: { /* ADC */
        bool carry_in = (os_context_flags(context) >> C_BIT) & 1;
        uword_t sum = rn + operand2;
        result = sum + carry_in;
        c = sum < rn || result < sum;
        v = ((sword_t)~(rn ^ operand2) & (sword_t)(result ^ rn)) < 0;
        break;
    }
    case 6: { /* SBC */
        bool carry_in = (os_context_flags(context) >> C_BIT) & 1;
        result = rn - operand2 - !carry_in;
        c = rn >= operand2 && (rn > operand2 || carry_in);
        v = ((sword_t)(operand2 ^ rn) & (sword_t)(result ^ rn)) < 0;
        break;
    }
    case 7: { /* RSC */
        bool carry_in = (os_context_flags(context) >> C_BIT) & 1;
        result = operand2 - rn - !carry_in;
        c = operand2 >= rn && (operand2 > rn || carry_in);
        v = ((sword_t)(operand2 ^ rn) & (sword_t)(result ^ operand2)) < 0;
        break;
    }
    case 8: /* TST */
        result = rn & operand2; c = carry_out;
        break;
    case 9: /* TEQ */
        result = rn ^ operand2; c = carry_out;
        break;
    case 10: /* CMP */
        result = rn - operand2;
        c = rn >= operand2;
        v = ((sword_t)(operand2 ^ rn) & (sword_t)(result ^ rn)) < 0;
        break;
    case 11: /* CMN */
        result = rn + operand2;
        c = result < rn;
        v = ((sword_t)~(rn ^ operand2) & (sword_t)(result ^ rn)) < 0;
        break;
    case 12: /* ORR */
        result = rn | operand2; c = carry_out;
        break;
    case 13: /* MOV */
        result = operand2; c = carry_out;
        break;
    case 14: /* BIC */
        result = rn & ~operand2; c = carry_out;
        break;
    case 15: /* MVN */
        result = ~operand2; c = carry_out;
        break;
    }

    if (opcode >= 8 && opcode <= 11)
        /* A comparison leaves its result in the flags only.  An
         * immediate PSR transfer (MSR) shares the encoding of a
         * comparison with Rd = PC, and this leaves it without an
         * effect too; the code generator never emits the real
         * thing. */
        return false;
    else if (rd == 15) {
        /* A write to PC branches; the low bit selects the
         * instruction set, which this port never switches to. */
        OS_CONTEXT_PC(context) = result & ~1u;
        return true;
    }
    else
        *os_context_register_addr(context, rd) = result;

    if (s)
        set_condition_flags(context, result >> 31, result == 0, c, v);
    return false;
}

void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int *)OS_CONTEXT_PC(context);
    uword_t next_pc = (uword_t)(pc + 1);

    if ((orig_inst & 0x0F000000) == 0x0A000000) {
        /* Branch and branch with link.  The offset counts from the
         * position of the instruction, which a trampoline would
         * change. */
        if (condition_holds(context, orig_inst >> 28))
            next_pc = (uword_t)pc + 8
                + (sword_t)((orig_inst & 0x00FFFFFF) << 2);
        if (orig_inst & 0x01000000) /* BL */
            *os_context_register_addr(context, reg_LR) = (uword_t)(pc + 1);
    }
    else if ((orig_inst & 0x0FFFFFF0) == 0x012FFF10) {
        /* BX Rm */
        next_pc = *os_context_register_addr(context, orig_inst & 0xF) & ~1u;
    }
    else if ((orig_inst & 0x0FFFFFF0) == 0x012FFF30) {
        /* BLX Rm */
        uword_t target =
            *os_context_register_addr(context, orig_inst & 0xF) & ~1u;
        *os_context_register_addr(context, reg_LR) = (uword_t)(pc + 1);
        next_pc = target;
    }
    else if ((orig_inst & 0x0C000000) == 0x04000000
             && ((orig_inst >> 16) & 0xF) == 15
             && (orig_inst & 0x01100000) == 0x01100000) {
        /* LDR Rd, [PC, #imm]: a literal pool load.  LDR PC, [PC,
         * #imm] is a branch through the word. */
        unsigned int rd = (orig_inst >> 12) & 0xF;
        uword_t addr = (uword_t)pc + 8
            + (uword_t)(orig_inst & 0x00800000 ? orig_inst & 0xFFF
                                               : -(sword_t)(orig_inst & 0xFFF));
        uword_t value = orig_inst & 0x00400000
            ? *(unsigned char *)addr : *(uword_t *)addr;
        if (rd == 15)
            next_pc = value & ~1u;
        else
            *os_context_register_addr(context, rd) = value;
    }
    else if ((orig_inst & 0x0C000000) == 0x00000000
             && (orig_inst & 0x90) != 0x90) {
        /* Data processing, which can name PC as an operand, or write
         * it, like the call of a simple function does, or compute an
         * address from it.  Anything else of the multiply, extra load
         * and register shifted encoding goes to the trampoline below. */
        if (!arm_data_processing(context, orig_inst))
            OS_CONTEXT_PC(context) = (uword_t)next_pc;
        return;
    }
    else {
        /* The instruction does not depend on its own position:
         * execute it in a trampoline, and continue at the
         * instruction after it.  LDR PC, [PC, #imm] branches back
         * without using a register, which no breakpoint can
         * clobber. */
        size_t size = getpagesize();
        struct thread *th = get_sb_vm_thread();
        unsigned int *trampoline = (unsigned int*)th->breakpoint_misc;

        if (!trampoline) {
            trampoline = mmap(NULL, size,
                              PROT_READ | PROT_WRITE,
                              MAP_PRIVATE | MAP_ANONYMOUS,
                              -1, 0);
            th->breakpoint_misc = trampoline;
        }
        else
            os_protect((os_vm_address_t)trampoline, size,
                       OS_VM_PROT_READ | OS_VM_PROT_WRITE);

        unsigned int *inst_ptr = trampoline;
        *inst_ptr++ = orig_inst;
        *inst_ptr++ = 0xE59FF000; /* ldr pc, [pc, #0] */
        inst_ptr++;               /* the literal at +12 */
        *(uword_t *)inst_ptr++ = next_pc;
        OS_CONTEXT_PC(context) = (uword_t)trampoline;
        os_flush_icache((os_vm_address_t) trampoline,
                        (char*) inst_ptr - (char*)trampoline);
        os_protect((os_vm_address_t)trampoline, size,
                   OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);
        return;
    }

    /* An instruction interpreted in place: continue after it. */
    OS_CONTEXT_PC(context) = (uword_t)next_pc;
}

void
arch_handle_breakpoint(os_context_t *context)
{
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    OS_CONTEXT_PC(context) = (int) handle_fun_end_breakpoint(context);
}




/* Linkage tables
 *
 * Linkage entry size is 16, because we need 4 instructions.
 */

#define LINKAGE_TEMP_REG        reg_NFP

void arch_write_linkage_table_entry(int index, void *target_addr, int datap)
{
  // allocate successive entries downward
  char *reloc_addr =
      (char*)ALIEN_LINKAGE_SPACE_END - (index + 1) * ALIEN_LINKAGE_TABLE_ENTRY_SIZE;
  if (datap) {
    *(unsigned long *)reloc_addr = (unsigned long)target_addr;
    return;
  }
  /*
    ldr reg, [pc, #4]
    bx  reg
    nop
    address

    BX is needed for thumb interworking, without it it could take just two words with
    ldr pc, [pc, #-4]
    address
  */
  int* inst_ptr;
  unsigned inst;

  inst_ptr = (int*) reloc_addr;

  // ldr reg, [pc, #4]
  inst = 0xe59f0000 | LINKAGE_TEMP_REG << 12 | 4;
  *inst_ptr++ = inst;

  // bx reg
  inst = 0xe12fff10 | LINKAGE_TEMP_REG;
  *inst_ptr++ = inst;

  // nop aka mov r0, r0
  inst = 0xe1a00000;
  *inst_ptr++ = inst;

  // address
  *inst_ptr++ = (int)target_addr;

  os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - reloc_addr);
}
