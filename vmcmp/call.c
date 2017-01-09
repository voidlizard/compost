#include "vm.h"

#include <stdlib.h>
#include <stdio.h>

typedef opcode_t* (*op_handler_t)(vm_state_t *, opcode_t *);

opcode_t *nop(vm_state_t *vm, opcode_t *op) {
    return ++op;
}

opcode_t *load(vm_state_t *vm, opcode_t *op) {
    word r1 = R1(*op);
    vm->regs[r1] = *(++op);
    return ++op;
}

opcode_t *mov(vm_state_t *vm, opcode_t *op) {
    word *regs = vm->regs;
    regs[R2(*op)] = regs[R1(*op)];
    return ++op;
}

opcode_t *add(vm_state_t *vm, opcode_t *op) {
    word *regs = vm->regs;
    regs[R3(*op)] = regs[R1(*op)] + regs[R2(*op)] ;
    return ++op;
}

opcode_t *sub(vm_state_t *vm, opcode_t *op) {
    word *regs = vm->regs;
    regs[R3(*op)] = regs[R1(*op)] - regs[R2(*op)] ;
    return ++op;
}

opcode_t *down(vm_state_t *vm, opcode_t *op) {
    return 0;
}


#define NOP8   nop, nop, nop, nop, nop, nop, nop, nop
#define NOP16  NOP8,  NOP8
#define NOP32  NOP16, NOP16
#define NOP64  NOP32, NOP32
#define NOP128 NOP64, NOP64


void vm_run( vm_state_t *vm, opcode_t *opcodes )
{
    opcode_t *op = opcodes;
    
    static const op_handler_t ops[256] = {
        nop, load, mov, add, sub, nop, 
        nop, nop,

        NOP128, NOP64, NOP32, NOP16, 
        nop, nop, nop, nop, nop, nop, nop, 
        down
    };
    
    for(; op; op = ops[OPCODE(*op)](vm, op) );
}


int main(int a, char **b)
{
    unsigned int i = 0;
    vm_state_t vm = { {0} };
    opcode_t opcodes[] = {
        #include "bytecode"
    };
    for(i=0;i<500000;i++)
    {
        vm_run(&vm, opcodes);
    }
    printf("R3: %08X\n", vm.regs[R3]);
    return 0;
}


