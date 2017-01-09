
#include "vm.h"

#include <stdlib.h>
#include <stdio.h>

#define NOP8   &&l_NOP,&&l_NOP,&&l_NOP,&&l_NOP,&&l_NOP,&&l_NOP,&&l_NOP,&&l_NOP
#define NOP16  NOP8,  NOP8
#define NOP32  NOP16, NOP16
#define NOP64  NOP32, NOP32
#define NOP128 NOP64, NOP64

#define NEXT() goto *code[OPCODE(*(++op))]

void vm_run( vm_state_t *vm, opcode_t *opcodes )
{
    opcode_t *op = opcodes;
    word *regs = vm->regs;
    word r1 = 0, r2 = 0, r3 = 0;

    static const void * code[256] = {
        &&l_NOP, &&l_LOAD, &&l_MOV, &&l_ADD, &&l_SUB, &&l_NOP, 
        &&l_NOP, &&l_NOP,

        NOP128, NOP64, NOP32, NOP16, 
        &&l_NOP, &&l_NOP, &&l_NOP, &&l_NOP, &&l_NOP, &&l_NOP, &&l_NOP, 
        &&l_DOWN
    };

    goto *code[OPCODE(*op)];

    l_NOP:
        NEXT();
    l_LOAD:
        r1 = R1(*op);
        regs[r1] = *(++op);
        NEXT();
    l_MOV:
        regs[R2(*op)] = regs[R1(*op)];
        NEXT();
    l_ADD:
        regs[R3(*op)] = regs[R1(*op)] + regs[R2(*op)] ;
        NEXT();
    l_SUB:
        regs[R3(*op)] = regs[R1(*op)] - regs[R2(*op)] ;
        NEXT();
    l_DOWN:
        return;

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


