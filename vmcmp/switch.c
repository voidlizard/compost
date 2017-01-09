
#include "vm.h"

#include <stdlib.h>
#include <stdio.h>

void vm_run( vm_state_t *vm, opcode_t *opcodes )
{
    opcode_t *op = opcodes;
    cmd_t cmd = 0;
    word *regs = vm->regs;
    word  r1 = 0, r2 = 0, r3 = 0;

    for(;;) {
        cmd = OPCODE(*op);
        r1  = R1(*op);
        r2  = R2(*op);
        r3  = R3(*op);
        switch( cmd ) {
            case op_NOP:
                op++; 
                break;
            case op_LOAD:
                op++; 
                regs[r1] = *op++;
                break;
            case op_MOV:
                regs[r2] = regs[r1];
                op++; 
                break;
            case op_ADD:
                regs[r3] = regs[r1] + regs[r2];
                op++; 
                break;
            case op_SUB:
                regs[r3] = regs[r1] - regs[r2];
                op++; 
                break;
            case op_DOWN:
                op++;
                return;
        }
    }
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


