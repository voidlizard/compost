#ifndef vm_h
#define vm_h

typedef enum cmd {
    op_NOP   = 0,
    op_LOAD  = 1,
    op_MOV   = 2,
    op_ADD   = 3,
    op_SUB   = 4,
    op_DOWN  = 0xFF
} cmd_t;

#define REGS 4

typedef enum reg {
    R1 = 0, R2 = 1, R3 = 2, R4 = 3
} reg_t;

typedef unsigned char byte;
typedef unsigned int  word;
typedef unsigned int  opcode_t;

#define OPCODE_ENC(x)             ((x)<<24)
#define OPCODE3_ENC(x,r1,r2,r3)   OPCODE_ENC((x))|((r1)<<16)|((r2)<<8)|(r3)

#define OPCODE(x)               ((x)>>24)
#define R1(x)                   ((x)>>16)&0xFF
#define R2(x)                   ((x)>>8)&0xFF
#define R3(x)                   ((x))&0xFF

#define NOP()                   OPCODE_ENC(op_NOP)
#define LOAD(r,x)               OPCODE3_ENC(op_LOAD, r, 0, 0), (x)
#define MOV(r1,r2)              OPCODE3_ENC(op_MOV, r1, r2)
#define ADD(r1,r2,r3)           OPCODE3_ENC(op_ADD, r1, r2, r3)
#define SUB(r1,r2,r3)           OPCODE3_ENC(op_SUB, r1, r2, r3)
#define DOWN()                  OPCODE3_ENC(op_DOWN, 0, 0, 0)


typedef struct vm_state {
    word regs[REGS];
} vm_state_t;


#endif

