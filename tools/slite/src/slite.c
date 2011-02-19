/*------------------------------------------------------------------------------
* slite.c -- MIPS-I simulator based on Steve Rhoad's "mlite"
*
* This is a slightly modified version of Steve Rhoad's "mlite" simulator, which
* is part of his PLASMA project (original date: 1/31/01).
*
*-------------------------------------------------------------------------------
* Usage:
*     slite <code file name> <data file name>
*
* The program will allocate a chunk of RAM (MEM_SIZE bytes) and map it to
* address 0x00000000 of the simulated CPU.
* Then it will read the 'code file' (as a big-endian plain binary) onto address
* 0x0 of the simulated CPU memory, and 'data file' on address 0x10000.
* Finally, will reset the CPU and enter the interactive debugger.
*
* (Note that the above is only necessary if the system does not have caches,
* because of the Harvard architecture. With caches in place, program loading
* would be antirely conventional).
*
* A simulation log file will be dumped to file "sw_sim_log.txt". This log can be
* used to compare with an equivalent log dumped by the hardware simulation, as
* a simple way to validate the hardware for a given program. See the project
* readme files for details.
*
*-------------------------------------------------------------------------------
* KNOWN BUGS:
*
*-------------------------------------------------------------------------------
* @date 2011-jan-16
*
*-------------------------------------------------------------------------------
* COPYRIGHT:    Software placed into the public domain by the author.
*               Software 'as is' without warranty.  Author liable for nothing.
*
* IMPORTANT: Assumes host is little endian.
*-----------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

/** Set to !=0 to disable file logging (much faster simulation) */
#define FILE_LOGGING_DISABLED (0)
/** Define to enable cache simulation (unimplemented) */
//#define ENABLE_CACHE


/*---- Definition of simulated system parameters -----------------------------*/

/* Uncomment to simulate Plasma behavior (vectors & memory mapping) */
//#define SIMULATE_PLASMA (1)

#ifdef SIMULATE_PLASMA

#define VECTOR_RESET (0x00000000)
#define VECTOR_TRAP  (0x0000003c)

#else

#define VECTOR_RESET (0xbfc00000)
#define VECTOR_TRAP  (0xbfc00180)

#endif

/** Definition of a memory block */
typedef struct s_block {
    uint32_t start;
    uint32_t size;
    uint32_t mask;
    uint32_t read_only;
    uint8_t  *mem;
    char     *area_name;
} t_block;


/*  Here's where we define the memory areas (blocks) of the system.

    The blocks should be defined in this order: BRAM, XRAM, FLASH

    BRAM is FPGA block ram initialized with bootstrap code
    XRAM is external SRAM
    FLASH is external flash

    Give any area a size of 0x0 to leave it unused.

    When a binary file is specified in the cmd line for one of these areas, it
    will be used to initialize it, checking bounds.


    Memory decoding is done in the order the blocks are defined; the address
    is anded with field .mask and then compared to field .start. If they match
    the address modulo the field .size is used to index the memory block, giving
    a 'mirror' effect. All of this simulates how the actual hardware works.
    Make sure the blocks don't overlap or the scheme will fail.
*/

#define NUM_MEM_BLOCKS (3)

#ifdef SIMULATE_PLASMA

t_block memory_map_default[NUM_MEM_BLOCKS] = {
    /* meant as bootstrap block, though it's read/write */
    {VECTOR_RESET,  0x00000800, 0xf0000000, 1, NULL, "Boot BRAM"},
    /* main external ram block  */
    {0x80000000,    0x00001000, 0xf0000000, 0, NULL, "XRAM"},
    /* external flash block -- not used in plasma simulation */
    {0xb0000000,    0x00000000, 0xf0000000, 0, NULL, "Flash"},
};

#else

t_block memory_map_default[NUM_MEM_BLOCKS] = {
    /* Bootstrap BRAM, read only */
    {VECTOR_RESET,  0x00004800, 0xf8000000, 1, NULL, "Boot BRAM"},
    /* main external ram block  */
    {0x00000000,    0x00080000, 0xf8000000, 0, NULL, "XRAM"},
    /* external flash block */
    {0xb0000000,    0x00040000, 0xf8000000, 0, NULL, "Flash"},
};

#endif

/*---- end of system parameters ----------------------------------------------*/


/** Values for the command line arguments */
typedef struct s_args {
    uint32_t log_trigger_address;
    char *log_file_name;
    char *bin_filename[NUM_MEM_BLOCKS]; /**< bin file to load to area or null */
} t_args;
/** Parse cmd line args globally accessible */
t_args cmd_line_args;


/*---- Endianess conversion macros -------------------------------------------*/

#define ntohs(A) ( ((A)>>8) | (((A)&0xff)<<8) )
#define htons(A) ntohs(A)
#define ntohl(A) ( ((A)>>24) | (((A)&0xff0000)>>8) | (((A)&0xff00)<<8) | ((A)<<24) )
#define htonl(A) ntohl(A)

/*---- OS-dependent support functions and definitions ------------------------*/
#ifndef WIN32
//Support for Linux
#define putch putchar
#include <termios.h>
#include <unistd.h>

void slite_sleep(unsigned int value){
    usleep(value * 1000);
}

int kbhit(void){
    struct termios oldt, newt;
    struct timeval tv;
    fd_set read_fd;

    tcgetattr(STDIN_FILENO, &oldt);
    newt = oldt;
    newt.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
    tv.tv_sec=0;
    tv.tv_usec=0;
    FD_ZERO(&read_fd);
    FD_SET(0,&read_fd);
    if(select(1, &read_fd, NULL, NULL, &tv) == -1){
        return 0;
    }
    //tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    if(FD_ISSET(0,&read_fd)){
        return 1;
    }
    return 0;
}

int getch(void){
    struct termios oldt, newt;
    int ch;

    tcgetattr(STDIN_FILENO, &oldt);
    newt = oldt;
    newt.c_lflag &= ~(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &newt);
    ch = getchar();
    //tcsetattr(STDIN_FILENO, TCSANOW, &oldt);
    return ch;
}
#else
//Support for Windows
#include <conio.h>
extern void __stdcall Sleep(unsigned long value);

void slite_sleep(unsigned int value){
    Sleep(value);
}

#endif
/*---- End of OS-dependent support functions and definitions -----------------*/

/*---- Hardware system parameters --------------------------------------------*/

/* Much of this is a remnant from Plasma's mlite and is  no longer used. */
/* FIXME Refactor HW system params */

#define UART_WRITE        0x20000000
#define UART_READ         0x20000000
#define IRQ_MASK          0x20000010
#define IRQ_STATUS        0x20000020
#define CONFIG_REG        0x20000070
#define MMU_PROCESS_ID    0x20000080
#define MMU_FAULT_ADDR    0x20000090
#define MMU_TLB           0x200000a0

#define IRQ_UART_READ_AVAILABLE  0x001
#define IRQ_UART_WRITE_AVAILABLE 0x002
#define IRQ_COUNTER18_NOT        0x004
#define IRQ_COUNTER18            0x008
#define IRQ_MMU                  0x200

/*----------------------------------------------------------------------------*/

/* These are flags that will be used to notify the main cycle function of any
   failed assertions in its subfunctions. */
#define ASRT_UNALIGNED_READ         (1<<0)
#define ASRT_UNALIGNED_WRITE        (1<<1)

char *assertion_messages[2] = {
   "Unaligned read",
   "Unaligned write"
};


/** Length of debugging jump target queue */
#define TRACE_BUFFER_SIZE (32)

typedef struct s_trace {
   unsigned int buf[TRACE_BUFFER_SIZE];   /**< queue of last jump targets */
   unsigned int next;                     /**< internal queue head pointer */
   FILE *log;                             /**< text log file or NULL */
   int log_triggered;                     /**< !=0 if log has been triggered */
   uint32_t log_trigger_address;          /**< */
   int pr[32];                            /**< last value of register bank */
   int hi, lo, epc;                       /**< last value of internal regs */
} t_trace;

typedef struct s_state {
   unsigned failed_assertions;            /**< assertion bitmap */
   unsigned faulty_address;               /**< addr that failed assertion */

   int delay_slot;              /**< !=0 if prev. instruction was a branch */

   int r[32];
   int opcode;
   int pc, pc_next, epc;
   uint32_t op_addr;            /**< address of opcode being simulated */
   unsigned int hi;
   unsigned int lo;
   int status;
   unsigned cp0_cause;
   int userMode;
   int processId;
   int exceptionId;             /**< DEPRECATED, to be removed */
   int faultAddr;
   int irqStatus;
   int skip;
   t_trace t;
   //unsigned char *mem;
   t_block blocks[NUM_MEM_BLOCKS];
   int wakeup;
   int big_endian;
} t_state;

static char *opcode_string[]={
   "SPECIAL","REGIMM","J","JAL","BEQ","BNE","BLEZ","BGTZ",
   "ADDI","ADDIU","SLTI","SLTIU","ANDI","ORI","XORI","LUI",
   "COP0","COP1","COP2","COP3","BEQL","BNEL","BLEZL","BGTZL",
   "?","?","?","?","?","?","?","?",
   "LB","LH","LWL","LW","LBU","LHU","LWR","?",
   "SB","SH","SWL","SW","?","?","SWR","CACHE",
   "LL","LWC1","LWC2","LWC3","?","LDC1","LDC2","LDC3"
   "SC","SWC1","SWC2","SWC3","?","SDC1","SDC2","SDC3"
};

static char *special_string[]={
   "SLL","?","SRL","SRA","SLLV","?","SRLV","SRAV",
   "JR","JALR","MOVZ","MOVN","SYSCALL","BREAK","?","SYNC",
   "MFHI","MTHI","MFLO","MTLO","?","?","?","?",
   "MULT","MULTU","DIV","DIVU","?","?","?","?",
   "ADD","ADDU","SUB","SUBU","AND","OR","XOR","NOR",
   "?","?","SLT","SLTU","?","DADDU","?","?",
   "TGE","TGEU","TLT","TLTU","TEQ","?","TNE","?",
   "?","?","?","?","?","?","?","?"
};

static char *regimm_string[]={
   "BLTZ","BGEZ","BLTZL","BGEZL","?","?","?","?",
   "TGEI","TGEIU","TLTI","TLTIU","TEQI","?","TNEI","?",
   "BLTZAL","BEQZAL","BLTZALL","BGEZALL","?","?","?","?",
   "?","?","?","?","?","?","?","?"
};

static unsigned int HWMemory[8];

/*---- Local function prototypes ---------------------------------------------*/

/* Debug and logging */
void init_trace_buffer(t_state *s, t_args *args);
void close_trace_buffer(t_state *s);
void dump_trace_buffer(t_state *s);
void log_cycle(t_state *s);
void log_read(t_state *s, int full_address, int word_value, int size, int log);
void log_failed_assertions(t_state *s);
uint32_t log_enabled(t_state *s);
void trigger_log(t_state *s);

int32_t parse_cmd_line(uint32_t argc, char **argv, t_args *args);
void usage(void);

/* CPU model */
void free_cpu(t_state *s);
int init_cpu(t_state *s);
void reset_cpu(t_state *s);

/* Hardware simulation */
int mem_read(t_state *s, int size, unsigned int address, int log);
void mem_write(t_state *s, int size, unsigned address, unsigned value, int log);
void start_load(t_state *s, uint32_t addr, int rt, int data);


/*---- Local functions -------------------------------------------------------*/

/** Log to file a memory read operation (not including target reg change) */
void log_read(t_state *s, int full_address, int word_value, int size, int log){
    if(log_enabled(s) && log!=0){
        fprintf(s->t.log, "(%08X) [%08X] <**>=%08X RD\n",
              s->op_addr, full_address, word_value);
    }
}

/** Read memory, optionally logging */
int mem_read(t_state *s, int size, unsigned int address, int log){
    unsigned int value=0, word_value=0, i, ptr;
    unsigned int full_address = address;

    s->irqStatus |= IRQ_UART_WRITE_AVAILABLE;
    switch(address){
    case UART_READ:
        /* FIXME Take input from text file */
        while(!kbhit());
        HWMemory[0] = getch();
        //s->irqStatus &= ~IRQ_UART_READ_AVAILABLE; //clear bit
        printf("%c", HWMemory[0]);
        return (HWMemory[0] << 24) | 0x03;
    case IRQ_MASK:
       return HWMemory[1];
    case IRQ_MASK + 4:
       slite_sleep(10);
       return 0;
    case IRQ_STATUS:
       /*if(kbhit())
          s->irqStatus |= IRQ_UART_READ_AVAILABLE;
       return s->irqStatus;
       */
       /* FIXME Optionally simulate UART TX delay */
       word_value = 0x00000003; /* Ready to TX and RX */
       //log_read(s, full_address, word_value, size, log);
       return word_value;
    case MMU_PROCESS_ID:
       return s->processId;
    case MMU_FAULT_ADDR:
       return s->faultAddr;
    }

    /* point ptr to the byte in the block, or NULL is the address is unmapped */
    ptr = 0;
    for(i=0;i<NUM_MEM_BLOCKS;i++){
        if((address & s->blocks[i].mask) ==
           (s->blocks[i].start & s->blocks[i].mask)){
            ptr = (unsigned)(s->blocks[i].mem) +
                  ((address - s->blocks[i].start) % s->blocks[i].size);
            break;
        }
    }
    if(!ptr){
        /* address out of mapped blocks: log and return zero */
        if(log_enabled(s) && log!=0){
            fprintf(s->t.log, "(%08X) [%08X] <**>=%08X RD UNMAPPED\n",
                s->pc, full_address, 0);
        }
        return 0;
    }

    /* get the whole word */
    word_value = *(int*)(ptr&0xfffffffc);
    if(s->big_endian){
        word_value = ntohl(word_value);
    }

    switch(size){
    case 4:
        if(address & 3){
            printf("Unaligned access PC=0x%x address=0x%x\n",
                (int)s->pc, (int)address);
        }
        if((address & 3) != 0){
            /* unaligned word, log fault */
            s->failed_assertions |= ASRT_UNALIGNED_READ;
            s->faulty_address = address;
            address = address & 0xfffffffc;
        }
        value = *(int*)ptr;
        if(s->big_endian){
            value = ntohl(value);
        }
        break;
    case 2:
        if((address & 1) != 0){
            /* unaligned halfword, log fault */
            s->failed_assertions |= ASRT_UNALIGNED_READ;
            s->faulty_address = address;
            address = address & 0xfffffffe;
        }
        value = *(unsigned short*)ptr;
        if(s->big_endian){
            value = ntohs((unsigned short)value);
        }
        break;
    case 1:
        value = *(unsigned char*)ptr;
        break;
    default:
        /* FIXME this is a bug, should quit */
        printf("ERROR");
    }

    //log_read(s, full_address, value, size, log);
    return(value);
}

/** Write memory */
void mem_write(t_state *s, int size, unsigned address, unsigned value, int log){
    unsigned int i, ptr, mask, dvalue, b0, b1, b2, b3;

    if(log_enabled(s)){
        b0 = value & 0x000000ff;
        b1 = value & 0x0000ff00;
        b2 = value & 0x00ff0000;
        b3 = value & 0xff000000;

        switch(size){
        case 4:  mask = 0x0f;
            dvalue = value;
            break;
        case 2:
            if((address&0x2)==0){
                mask = 0xc;
                dvalue = b1<<16 | b0<<16;
            }
            else{
               mask = 0x3;
               dvalue = b1 | b0;
            }
            break;
        case 1:
            switch(address%4){
            case 0 : mask = 0x8;
                dvalue = b0<<24;
                break;
            case 1 : mask = 0x4;
                dvalue = b0<<16;
                break;
            case 2 : mask = 0x2;
                dvalue = b0<<8;
                break;
            case 3 : mask = 0x1;
                dvalue = b0;
                break;
            }
            break;
        default:
            printf("BUG: mem write size invalid (%08x)\n", s->pc);
            exit(2);
        }

        fprintf(s->t.log, "(%08X) [%08X] |%02X|=%08X WR\n",
                s->op_addr, address&0xfffffffc, mask, dvalue);
    }

    switch(address){
    case UART_WRITE:
        putch(value);
        fflush(stdout);
        return;
    case IRQ_MASK:
        HWMemory[1] = value;
        return;
    case IRQ_STATUS:
        s->irqStatus = value;
        return;
    case CONFIG_REG:
        return;
    case MMU_PROCESS_ID:
        //printf("processId=%d\n", value);
        s->processId = value;
        return;
    }

    ptr = 0;
    for(i=0;i<NUM_MEM_BLOCKS;i++){
        if((address & s->blocks[i].mask) ==
                  (s->blocks[i].start & s->blocks[i].mask)){
            ptr = (unsigned)(s->blocks[i].mem) +
                            ((address - s->blocks[i].start) % s->blocks[i].size);

            if(s->blocks[i].read_only){
                if(log_enabled(s) && log!=0){
                    fprintf(s->t.log, "(%08X) [%08X] |%02X|=%08X WR READ ONLY\n",
                    s->op_addr, address, mask, dvalue);
                    return;
                }
            }
            break;
        }
    }
    if(!ptr){
        /* address out of mapped blocks: log and return zero */
        if(log_enabled(s) && log!=0){
            fprintf(s->t.log, "(%08X) [%08X] |%02X|=%08X WR UNMAPPED\n",
                s->op_addr, address, mask, dvalue);
        }
        return;
    }

    switch(size){
    case 4:
        if((address & 3) != 0){
            /* unaligned word, log fault */
            s->failed_assertions |= ASRT_UNALIGNED_WRITE;
            s->faulty_address = address;
            address = address & (~0x03);
        }
        if(s->big_endian){
            value = htonl(value);
        }
        *(int*)ptr = value;
        break;
    case 2:
        if((address & 1) != 0){
            /* unaligned halfword, log fault */
            s->failed_assertions |= ASRT_UNALIGNED_WRITE;
            s->faulty_address = address;
            address = address & (~0x01);
        }
        if(s->big_endian){
            value = htons((unsigned short)value);
        }
        *(short*)ptr = (unsigned short)value;
        break;
    case 1:
        *(char*)ptr = (unsigned char)value;
        break;
    default:
        /* FIXME this is a bug, should quit */
        printf("ERROR");
    }
}

/*---- Optional MMU and cache implementation ---------------------------------*/

/*
   The actual core does not have a cache so all of the original Plasma mlite.c
   code for cache simulation has been removed.
*/

/*---- End optional cache implementation -------------------------------------*/


/** Simulates MIPS-I multiplier unsigned behavior*/
void mult_big(unsigned int a,
              unsigned int b,
              unsigned int *hi,
              unsigned int *lo){
    unsigned int ahi, alo, bhi, blo;
    unsigned int c0, c1, c2;
    unsigned int c1_a, c1_b;

    ahi = a >> 16;
    alo = a & 0xffff;
    bhi = b >> 16;
    blo = b & 0xffff;

    c0 = alo * blo;
    c1_a = ahi * blo;
    c1_b = alo * bhi;
    c2 = ahi * bhi;

    c2 += (c1_a >> 16) + (c1_b >> 16);
    c1 = (c1_a & 0xffff) + (c1_b & 0xffff) + (c0 >> 16);
    c2 += (c1 >> 16);
    c0 = (c1 << 16) + (c0 & 0xffff);
    *hi = c2;
    *lo = c0;
}

/** Simulates MIPS-I multiplier signed behavior*/
void mult_big_signed(int a,
                     int b,
                     unsigned int *hi,
                     unsigned int *lo){
    int64_t xa, xb, xr, temp;
    int32_t rh, rl;

    xa = a;
    xb = b;
    xr = xa * xb;

    temp = (xr >> 32) & 0xffffffff;
    rh = temp;
    temp = (xr >> 0) & 0xffffffff;
    rl = temp;

    *hi = rh;
    *lo = rl;
}

/** Load data from memory (used to simulate load delay slots) */
void start_load(t_state *s, uint32_t addr, int rt, int data){
    /* load delay slot not simulated */
    log_read(s, addr, data, 1, 1);
    s->r[rt] = data;
}

/** Execute one cycle of the CPU (including any interlock stall cycles) */
void cycle(t_state *s, int show_mode){
    unsigned int opcode;
    int delay_slot = 0; /* 1 of this instruction is a branch */
    unsigned int op, rs, rt, rd, re, func, imm, target;
    int trap_cause = 0;
    int imm_shift, branch=0, lbranch=2, skip2=0;
    int *r=s->r;
    unsigned int *u=(unsigned int*)s->r;
    unsigned int ptr, epc, rSave;

    /* fetch and decode instruction */
    opcode = mem_read(s, 4, s->pc, 0);
    op = (opcode >> 26) & 0x3f;
    rs = (opcode >> 21) & 0x1f;
    rt = (opcode >> 16) & 0x1f;
    rd = (opcode >> 11) & 0x1f;
    re = (opcode >> 6) & 0x1f;
    func = opcode & 0x3f;
    imm = opcode & 0xffff;
    imm_shift = (((int)(short)imm) << 2) - 4;
    target = (opcode << 6) >> 4;
    ptr = (short)imm + r[rs];
    r[0] = 0;

    /* Trigger log if we fetch from trigger address */
    if(s->pc == s->t.log_trigger_address){
        trigger_log(s);
    }

    /* if we are priting state to console, do it now */
    if(show_mode){
        printf("%8.8x %8.8x ", s->pc, opcode);
        if(op == 0){
            printf("%8s ", special_string[func]);
        }
        else if(op == 1){
            printf("%8s ", regimm_string[rt]);
        }
        else{
            printf("%8s ", opcode_string[op]);
        }

        printf("$%2.2d $%2.2d $%2.2d $%2.2d ", rs, rt, rd, re);
        printf("%4.4x", imm);
        if(show_mode == 1){
            printf(" r[%2.2d]=%8.8x r[%2.2d]=%8.8x", rs, r[rs], rt, r[rt]);
        }
        printf("\n");
    }

    /* if we're just showing state to console, quit and don't run instruction */
    if(show_mode > 5){
        return;
    }

    /* epc will point to the victim instruction, i.e. THIS instruction */
    epc = s->pc;

    /* If we catch a jump instruction jumping to itself, assume we hit the
       and of the program and quit. */
    if(s->pc == s->pc_next+4){
        printf("\n\nEndless loop at 0x%08x\n\n", s->pc-4);
        s->wakeup = 1;
    }
    s->op_addr = s->pc;
    s->pc = s->pc_next;
    s->pc_next = s->pc_next + 4;
    if(s->skip){
        s->skip = 0;
        return;
    }
    rSave = r[rt];

    switch(op){
    case 0x00:/*SPECIAL*/
        switch(func){
        case 0x00:/*SLL*/  r[rd]=r[rt]<<re;          break;
        case 0x02:/*SRL*/  r[rd]=u[rt]>>re;          break;
        case 0x03:/*SRA*/  r[rd]=r[rt]>>re;          break;
        case 0x04:/*SLLV*/ r[rd]=r[rt]<<r[rs];       break;
        case 0x06:/*SRLV*/ r[rd]=u[rt]>>r[rs];       break;
        case 0x07:/*SRAV*/ r[rd]=r[rt]>>r[rs];       break;
        case 0x08:/*JR*/   delay_slot=1;
                           s->pc_next=r[rs];         break;
        case 0x09:/*JALR*/ delay_slot=1;
                           r[rd]=s->pc_next;
                           s->pc_next=r[rs]; break;
        case 0x0a:/*MOVZ*/ if(!r[rt]) r[rd]=r[rs];   break;  /*IV*/
        case 0x0b:/*MOVN*/ if(r[rt]) r[rd]=r[rs];    break;  /*IV*/
        case 0x0c:/*SYSCALL*/ trap_cause = 8;
                              s->exceptionId=1; break;
        case 0x0d:/*BREAK*/   trap_cause = 9;
                              s->exceptionId=1; break;
        case 0x0f:/*SYNC*/ s->wakeup=1;              break;
        case 0x10:/*MFHI*/ r[rd]=s->hi;              break;
        case 0x11:/*FTHI*/ s->hi=r[rs];              break;
        case 0x12:/*MFLO*/ r[rd]=s->lo;              break;
        case 0x13:/*MTLO*/ s->lo=r[rs];              break;
        case 0x18:/*MULT*/ mult_big_signed(r[rs],r[rt],&s->hi,&s->lo); break;
        case 0x19:/*MULTU*/ mult_big(r[rs],r[rt],&s->hi,&s->lo); break;
        case 0x1a:/*DIV*/  s->lo=r[rs]/r[rt]; s->hi=r[rs]%r[rt]; break;
        case 0x1b:/*DIVU*/ s->lo=u[rs]/u[rt]; s->hi=u[rs]%u[rt]; break;
        case 0x20:/*ADD*/  r[rd]=r[rs]+r[rt];        break;
        case 0x21:/*ADDU*/ r[rd]=r[rs]+r[rt];        break;
        case 0x22:/*SUB*/  r[rd]=r[rs]-r[rt];        break;
        case 0x23:/*SUBU*/ r[rd]=r[rs]-r[rt];        break;
        case 0x24:/*AND*/  r[rd]=r[rs]&r[rt];        break;
        case 0x25:/*OR*/   r[rd]=r[rs]|r[rt];        break;
        case 0x26:/*XOR*/  r[rd]=r[rs]^r[rt];        break;
        case 0x27:/*NOR*/  r[rd]=~(r[rs]|r[rt]);     break;
        case 0x2a:/*SLT*/  r[rd]=r[rs]<r[rt];        break;
        case 0x2b:/*SLTU*/ r[rd]=u[rs]<u[rt];        break;
        case 0x2d:/*DADDU*/r[rd]=r[rs]+u[rt];        break;
        case 0x31:/*TGEU*/ break;
        case 0x32:/*TLT*/  break;
        case 0x33:/*TLTU*/ break;
        case 0x34:/*TEQ*/  break;
        case 0x36:/*TNE*/  break;
        default: printf("ERROR0(*0x%x~0x%x)\n", s->pc, opcode);
           s->wakeup=1;
        }
        break;
    case 0x01:/*REGIMM*/
        switch(rt){
            case 0x10:/*BLTZAL*/ r[31]=s->pc_next;
            case 0x00:/*BLTZ*/   branch=r[rs]<0;    break;
            case 0x11:/*BGEZAL*/ r[31]=s->pc_next;
            case 0x01:/*BGEZ*/   branch=r[rs]>=0;   break;
            case 0x12:/*BLTZALL*/r[31]=s->pc_next;
            case 0x02:/*BLTZL*/  lbranch=r[rs]<0;   break;
            case 0x13:/*BGEZALL*/r[31]=s->pc_next;
            case 0x03:/*BGEZL*/  lbranch=r[rs]>=0;  break;
            default: printf("ERROR1\n"); s->wakeup=1;
        }
        break;
    case 0x03:/*JAL*/    r[31]=s->pc_next;
    case 0x02:/*J*/      delay_slot=1;
                       s->pc_next=(s->pc&0xf0000000)|target; break;
    case 0x04:/*BEQ*/    branch=r[rs]==r[rt];     break;
    case 0x05:/*BNE*/    branch=r[rs]!=r[rt];     break;
    case 0x06:/*BLEZ*/   branch=r[rs]<=0;         break;
    case 0x07:/*BGTZ*/   branch=r[rs]>0;          break;
    case 0x08:/*ADDI*/   r[rt]=r[rs]+(short)imm;  break;
    case 0x09:/*ADDIU*/  u[rt]=u[rs]+(short)imm;  break;
    case 0x0a:/*SLTI*/   r[rt]=r[rs]<(short)imm;  break;
    case 0x0b:/*SLTIU*/  u[rt]=u[rs]<(unsigned int)(short)imm; break;
    case 0x0c:/*ANDI*/   r[rt]=r[rs]&imm;         break;
    case 0x0d:/*ORI*/    r[rt]=r[rs]|imm;         break;
    case 0x0e:/*XORI*/   r[rt]=r[rs]^imm;         break;
    case 0x0f:/*LUI*/    r[rt]=(imm<<16);         break;
    case 0x10:/*COP0*/
        if((opcode & (1<<23)) == 0){  //move from CP0
            if(rd == 12){
                r[rt]=s->status;
            }
            else if(rd == 13){
                r[rt]=s->cp0_cause;
            }
            else{
                r[rt]=s->epc;
            }
        }
        else{                         //move to CP0
            s->status=r[rt]&1;
            if(s->processId && (r[rt]&2)){
                s->userMode|=r[rt]&2;
                //printf("CpuStatus=%d %d %d\n", r[rt], s->status, s->userMode);
                //s->wakeup = 1;
                //printf("pc=0x%x\n", epc);
            }
        }
        break;
//      case 0x11:/*COP1*/ break;
//      case 0x12:/*COP2*/ break;
//      case 0x13:/*COP3*/ break;
    case 0x14:/*BEQL*/  lbranch=r[rs]==r[rt];    break;
    case 0x15:/*BNEL*/  lbranch=r[rs]!=r[rt];    break;
    case 0x16:/*BLEZL*/ lbranch=r[rs]<=0;        break;
    case 0x17:/*BGTZL*/ lbranch=r[rs]>0;         break;
//      case 0x1c:/*MAD*/  break;   /*IV*/
    case 0x20:/*LB*/    //r[rt]=(signed char)mem_read(s,1,ptr,1);  break;
                        start_load(s, ptr, rt,(signed char)mem_read(s,1,ptr,1));
                        break;

    case 0x21:/*LH*/    //r[rt]=(signed short)mem_read(s,2,ptr,1); break;
                        start_load(s, ptr, rt, (signed short)mem_read(s,2,ptr,1));
                        break;
    case 0x22:/*LWL*/   //target=8*(ptr&3);
                        //r[rt]=(r[rt]&~(0xffffffff<<target))|
                        //      (mem_read(s,4,ptr&~3)<<target); break;
                        break;
    case 0x23:/*LW*/    //r[rt]=mem_read(s,4,ptr,1);   break;
                        start_load(s, ptr, rt, mem_read(s,4,ptr,1));
                        break;
    case 0x24:/*LBU*/   //r[rt]=(unsigned char)mem_read(s,1,ptr,1); break;
                        start_load(s, ptr, rt, (unsigned char)mem_read(s,1,ptr,1));
                        break;
    case 0x25:/*LHU*/   //r[rt]= (unsigned short)mem_read(s,2,ptr,1);
                        start_load(s, ptr, rt, (unsigned short)mem_read(s,2,ptr,1));
                        break;
    case 0x26:/*LWR*/   //target=32-8*(ptr&3);
                        //r[rt]=(r[rt]&~((unsigned int)0xffffffff>>target))|
                        //((unsigned int)mem_read(s,4,ptr&~3)>>target);
                        break;
    case 0x28:/*SB*/    mem_write(s,1,ptr,r[rt],1);  break;
    case 0x29:/*SH*/    mem_write(s,2,ptr,r[rt],1);  break;
    case 0x2a:/*SWL*/   //mem_write(s,1,ptr,r[rt]>>24);
                        //mem_write(s,1,ptr+1,r[rt]>>16);
                        //mem_write(s,1,ptr+2,r[rt]>>8);
                        //mem_write(s,1,ptr+3,r[rt]); break;
    case 0x2b:/*SW*/    mem_write(s,4,ptr,r[rt],1);  break;
    case 0x2e:/*SWR*/   break; //FIXME
    case 0x2f:/*CACHE*/ break;
    case 0x30:/*LL*/    //r[rt]=mem_read(s,4,ptr);   break;
                        start_load(s, ptr, rt, mem_read(s,4,ptr,1));
                        break;
//      case 0x31:/*LWC1*/ break;
//      case 0x32:/*LWC2*/ break;
//      case 0x33:/*LWC3*/ break;
//      case 0x35:/*LDC1*/ break;
//      case 0x36:/*LDC2*/ break;
//      case 0x37:/*LDC3*/ break;
//      case 0x38:/*SC*/     *(int*)ptr=r[rt]; r[rt]=1; break;
    case 0x38:/*SC*/    mem_write(s,4,ptr,r[rt],1); r[rt]=1; break;
//      case 0x39:/*SWC1*/ break;
//      case 0x3a:/*SWC2*/ break;
//      case 0x3b:/*SWC3*/ break;
//      case 0x3d:/*SDC1*/ break;
//      case 0x3e:/*SDC2*/ break;
//      case 0x3f:/*SDC3*/ break;
    default:
        /* FIXME should trap unimplemented opcodes */
        printf("ERROR2 address=0x%x opcode=0x%x\n", s->pc, opcode);
        s->wakeup=1;
    }

    /* adjust next PC if this was a ajump instruction */
    s->pc_next += (branch || lbranch == 1) ? imm_shift : 0;
    s->pc_next &= ~3;
    s->skip = (lbranch == 0) | skip2;

    /* If there was trouble (failed assertions), log it */
    if(s->failed_assertions!=0){
        log_failed_assertions(s);
        s->failed_assertions=0;
    }

    /* if there's a delayed load pending, do it now: load reg with memory data*/
    /* load delay slots not simulated */

    /* Handle exceptions */
   if(s->exceptionId){
        r[rt] = rSave;
        s->cp0_cause = (s->delay_slot & 0x1) << 31 | (trap_cause & 0x1f);
        /* adjust epc if we (i.e. the victim instruction) are in a delay slot */
        if(s->delay_slot){
        epc = epc - 4;
        }
        s->epc = epc;
        s->pc_next = VECTOR_TRAP;
        s->skip = 1;
        s->exceptionId = 0;
        s->userMode = 0;
        //s->wakeup = 1;
    }

    /* if we're NOT showing output to console, log state of CPU to file */
    if(!show_mode){
        log_cycle(s);
    }



    /* if this instruction was any kind of branch that actually jumped, then
       the next instruction will be in a delay slot. Remember it. */
    delay_slot = ((lbranch==1) || branch || delay_slot);
    s->delay_slot = delay_slot;
}

/** Dump CPU state to console */
void show_state(t_state *s){
    int i,j;
    printf("pid=%d userMode=%d, epc=0x%x\n", s->processId, s->userMode, s->epc);
    printf("hi=0x%08x lo=0x%08x\n", s->hi, s->lo);
    for(i = 0; i < 4; ++i){
        printf("%2.2d ", i * 8);
        for(j = 0; j < 8; ++j){
            printf("%8.8x ", s->r[i*8+j]);
        }
        printf("\n");
    }
    //printf("%8.8lx %8.8lx %8.8lx %8.8lx\n", s->pc, s->pc_next, s->hi, s->lo);
    j = s->pc;
    for(i = -4; i <= 8; ++i){
        printf("%c", i==0 ? '*' : ' ');
        s->pc = j + i * 4;
        cycle(s, 10);
    }
    s->pc = j;
}

/** Show debug monitor prompt and execute user command */
void do_debug(t_state *s){
    int ch;
    int i, j=0, watch=0, addr;
    s->pc_next = s->pc + 4;
    s->skip = 0;
    s->wakeup = 0;
    show_state(s);
    ch = ' ';
    for(;;){
        if(ch != 'n'){
            if(watch){
                printf("0x%8.8x=0x%8.8x\n", watch, mem_read(s, 4, watch,0));
            }
            printf("1=Debug 2=t_trace 3=Step 4=BreakPt 5=Go 6=Memory ");
            printf("7=Watch 8=Jump 9=Quit A=Dump\n");
            printf("L=LogTrigger > ");
        }
        ch = getch();
        if(ch != 'n'){
            printf("\n");
        }
        switch(ch){
        case 'a': case 'A':
            dump_trace_buffer(s); break;
        case '1': case 'd': case ' ':
            cycle(s, 0); show_state(s); break;
        case 'n':
            cycle(s, 1); break;
        case '2': case 't':
            cycle(s, 0); printf("*"); cycle(s, 10); break;
        case '3': case 's':
            printf("Count> ");
            scanf("%d", &j);
            for(i = 0; i < j; ++i){
                cycle(s, 1);
            }
            show_state(s);
            break;
        case '4': case 'b':
            printf("Line> ");
            scanf("%x", &j);
            printf("break point=0x%x\n", j);
            break;
        case '5': case 'g':
            s->wakeup = 0;
            cycle(s, 0);
            while(s->wakeup == 0){
                if(s->pc == j){
                    printf("\n\nStop: pc = 0x%08x\n\n", j);
                    break;
                }
                cycle(s, 0);
            }
            show_state(s);
            break;
        case 'G':
            s->wakeup = 0;
            cycle(s, 1);
            while(s->wakeup == 0){
                if(s->pc == j){
                    break;
                }
                cycle(s, 1);
            }
            show_state(s);
            break;
        case '6': case 'm':
            printf("Memory> ");
            scanf("%x", &j);
            for(i = 0; i < 8; ++i){
                printf("%8.8x ", mem_read(s, 4, j+i*4, 0));
            }
            printf("\n");
            break;
        case '7': case 'w':
            printf("Watch> ");
            scanf("%x", &watch);
            break;
        case '8': case 'j':
            printf("Jump> ");
            scanf("%x", &addr);
            s->pc = addr;
            s->pc_next = addr + 4;
            show_state(s);
            break;
        case '9': case 'q':
            return;
        case 'l':
            printf("Address> ");
            scanf("%x", &(s->t.log_trigger_address));
            printf("Log trigger address=0x%x\n", s->t.log_trigger_address);
            break;
        }
    }
}

/** Read binary code and data files */
int read_binary_files(t_state *s, t_args *args){
    FILE *in;
    uint32_t bytes, i, files_read=0;

    for(i=0;i<NUM_MEM_BLOCKS;i++){
        if(args->bin_filename[i]!=NULL){

            in = fopen(args->bin_filename[i], "rb");
            if(in == NULL){
                free_cpu(s);
                printf("Can't open file %s, quitting!\n",args->bin_filename[i]);
                return(0);
            }

            bytes = fread(s->blocks[i].mem, 1, s->blocks[i].size, in);
            fclose(in);
            printf("%-16s [size= %6dKB, start= 0x%08x] loaded %d bytes.\n",
                    s->blocks[i].area_name,
                    s->blocks[i].size/1024,
                    s->blocks[i].start,
                    bytes);
            files_read++;
        }
    }

    if(!files_read){
        free_cpu(s);
        printf("No binary object files read, quitting\n");
        return 0;
    }

    return files_read;
}

/*----------------------------------------------------------------------------*/

int main(int argc,char *argv[]){
    t_state state, *s=&state;

    printf("MIPS-I emulator (" __DATE__ ")\n\n");
    if(!init_cpu(s)){
        printf("Trouble allocating memory, quitting!\n");
        return 1;
    };

    /* Parse command line and pass any relevant arguments to CPU record */
    if(parse_cmd_line(argc,argv, &cmd_line_args)==0){
        return 0;
    }

    /* Read binary object files into memory*/
    if(!read_binary_files(s, &cmd_line_args)){
        return 2;
    }
    printf("\n\n");

    init_trace_buffer(s, &cmd_line_args);

    /* NOTE: Original mlite supported loading little-endian code, which this
      program doesn't. The endianess-conversion code has been removed.
    */

    /* Simulate a CPU reset */
    reset_cpu(s);

    /* Enter debug command interface; will only exit clean with user command */
    do_debug(s);

    /* Close and deallocate everything and quit */
    close_trace_buffer(s);
    free_cpu(s);
    return(0);
}

/*----------------------------------------------------------------------------*/


void init_trace_buffer(t_state *s, t_args *args){
    int i;

#if FILE_LOGGING_DISABLED
    s->t.log = NULL;
    s->t.log_triggered = 0;
    return;
#else
    /* clear trace buffer */
    for(i=0;i<TRACE_BUFFER_SIZE;i++){
        s->t.buf[i]=0xffffffff;
    }
    s->t.next = 0;

    /* if file logging is enabled, open log file */
    if(args->log_file_name!=NULL){
        s->t.log = fopen(args->log_file_name, "w");
        if(s->t.log==NULL){
            printf("Error opening log file '%s', file logging disabled\n",
                    args->log_file_name);
        }
    }
    else{
        s->t.log = NULL;
    }

    /* Setup log trigger */
    s->t.log_triggered = 0;
    s->t.log_trigger_address = args->log_trigger_address;
#endif
}

/** Dumps last jump targets as a chunk of hex numbers (older is left top) */
void dump_trace_buffer(t_state *s){
    int i, col;

    for(i=0, col=0;i<TRACE_BUFFER_SIZE;i++, col++){
        printf("%08x ", s->t.buf[s->t.next + i]);
        if((col % 8)==7){
            printf("\n");
        }
    }
}

/** Logs last cycle's activity (changes in state and/or loads/stores) */
void log_cycle(t_state *s){
    static unsigned int last_pc = 0;
    int i;
    uint32_t log_pc;

    /* store PC in trace buffer only if there was a jump */
    if(s->pc != (last_pc+4)){
        s->t.buf[s->t.next] = s->pc;
        s->t.next = (s->t.next + 1) % TRACE_BUFFER_SIZE;
    }
    last_pc = s->pc;

    /* if file logging is enabled, dump a trace log to file */
    if(log_enabled(s)){
        log_pc = s->op_addr;

        for(i=0;i<32;i++){
            if(s->t.pr[i] != s->r[i]){
                fprintf(s->t.log, "(%08X) [%02X]=%08X\n", log_pc, i, s->r[i]);
            }
            s->t.pr[i] = s->r[i];
        }
        if(s->lo != s->t.lo){
            fprintf(s->t.log, "(%08X) [LO]=%08X\n", log_pc, s->lo);
        }
        s->t.lo = s->lo;

        if(s->hi != s->t.hi){
            fprintf(s->t.log, "(%08X) [HI]=%08X\n", log_pc, s->hi);
        }
        s->t.hi = s->hi;

        /* */
        /* FIXME epc may change by direct write too, handle case */
        if(s->epc != s->t.epc){
            fprintf(s->t.log, "(%08X) [EP]=%08X\n", log_pc, s->epc);
        }
        s->t.epc = s->epc;
    }
}

/** Frees debug buffers and closes log file */
void close_trace_buffer(t_state *s){
    if(s->t.log){
        fclose(s->t.log);
    }
}

/** Logs a message for each failed assertion, each in a line */
void log_failed_assertions(t_state *s){
    unsigned bitmap = s->failed_assertions;
    int i = 0;

    /* This loop will crash the program if the message table is too short...*/
    if(s->t.log != NULL){
        for(i=0;i<32;i++){
            if(bitmap & 0x1){
                fprintf(s->t.log, "ASSERTION FAILED: [%08x] %s\n",
                        s->faulty_address,
                        assertion_messages[i]);
            }
            bitmap = bitmap >> 1;
        }
    }
}

uint32_t log_enabled(t_state *s){
    return ((s->t.log != NULL) && (s->t.log_triggered!=0));
}

void trigger_log(t_state *s){
    uint32_t i;

    s->t.log_triggered = 1;

    for(i=0;i<32;i++){
        s->t.pr[i] = s->r[i];
    }

    s->t.lo = s->lo;
    s->t.hi = s->hi;
    s->t.epc = s->epc;
}

void free_cpu(t_state *s){
    int i;

    for(i=0;i<NUM_MEM_BLOCKS;i++){
        free(s->blocks[i].mem);
        s->blocks[i].mem = NULL;
    }
}

void reset_cpu(t_state *s){
    s->pc = VECTOR_RESET;     /* reset start vector */
    s->delay_slot = 0;
    s->failed_assertions = 0; /* no failed assertions pending */
}

int init_cpu(t_state *s){
    int i, j;

    memset(s, 0, sizeof(t_state));
    s->big_endian = 1;

    /* Initialize memory map */
    for(i=0;i<NUM_MEM_BLOCKS;i++){
        s->blocks[i].start =  memory_map_default[i].start;
        s->blocks[i].size =  memory_map_default[i].size;
        s->blocks[i].area_name =  memory_map_default[i].area_name;
        s->blocks[i].mask =  memory_map_default[i].mask;
        s->blocks[i].read_only =  memory_map_default[i].read_only;

        s->blocks[i].mem = (unsigned char*)malloc(s->blocks[i].size);

        if(s->blocks[i].mem == NULL){
            for(j=0;j<i;j++){
                free(s->blocks[j].mem);
            }
            return 0;
        }
        memset(s->blocks[i].mem, 0, s->blocks[i].size);
    }
    return NUM_MEM_BLOCKS;
}

int32_t parse_cmd_line(uint32_t argc, char **argv, t_args *args){
    uint32_t i;

    /* fill cmd line args with default values */
    for(i=0;i<NUM_MEM_BLOCKS;i++){
        args->bin_filename[i] = NULL;
        args->log_file_name = "sw_sim_log.txt";
        args->log_trigger_address = VECTOR_RESET;
    }

    /* parse actual cmd line args */
    for(i=1;i<argc;i++){
        if(strcmp(argv[i],"--plasma")==0){
            #ifdef SIMULATE_PLASMA
                /* program compiled for plasma compatibility, no problem */
            #else
                /* program compiled for mips-1 compatibility, error*/
                printf("Error: program compiled for compatibility to MIPS-I\n");
                return 0;
            #endif
        }
        else if(strncmp(argv[i],"--bram=", strlen("--bram="))==0){
            args->bin_filename[0] = &(argv[i][strlen("--bram=")]);
        }
        else if(strncmp(argv[i],"--flash=", strlen("--flash="))==0){
            args->bin_filename[2] = &(argv[i][strlen("--flash=")]);
        }
        else if(strncmp(argv[i],"--xram=", strlen("--xram="))==0){
            args->bin_filename[1] = &(argv[i][strlen("--xram=")]);
        }
        else if(strncmp(argv[i],"--trigger=", strlen("--trigger="))==0){
            sscanf(&(argv[i][strlen("--trigger=")]), "%x", &(args->log_trigger_address));
        }
        else if((strcmp(argv[i],"--help")==0)||(strcmp(argv[i],"-h")==0)){
            usage();
            return 0;
        }
        else{
            printf("unknown argument '%s'\n\n",argv[i]);
            usage();
            return 0;
        }
    }

    return 1;
}

void usage(void){
    printf("Usage:");
    printf("    slite file.exe [arguments]\n");
    printf("Arguments:\n");
    printf("--bram=<file name>      : BRAM initialization file\n");
    printf("--xram=<file name>      : XRAM initialization file\n");
    printf("--flash=<file name>     : FLASH initialization file\n");
    printf("--trigger=<hex number>  : Log trigger address\n");
    printf("--plasma                : Simulate Plasma instead of MIPS-I\n");
    printf("--help, -h              : Show this usage text\n");
}
