################################################################################
# opcode.s -- MIPS opcode tester for Ion project
#-------------------------------------------------------------------------------
# ORIGINAL AUTHOR: Steve Rhoads (rhoadss@yahoo.com) -- 1/10/01
#
# This file is an edited version of 'opcodes.asm', which is part of the Plasma
# project (http://opencores.org/project,plasma). 
# COPYRIGHT: Software placed into the public domain by the original author.
# Software 'as is' without warranty.  Author liable for nothing.
#
#-------------------------------------------------------------------------------
# This assembly file tests all of the opcodes supported by the Ion core. 
# This test assumes that address 0x20000000 is the UART write register.
# Successful tests will print out "A" or "AB" or "ABC" or ....
# Missing letters or letters out of order indicate a failure.
#
#-------------------------------------------------------------------------------
# NOTE: This test bench relies on the simulation logs to catch errors. That is,
# unlike the original Plasma code, this one does not check the test success 
# conditions. Instead, it performs the operations to be tested and relies on you
# to compare the logs from the logic simulation and the software simulation.
# Test that work this way have been commented with this tag: "@log"
#
#-------------------------------------------------------------------------------
# NOTE: NOPs have been inserted after load instructions.
#
################################################################################

    #-- Set flags below to >0 to enable/disable test assembly ------------------

    .set TEST_UNALIGNED_LOADS, 0        # unaligned loads
    .set TEST_UNALIGNED_STORES, 0       # unaligned stores
    .set TEST_BREAK, 1                  # BREAK instruction
    # WARNING: the assembler expands div instructions, see 'as' manual
    .set TEST_DIV, 1                    # DIV* instructions
    .set TEST_MUL, 1                    # MUL* instructions
    .set USE_CACHE, 1                   # Initialize and enable cache
    
    .set ICACHE_NUM_LINES, 256              # no. of lines in the I-Cache
    .set DCACHE_NUM_LINES, 256              # no. of lines in the D-Cache
    .set DCACHE_LINE_SIZE, 4                # D-Cache line size in words

    #---------------------------------------------------------------------------

    .text
    .align  2
    .globl  entry
    .ent    entry
entry:
    .set    noreorder

    la      $gp, _gp            #initialize stack pointer
    la      $4, __bss_start     #$4 = .sbss_start
    la      $5, _end            #$5 = .bss_end
    nop                         #no stack needed
    nop

    b       StartTest
    nop


    # Trap handler address 
    #.org    0x3c
    .org    0x0180
    
    # We have three trap sources: syscall, break and unimplemented opcode
    # Plus we have to account for a faulty cause code; that's 4 causes
    # Besides, we have to look out for the branch delay flag (BD)
    # We'll just increment $4 by a fixed constant depending on the cause
    # so we will not save any registers (there's no stack anyway)
InterruptVector:
    mfc0    $k0,$13             # Get trap cause code
    andi    $k0,$k0,0x01f
    ori     $k1,$zero,0x8       # was it a syscall?
    beq     $k0,$k1,trap_syscall
    addi    $k1,$k1,0x1         # was it a break?
    beq     $k0,$k1,trap_break
    addi    $k1,$k1,0x1         # was it a bad opcode?
    bne     $k0,$k1,trap_invalid
    nop
    
    # Unimplemented instruction
trap_unimplemented:
    j       trap_return
    add     $4,$4,4

    # Break instruction
trap_break:
    j       trap_return
    add     $4,$4,5
    
    # Syscall instruction
trap_syscall:
    j       trap_return
    add     $4,$4,6

    # Invalid trap cause code, most likely hardware bug
trap_invalid:
    j       trap_return
    add     $4,$4,0xf

trap_return:
    mfc0    $k1,$14             # C0_EPC=14 (Exception PC)
    mfc0    $k0,$13             # Get bit 31 (BD) from C0 cause register
    srl     $k0,31
    andi    $k0,$k0,1
    bnez    $k0,trap_return_delay_slot
    addi    $k1,$k1,4           # skip trap instruction
    jr      $k1
    nop
trap_return_delay_slot:
    addi    $k1,$k1,4           # skip jump instruction too
    jr      $k1                 # (we just added 8 to epc)


StartTest:
    .ifgt   USE_CACHE
    jal     setup_cache
    nop
    .else
    mtc0    $0,$12              # disable interrupts, disable cache
    .endif
    lui     $20,0x2000          # serial port write address
    ori     $21,$0,'\n'         # <CR> character
    ori     $22,$0,'X'          # 'X' letter
    ori     $23,$0,'\r'
    ori     $24,$0,0x0f80       # temp memory

    sb      $23,0($20)
    sb      $21,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
  
    ######################################
    #Arithmetic Instructions
    ######################################
ArthmeticTest:
    ori     $2,$0,'A'
    sb      $2,0($20)
    ori     $2,$0,'r'
    sb      $2,0($20)
    ori     $2,$0,'i'
    sb      $2,0($20)
    ori     $2,$0,'t'
    sb      $2,0($20)
    ori     $2,$0,'h'
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #a: ADD
    ori     $2,$0,'a'
    sb      $2,0($20)
    ori     $3,$0,5
    ori     $4,$0,60
    add     $2,$3,$4
    sb      $2,0($20)    #A
    sb      $23,0($20)
    sb      $21,0($20)
 
    #b: ADDI
    ori     $2,$0,'b'
    sb      $2,0($20)
    ori     $4,$0,60
    addi    $2,$4,5
    sb      $2,0($20)    #A
    sb      $23,0($20)
    sb      $21,0($20)
 
    #c: ADDIU
    ori     $2,$0,'c'
    sb      $2,0($20)
    ori     $4,$0,50
    addiu   $5,$4,15
    sb      $5,0($20)    #A
    sb      $23,0($20)
    sb      $21,0($20)
 
    #d: ADDU
    ori     $2,$0,'d'
    sb      $2,0($20)
    ori     $3,$0,5
    ori     $4,$0,60
    add     $2,$3,$4
    sb      $2,0($20)    #A
    sb      $23,0($20)
    sb      $21,0($20)
 
    # DIV tests, skip conditionally
    .ifgt TEST_DIV
    
    #e: DIV
    ori     $2,$0,'e'
    sb      $2,0($20)
    ori     $2,$0,65*117+41
    ori     $3,$0,117
    div     $2,$3
    nop
    mflo    $4
    sb      $4,0($20)    #A
    mfhi    $4
    addi    $4,$4,66-41
    sb      $4,0($20)    #B
    li      $2,-67*19
    ori     $3,$0,19
    div     $2,$3
    nop
    mflo    $4
    sub     $4,$0,$4
    sb      $4,0($20)    #C
    ori     $2,$0,68*23
    li      $3,-23
    div     $2,$3
    nop
    mflo    $4
    sub     $4,$0,$4
    sb      $4,0($20)    #D
    li      $2,-69*13
    li      $3,-13
    div     $2,$3
    mflo    $4
    sb      $4,0($20)    #E
    sb      $23,0($20)
    sb      $21,0($20)
 
    #f: DIVU
    ori     $2,$0,'f'
    sb      $2,0($20)
    ori     $2,$0,65*13
    ori     $3,$0,13
    divu    $2,$3
    nop
    mflo    $4
    sb      $4,0($20)    #A
    sb      $23,0($20)
    sb      $21,0($20)
    .endif

    # MUL tests, skip conditionally
    .ifgt   TEST_MUL
 
    #g: MULT
    ori     $2,$0,'g'
    sb      $2,0($20)
    ori     $2,$0,5
    ori     $3,$0,13
    mult    $2,$3
    nop     
    mflo    $4
    sb      $4,0($20)    #A
    li      $2,-5
    ori     $3,$0,13
    mult    $2,$3
    mfhi    $5
    mflo    $4
    sub     $4,$0,$4
    addu    $4,$4,$5
    addi    $4,$4,2
    sb      $4,0($20)    #B
    ori     $2,$0,5
    li      $3,-13
    mult    $2,$3
    mfhi    $5
    mflo    $4
    sub     $4,$0,$4
    addu    $4,$4,$5
    addi    $4,$4,3
    sb      $4,0($20)    #C
    li      $2,-5
    li      $3,-13
    mult    $2,$3
    mfhi    $5
    mflo    $4
    addu    $4,$4,$5
    addi    $4,$4,3
    sb      $4,0($20)    #D
    lui     $4,0xfe98
    ori     $4,$4,0x62e5
    lui     $5,0x6
    ori     $5,0x8db8
    mult    $4,$5
    mfhi    $6
    addiu   $7,$6,2356+1+'E' #E
    sb      $7,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #h: MULTU
    ori     $2,$0,'h'
    sb      $2,0($20)
    ori     $2,$0,5
    ori     $3,$0,13
    multu   $2,$3
    nop     
    mflo    $4
    sb      $4,0($20)    #A
    sb      $23,0($20)
    sb      $21,0($20)
    .endif
 
    #i: SLT
    ori     $2,$0,'i'
    sb      $2,0($20)
    ori     $2,$0,10
    ori     $3,$0,12
    slt     $4,$2,$3
    addi    $5,$4,64
    sb      $5,0($20)    #A
    slt     $4,$3,$2
    addi    $5,$4,66
    sb      $5,0($20)    #B
    li      $2,0xfffffff0
    slt     $4,$2,$3
    addi    $5,$4,66
    sb      $5,0($20)    #C
    slt     $4,$3,$2
    addi    $5,$4,68
    sb      $5,0($20)    #D
    li      $3,0xffffffff
    slt     $4,$2,$3
    addi    $5,$4,68
    sb      $5,0($20)    #E
    slt     $4,$3,$2
    addi    $5,$4,70
    sb      $5,0($20)    #F
    sb      $23,0($20)
    sb      $21,0($20)
 
    #j: SLTI
    ori     $2,$0,'j'
    sb      $2,0($20)
    ori     $2,$0,10
    slti    $4,$2,12
    addi    $5,$4,64
    sb      $5,0($20)    #A
    slti    $4,$2,8
    addi    $5,$4,66
    sb      $5,0($20)    #B
    sb      $23,0($20)
    sb      $21,0($20)
 
    #k: SLTIU
    ori     $2,$0,'k'
    sb      $2,0($20)
    ori     $2,$0,10
    sltiu   $4,$2,12
    addi    $5,$4,64
    sb      $5,0($20)    #A
    sltiu   $4,$2,8
    addi    $5,$4,66
    sb      $5,0($20)    #B
    sb      $23,0($20)
    sb      $21,0($20)
 
    #l: SLTU
    ori     $2,$0,'l'
    sb      $2,0($20)
    ori     $2,$0,10
    ori     $3,$0,12
    slt     $4,$2,$3
    addi    $5,$4,64
    sb      $5,0($20)    #A
    slt     $4,$3,$2
    addi    $5,$4,66
    sb      $5,0($20)    #B
    sb      $23,0($20)
    sb      $21,0($20)
 
    #m: SUB
    ori     $2,$0,'m'
    sb      $2,0($20)
    ori     $3,$0,70
    ori     $4,$0,5
    sub     $2,$3,$4
    sb      $2,0($20)    #A
    sb      $23,0($20)
    sb      $21,0($20)
 
    #n: SUBU
    ori     $2,$0,'n'
    sb      $2,0($20)
    ori     $3,$0,70
    ori     $4,$0,5
    sub     $2,$3,$4
    sb      $2,0($20)    #A
    sb      $23,0($20)
    sb      $21,0($20)
 
    ######################################
    #Branch and Jump Instructions
    ######################################
BranchTest:
    ori     $2,$0,'B'
    sb      $2,0($20)
    ori     $2,$0,'r'
    sb      $2,0($20)
    ori     $2,$0,'a'
    sb      $2,0($20)
    ori     $2,$0,'n'
    sb      $2,0($20)
    ori     $2,$0,'c'
    sb      $2,0($20)
    ori     $2,$0,'h'
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #a: B
    ori     $2,$0,'a'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    b       $B1
    sb      $10,0($20)
    sb      $22,0($20)
$B1:
    sb      $11,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #b: BAL
    ori     $2,$0,'b'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $14,$0,'E'
    ori     $15,$0,'X'
    bal     $BAL1
    sb      $10,0($20)
    sb      $13,0($20)
    b       $BAL2
    sb      $14,0($20)
    sb      $15,0($20)
$BAL1:
    sb      $11,0($20)
    jr      $31
    sb      $12,0($20)
    sb      $22,0($20)
$BAL2:
    sb      $23,0($20)
    sb      $21,0($20)
 
    #c: BEQ
    ori     $2,$0,'c'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $2,$0,100
    ori     $3,$0,123
    ori     $4,$0,123
    beq     $2,$3,$BEQ1
    sb      $10,0($20)
    sb      $11,0($20)
    beq     $3,$4,$BEQ1
    sb      $12,0($20)
    sb      $22,0($20)
$BEQ1:
    sb      $13,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #d: BGEZ
    ori     $2,$0,'d'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    or      $15,$0,'X'
    ori     $2,$0,100
    li      $3,0xffff1234
    ori     $4,$0,123
    bgez    $3,$BGEZ1
    sb      $10,0($20)
    sb      $11,0($20)
    bgez    $2,$BGEZ1
    sb      $12,0($20)
    sb      $22,0($20)
$BGEZ1:
    bgez    $0,$BGEZ2
    nop
    sb      $15,0($20)
$BGEZ2:
    sb      $13,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #e: BGEZAL
    ori     $2,$0,'e'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $14,$0,'E'
    ori     $15,$0,'X'
    li      $3,0xffff1234
    bgezal  $3,$BGEZAL1
    nop
    sb      $10,0($20)
    bgezal  $0,$BGEZAL1
    nop
    sb      $13,0($20)
    b       $BGEZAL2
    sb      $14,0($20)
    sb      $15,0($20)
$BGEZAL1:
    sb      $11,0($20)
    jr      $31
    sb      $12,0($20)
    sb      $22,0($20)
$BGEZAL2:
    sb      $23,0($20)
    sb      $21,0($20)
 
    #f: BGTZ
    ori     $2,$0,'f'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $2,$0,100
    li      $3,0xffff1234
    bgtz    $3,$BGTZ1
    sb      $10,0($20)
    sb      $11,0($20)
    bgtz    $2,$BGTZ1
    sb      $12,0($20)
    sb      $22,0($20)
$BGTZ1:
    sb      $13,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #g: BLEZ
    ori     $2,$0,'g'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $2,$0,100
    li      $3,0xffff1234
    blez    $2,$BLEZ1
    sb      $10,0($20)
    sb      $11,0($20)
    blez    $3,$BLEZ1
    sb      $12,0($20)
    sb      $22,0($20)
$BLEZ1:
    blez    $0,$BLEZ2
    nop
    sb      $22,0($20)
$BLEZ2:
    sb      $13,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #h: BLTZ
    ori     $2,$0,'h'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $14,$0,'E'
    ori     $2,$0,100
    li      $3,0xffff1234
    ori     $4,$0,0
    bltz    $2,$BLTZ1
    sb      $10,0($20)
    sb      $11,0($20)
    bltz    $3,$BLTZ1
    sb      $12,0($20)
    sb      $22,0($20)
$BLTZ1:
    bltz    $4,$BLTZ2
    nop     
    sb      $13,0($20)
$BLTZ2:    
    sb      $14,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #i: BLTZAL
    ori     $2,$0,'i'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $14,$0,'E'
    ori     $15,$0,'X'
    li      $3,0xffff1234
    bltzal  $0,$BLTZAL1
    nop
    sb      $10,0($20)
    bltzal  $3,$BLTZAL1
    nop
    sb      $13,0($20)
    b       $BLTZAL2
    sb      $14,0($20)
    sb      $15,0($20)
$BLTZAL1:  
    sb      $11,0($20)
    jr      $31
    sb      $12,0($20)
    sb      $22,0($20)
$BLTZAL2:  
    sb      $23,0($20)
    sb      $21,0($20)
 
    #j: BNE
    ori     $2,$0,'j'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $2,$0,100
    ori     $3,$0,123
    ori     $4,$0,123
    bne     $3,$4,$BNE1
    sb      $10,0($20)
    sb      $11,0($20)
    bne     $2,$3,$BNE1
    sb      $12,0($20)
    sb      $22,0($20)
$BNE1:     
    sb      $13,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #k: J
    ori     $2,$0,'k'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $15,$0,'X'
    j       $J1
    sb      $10,0($20)
    sb      $15,0($20)
$J1:       
    sb      $11,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #l: JAL
    ori     $2,$0,'l'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $14,$0,'E'
    ori     $15,$0,'X'
    jal     $JAL1
    sb      $10,0($20)
    sb      $13,0($20)
    b       $JAL2
    sb      $14,0($20)
    sb      $15,0($20)
$JAL1:     
    sb      $11,0($20)
    jr      $31
    sb      $12,0($20)
    sb      $22,0($20)
$JAL2:     
    sb      $23,0($20)
    sb      $21,0($20)
 
    #m: JALR
    ori     $2,$0,'m'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $12,$0,'C'
    ori     $13,$0,'D'
    ori     $14,$0,'E'
    ori     $15,$0,'X'
    la      $3,$JALR1
    jalr    $3
    sb      $10,0($20)
    sb      $13,0($20)
    b       $JALR2
    sb      $14,0($20)
    sb      $15,0($20)
$JALR1:    
    sb      $11,0($20)
    jr      $31
    sb      $12,0($20)
    sb      $22,0($20)
$JALR2:    
    sb      $23,0($20)
    sb      $21,0($20)
            
    #n: JR  
    ori     $2,$0,'n'
    sb      $2,0($20)
    ori     $10,$0,'A'
    ori     $11,$0,'B'
    ori     $15,$0,'X'
    la      $3,$JR1
    jr      $3
    sb      $10,0($20)
    sb      $15,0($20)
$JR1:      
    sb      $11,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #o: NOP
    ori     $2,$0,'o'
    sb      $2,0($20)
    ori     $2,$0,65
    nop     
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
 #   b     LoadTest
 
    
BreakTest:
    .ifgt TEST_BREAK
    #p: BREAK
    ori     $2,$0,'p'       # check if it jumps to break address and comes back
    sb      $2,0($20)
    ori     $2,$0,'z'
    ori     $4,$0,59
    break   0
    addi    $4,$4,1
    sb      $4,0($20)
    
    break   0               # check if load instruction is aborted (@log)
    lb      $2,16($2)       
    
    break   0               # check if jump instruction is aborted (@log)
    j       break_jump_test1
    add     $4,$4,5
    
break_jump_test1:
    add     $4,$4,1         # make sure the jump shows in the log (@log)

    break   0               # check if store instruction is aborted
    sb      $4,0($20)

    j       break_jump_test2# check if break works in delay slot of jump
    break   0
    nop
    j       break_continue
    nop

break_jump_test2:
    add     $4,$4,1
    
break_continue:
    sb      $23,0($20)
    sb      $21,0($20)    
    .endif
 
    #q: SYSCALL
    ori     $2,$0,'q'       # check if it jumpts to trap vector and comes back
    sb      $2,0($20)
    ori     $4,$0,61
    syscall 0
    addi    $4,$4,-1
    sb      $4,0($20)
    
    syscall 0               # check if load instruction is aborted (@log)
    lb      $2,16($2)       
    
    syscall 0               # check if jump instruction is aborted (@log)
    j       syscall_jump_test1
    add     $4,$4,5
    
syscall_jump_test1:
    add     $4,$4,1         # make sure the jump shows in the log (@log)

    # TODO traps in delay slots not supported yet
    #j       syscall_jump_test2 # check if syscall works in delay slot of jump
    #break   0
    nop
    j       syscall_continue
    nop

syscall_jump_test2:
    add     $4,$4,1
    
syscall_continue:
    sb      $23,0($20)
    sb      $21,0($20)
 
 
    ######################################
    #Load, Store, and Memory Control Instructions
    ######################################
LoadTest:
    ori     $2,$0,'L'
    sb      $2,0($20)
    ori     $2,$0,'o'
    sb      $2,0($20)
    ori     $2,$0,'a'
    sb      $2,0($20)
    ori     $2,$0,'d'
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #a: LB
    ori     $2,$0,'a'
    sb      $2,0($20)
    or      $2,$0,$24
    li      $3,0x414243fc
    sw      $3,16($2)              # 16($2)    = 0x414243fc
    lb      $4,16($2)              # $4        = 0x41    
    sb      $4,0($20)              # UART      = 0x41
    lb      $4,17($2)              # $4        = 0x42
    nop
    sb      $4,0($20)              # UART      = 0x42
    lb      $4,18($2)              # $4        = 0x43
    nop
    sb      $4,0($20)              # UART      = 0x43
    lb      $2,19($2)              # $2        = 0xffff.fffc
    nop
    sra     $3,$2,8                # $3        = 0xffff.ffff
    addi    $3,$3,0x45             # $3        = 0x44
    sb      $3,0($20)              # UART      = 0x44
    addi    $2,$2,0x49             # $4        = 0x45, overflow
    sb      $2,0($20)              # UART      = 0x45
    sb      $23,0($20)             # UART      = CR
    sb      $21,0($20)             # UART      = LF
 
    #b: LBU
    ori     $2,$0,'b'
    sb      $2,0($20)
    or      $2,$0,$24
    li      $3,0x41424344
    sw      $3,16($2)
    lb      $4,16($2)
    sb      $4,0($20)
    lb      $4,17($2)
    sb      $4,0($20)
    lb      $4,18($2)
    sb      $4,0($20)
    lb      $2,19($2)
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #c: LH
    ori     $2,$0,'c'
    sb      $2,0($20)
    or      $2,$0,$24
    li      $3,0x00410042
    sw      $3,16($2)
    lh      $4,16($2)
    sb      $4,0($20)
    lh      $2,18($2)
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #d: LHU
    ori     $2,$0,'d'
    sb      $2,0($20)
    or      $2,$0,$24
    li      $3,0x00410042
    sw      $3,16($2)
    lh      $4,16($2)
    sb      $4,0($20)
    lh      $2,18($2)
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #e: LW
    ori     $2,$0,'e'
    sb      $2,0($20)
    or      $2,$0,$24
    li      $3,'A'
    sw      $3,16($2)
    ori     $3,$0,0
    lw      $2,16($2)
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
    
    .ifgt TEST_UNALIGNED_LOADS
    #f: LWL & LWR
    ori     $2,$0,'f'
    sb      $2,0($20)
    or      $2,$0,$24
    li      $3,'A'
    sw      $3,16($2)
    ori     $3,$0,0
    lwl     $2,16($2)
    lwr     $2,16($2)
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
    .endif
    
    #g: SB
    ori     $2,$0,'g'
    sb      $2,0($20)
    ori     $2,$0,'A'
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #h: SH
    ori     $2,$0,'h'
    sb      $2,0($20)
    or      $4,$0,$24
    ori     $2,$0,0x4142
    sh      $2,16($4)
    lb      $3,16($4)
    sb      $3,0($20)
    lb      $2,17($4)
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #i: SW
    ori     $2,$0,'i'
    sb      $2,0($20)
    or      $2,$0,$24
    li      $3,0x41424344
    sw      $3,16($2)
    lb      $4,16($2)
    sb      $4,0($20)
    lb      $4,17($2)
    sb      $4,0($20)
    lb      $4,18($2)
    sb      $4,0($20)
    lb      $2,19($2)
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    .ifgt  TEST_UNALIGNED_STORES
    #j: SWL & SWR
    ori     $2,$0,'j'
    sb      $2,0($20)
    or      $2,$0,$24
    li      $3,0x41424344
    swl     $3,16($2)
    swr     $3,16($2)
    lb      $4,16($2)
    sb      $4,0($20)
    lb      $4,17($2)
    sb      $4,0($20)
    lb      $4,18($2)
    sb      $4,0($20)
    lb      $2,19($2)
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
    .endif
 
    ######################################
    #Logical Instructions
    ######################################
LogicalTest:
    ori     $2,$0,'L'
    sb      $2,0($20)
    ori     $2,$0,'o'
    sb      $2,0($20)
    ori     $2,$0,'g'
    sb      $2,0($20)
    ori     $2,$0,'i'
    sb      $2,0($20)
    ori     $2,$0,'c'
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #a: AND
    ori     $2,$0,'a'
    sb      $2,0($20)
    ori     $2,$0,0x0741
    ori     $3,$0,0x60f3
    and     $4,$2,$3
    sb      $4,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #b: ANDI
    ori     $2,$0,'b'
    sb      $2,0($20)
    ori     $2,$0,0x0741
    andi    $4,$2,0x60f3
    sb      $4,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #c: LUI
    ori     $2,$0,'c'
    sb      $2,0($20)
    lui     $2,0x41
    srl     $3,$2,16
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #d: NOR
    ori     $2,$0,'d'
    sb      $2,0($20)
    li      $2,0xf0fff08e
    li      $3,0x0f0f0f30
    nor     $4,$2,$3
    sb      $4,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #e: OR
    ori     $2,$0,'e'
    sb      $2,0($20)
    ori     $2,$0,0x40
    ori     $3,$0,0x01
    or      $4,$2,$3
    sb      $4,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #f: ORI
    ori     $2,$0,'f'
    sb      $2,0($20)
    ori     $2,$0,0x40
    ori     $4,$2,0x01
    sb      $4,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #g: XOR
    ori     $2,$0,'g'
    sb      $2,0($20)
    ori     $2,$0,0xf043
    ori     $3,$0,0xf002
    xor     $4,$2,$3
    sb      $4,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #h: XORI
    ori     $2,$0,'h'
    sb      $2,0($20)
    ori     $2,$0,0xf043
    xor     $4,$2,0xf002
    sb      $4,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
  
    ######################################
    #Move Instructions
    ######################################
MoveTest:
    ori     $2,$0,'M'
    sb      $2,0($20)
    ori     $2,$0,'o'
    sb      $2,0($20)
    ori     $2,$0,'v'
    sb      $2,0($20)
    ori     $2,$0,'e'
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    # HI and LO are only implemented if the mul/div block is
    .ifgt (TEST_DIV + TEST_MUL)
    #a: MFHI
    ori     $2,$0,'a'
    sb      $2,0($20)
    ori     $2,$0,65
    mthi    $2
    mfhi    $3
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #b: MFLO
    ori     $2,$0,'b'
    sb      $2,0($20)
    ori     $2,$0,65
    mtlo    $2
    mflo    $3
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #c: MTHI
    ori     $2,$0,'c'
    sb      $2,0($20)
    ori     $2,$0,65
    mthi    $2
    mfhi    $3
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #d: MTLO
    ori     $2,$0,'d'
    sb      $2,0($20)
    ori     $2,$0,66   # use 'B' instead of 'A' to cach change in HI and LO
    mtlo    $2
    mflo    $3
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
    .endif
 
    ######################################
    #Shift Instructions
    ######################################
ShiftTest:
    ori     $2,$0,'S'
    sb      $2,0($20)
    ori     $2,$0,'h'
    sb      $2,0($20)
    ori     $2,$0,'i'
    sb      $2,0($20)
    ori     $2,$0,'f'
    sb      $2,0($20)
    ori     $2,$0,'t'
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #a: SLL
    ori     $2,$0,'a'
    sb      $2,0($20)
    li      $2,0x40414243
    sll     $3,$2,8
    srl     $3,$3,24
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #b: SLLV
    ori     $2,$0,'b'
    sb      $2,0($20)
    li      $2,0x40414243
    ori     $3,$0,8
    sllv    $3,$2,$3
    srl     $3,$3,24
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #c: SRA
    ori     $2,$0,'c'
    sb      $2,0($20)
    li      $2,0x40414243
    sra     $3,$2,16
    sb      $3,0($20)
    li      $2,0x84000000
    sra     $3,$2,25
    sub     $3,$3,0x80
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #d: SRAV
    ori     $2,$0,'d'
    sb      $2,0($20)
    li      $2,0x40414243
    ori     $3,$0,16
    srav    $3,$2,$3
    sb      $3,0($20)
    ori     $3,$0,25
    li      $2,0x84000000
    srav    $3,$2,$3
    sub     $3,$3,0x80
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #e: SRL
    ori     $2,$0,'e'
    sb      $2,0($20)
    li      $2,0x40414243
    srl     $3,$2,16
    sb      $3,0($20)
    li      $2,0x84000000
    srl     $3,$2,25
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
    #f: SRLV
    ori     $2,$0,'f'
    sb      $2,0($20)
    li      $2,0x40414243
    ori     $3,$0,16
    srlv    $4,$2,$3
    sb      $4,0($20)
    ori     $3,$0,25
    li      $2,0x84000000
    srlv    $3,$2,$3
    sb      $3,0($20)
    sb      $23,0($20)
    sb      $21,0($20)
 
 
    ori     $2,$0,'D'
    sb      $2,0($20)
    ori     $2,$0,'o'
    sb      $2,0($20)
    ori     $2,$0,'n'
    sb      $2,0($20)
    ori     $2,$0,'e'
    sb      $2,0($20)
    sb      $23,0($20)
    sb      $21,0($20)

    # These tests are to be evaluated by matching logs with the software 
    # simulator. There is no need to perform any actual tests here, if there is 
    # any error it will show up as a mismatch in the logs.

    #-- Arithmetic --------------------------------------------------
    ori     $s0,$zero,0xa5a5
    ori     $s1,$zero,15
    ori     $s2,$zero,1
    li      $s3,-1
    li      $s4,-1000
    li      $s5,0x80000000
    li      $s6,0x7fffffff
    
    #-- ADD ---------------------------
    move    $t0,$s0                     # P + P = P
    move    $t1,$s1
    add     $t0,$t1,$s1
    move    $t0,$s0                     # P + N = P
    move    $t1,$s1
    add     $t0,$t1,$s3
    move    $t0,$s0                     # P + N = N
    move    $t1,$s1
    add     $t0,$t1,$s4
    move    $t0,$s0                     # P + P = N (overflow)
    move    $t1,$s6
    add     $t0,$t1,$s6
    move    $t0,$s0                     # N + N = P (overflow)
    move    $t1,$s5
    add     $t0,$t1,$s5
    
    #-- ADDI --------------------------
    move    $t0,$s0                     # N + N = P (overflow)
    move    $t1,$s5
    addi    $t0,$t1,-1
    move    $t0,$s0                     # N + P = N
    move    $t1,$s5
    addi    $t0,$t1,15
    move    $t0,$s0                     # N + P = P
    move    $t1,$s3
    addi    $t0,$t1,2
    move    $t0,$s0                     # P + P = P
    move    $t1,$s1
    addi    $t0,$t1,2
    move    $t0,$s0                     # P + P = N (overflow)
    move    $t1,$s6
    addi    $t0,$t1,2

    #-- ADDIU -------------------------
    move    $t0,$s0                     # N + N = P (overflow)
    move    $t1,$s5
    addiu   $t0,$t1,-1
    move    $t0,$s0                     # N + P = N
    move    $t1,$s5
    addiu   $t0,$t1,15
    move    $t0,$s0                     # N + P = P
    move    $t1,$s3
    addiu   $t0,$t1,2
    move    $t0,$s0                     # P + P = P
    move    $t1,$s1
    addiu   $t0,$t1,2
    move    $t0,$s0                     # P + P = N (overflow)
    move    $t1,$s6
    addiu   $t0,$t1,2

    #-- ADDU --------------------------
    move    $t0,$s0                     # P + P = P
    move    $t1,$s1
    addu    $t0,$t1,$s1
    move    $t0,$s0                     # P + N = P
    move    $t1,$s1
    addu    $t0,$t1,$s3
    move    $t0,$s0                     # P + N = N
    move    $t1,$s1
    addu    $t0,$t1,$s4
    move    $t0,$s0                     # P + P = N (overflow)
    move    $t1,$s6
    addu    $t0,$t1,$s6
    move    $t0,$s0                     # N + N = P (overflow)
    move    $t1,$s5
    addu    $t0,$t1,$s5    

    #-- SUB ---------------------------
    move    $t0,$s0                     # P - P = P
    move    $t1,$s2
    sub     $t0,$t1,$s1
    move    $t0,$s0                     # P - N = P
    move    $t1,$s1
    add     $t0,$t1,$s3
    move    $t0,$s0                     # P + N = N
    move    $t1,$s1
    add     $t0,$t1,$s4
    move    $t0,$s0                     # P + P = N (overflow)
    move    $t1,$s6
    add     $t0,$t1,$s6
    move    $t0,$s0                     # N + N = P (overflow)
    move    $t1,$s5
    add     $t0,$t1,$s5

    # SLT, SLTI, SLTIU
    ori     $a2,$zero,0xa5a5
    ori     $a0,$zero,15
    ori     $a1,$zero,1
    move    $v0,$a3
    slt     $v0,$a0,$a1
    move    $v0,$a3
    slt     $v0,$a1,$a0
    move    $v0,$a3
    slti    $v0,$a0,1
    move    $v0,$a3
    slti    $v0,$a1,15
    move    $v0,$a3
    sltiu   $v0,$a0,1
    move    $v0,$a3
    sltiu   $v0,$a1,15
    li      $a1,0xa5a5
    li      $a0,1029
    addiu   $a0,$a0,-2000
    sltu    $a1,$a0,1000


    #-- Relative jumps ----------------------------------------------
    li      $s0,0x7fffffff
    ori     $s1,1000
    ori     $s1,15
    ori     $s1,2
    li      $s4,0x80000000
    li      $s5,-1001
    li      $s6,-16
    li      $s7,-3
    
    bgtz    $s1,test_b0
    nop
    ori     $v0,0x5500
test_b0:
    bgtz    $s7,test_b1
    nop
    ori     $v0,0x5501
test_b1:
    bgtz    $s0,test_b2
    nop
    ori     $v0,0x5502
test_b2:
    bgtz    $s4,test_b3
    nop
    ori     $v0,0x5503
test_b3:
    bgez    $s1,test_b4
    nop
    ori     $v0,0x5500
test_b4:
    bgez    $s7,test_b5
    nop
    ori     $v0,0x5501
test_b5:
    bgez    $s0,test_b6
    nop
    ori     $v0,0x5502
test_b6:
    bgez    $s4,test_b7
    nop
    ori     $v0,0x5503
test_b7:
    bltz    $s1,test_b8
    nop
    ori     $v0,0x5500
test_b8:
    bltz    $s7,test_b9
    nop
    ori     $v0,0x5501
test_b9:
    bltz    $s0,test_b10
    nop
    ori     $v0,0x5502
test_b10:
    bltz    $s4,test_b11
    nop
    ori     $v0,0x5503
test_b11:

$DONE:
    j       $DONE
    nop

# void setup_cache(void) -- invalidates all I- and D-Cache lines (uses no RAM)
setup_cache:
    lui     $a0,0x0001          # Enable I-cache line invalidation
    mtc0    $a0,$12
    
    # In order to invalidate a I-Cache line we have to write its tag number to 
    # any address while bits CP0[12].17:16=01. The write will be executed as a
    # regular write too, as a side effect, so we need to choose a harmless 
    # target address. The BSS will do -- it will be cleared later.
    # We'll cover all ICACHE_NUM_LINES lines no matter what the starting 
    # address is, anyway.
    
    la      $a0,__bss_start
    li      $a2,0
    li      $a1,ICACHE_NUM_LINES-1
    
inv_i_cache_loop:
    sw      $a2,0($a0)
    blt     $a2,$a1,inv_i_cache_loop
    addi    $a2,1
    
    # Now, the D-Cache is different. To invalidate a D-Cache line you just 
    # read from it (by proper selection of a dummy target address)  while bits 
    # CP0[12].17:16=01. The data read is undefined and should be discarded.

    li      $a0,0               # Use any base address that is mapped
    li      $a2,0
    li      $a1,DCACHE_NUM_LINES-1
    
inv_d_cache_loop:
    lw      $zero,0($a0)
    addi    $a0,DCACHE_LINE_SIZE*4
    blt     $a2,$a1,inv_d_cache_loop
    addi    $a2,1    
    
    lui     $a1,0x0002          # Leave with cache enabled
    jr      $ra
    mtc0    $a1,$12       
    .set    reorder
    .end    entry
 
