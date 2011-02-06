################################################################################
# memtest.s -- Test external RAM memory (XRAM)
#-------------------------------------------------------------------------------
# This program tests the external RAM (connected to the core through the cache
# module). Currently it only finds the RAM top address if given the bottom 
# address. Subsequent versions will add some minimal diagnostic capability,
# which will be needed when DRAM access is implemented.
#
# This program does only support a single continuous chunk of RAM. If the cache
# module ever supports more than one chunk (e.g. DRAM and SRAM as in the DE-1 
# board) this program will be modified accordingly.
# 
# The program assumes there's no useable r/w memory other than the XRAM so it
# does not use any memory for variables or stack.
#
#------------------------------------------------------------------------------- 
# To be run from reset vector, standalone. Interrupts must be disabled.
#
################################################################################

    #---- Set to >0 to enable a few debug messages
    .set DEBUG,         0

    #---- 
    .set XRAM_BASE,     0x80000000          # 1st XRAM address
    .set XRAM_MAX,      1024                # max. no. of KB to test for

    .set UART_BASE,     0x20000000          # UART base address
    .set UART_TX,       0x0000              # TX reg offset
    .set UART_STATUS,   0x0020              # status reg offset

    #---------------------------------------------------------------------------

    .text
    .align  2
    .globl  entry
    .ent    entry
entry:
    .set    noreorder

    b       start_test
    nop

    #--- Trap handler address: we don't expect any traps -----------------------
    .org    0x3c
interrupt_vector:
    b       interrupt_vector
    nop

#-------------------------------------------------------------------------------

start_test:        
    mtc0    $0,$12              # disable interrupts
        
    la      $a0,msg0
    jal     puts
    nop
    li      $a0,XRAM_BASE
    li      $a1,8
    jal     put_hex
    nop
    la      $a0,crlf
    jal     puts
    nop
    
    la      $t0,XRAM_BASE       # address of memory word being tested
    li      $t2,XRAM_MAX        # max amount of KBs to test for
    li      $t3,1               # (used to decrement $t2)
    li      $t4,0               # no. of KBs found
    move    $t5,$t0             # keep the start addr at hand for comparison
    
    sw      $zero,0($t0)        # clear 1st test word (in case of prev. run)
    
test_loop:
    lw      $t1,0($t0)          # read word contents
    beq     $t5,$t1,hit_mirror  # if it's the start address, we hit a mirror
    nop                         # we rolled off the end of the RAM back here
    sw      $t0,0($t0)          # word = word address
    lw      $t1,0($t0)          # read word back...
    bne     $t1,$t0,bad_word    # ...and if no match, we run off the RAM
    nop                         # 
    sub     $t2,$t2,$t3         # decrement loop counter...
    bnez    $t2,test_loop       # ...and go back if there's more to go
    addiu   $t0,0x400           # in any case, increment test address by 1KB
    
    b       end_test            # end of memory found, result is in $t4
    nop
 
hit_mirror:                     # memory mirror detected
    .ifgt   DEBUG
    la      $a0,msg_mirror
    jal     puts
    nop
    .endif
    b       end_test
    nop
    
bad_word:                       # readback error detected (maybe r/o area?)
    .ifgt   DEBUG
    la      $a0,msg_bad
    jal     puts
    nop
    .endif
    b       end_test
    nop

end_test:                       # test done, ramtop+1 in $t0, #KB in $t4
    la      $a0,msg1            # Print ramtop message...
    jal     puts
    nop
    move    $a0,$t0
    li      $a1,8
    jal     put_hex
    nop
    la      $a0,crlf
    jal     puts
    nop
    
$DONE:
    j       $DONE               # ...and freeze here
    nop


#---- Functions ----------------------------------------------------------------
# WARNING: Not for general use!
# All parameters in $a0..$a4, stack unused. No attempt to comply with any ABI
# has been made.
# Since we can't use any RAM, register have been used liberally with no regard
# for intended usage -- have to share reg bank with calling function.
   
# void puts(char *s) -- print zero-terminated string
puts: 
    la      $s0,UART_BASE       # UART base address
puts_loop:
    lb      $v0,0($a0)
    beqz    $v0,puts_end
    addiu   $a0,1
puts_wait_tx_rdy:    
    lw      $v1,UART_STATUS($s0)
    andi    $v1,$v1,0x02
    beqz    $v1,puts_wait_tx_rdy
    nop
    sw      $v0,UART_TX($s0)
    b       puts_loop
    nop
    
puts_end:
    jr      $ra
    nop    

# void put_hex(int n, int d) -- print integer as d-digit hex
put_hex:
    la      $s0,UART_BASE
    la      $s1,put_hex_table
    addi    $a1,-1
    add     $a1,$a1,$a1
    add     $a1,$a1,$a1

put_hex_loop:
    srlv    $v0,$a0,$a1
    andi    $v0,$v0,0x0f
    addu    $s2,$s1,$v0
    lb      $v0,0($s2)
put_hex_wait_tx_rdy:
    lw      $v1,UART_STATUS($s0)
    andi    $v1,$v1,0x02
    beqz    $v1,put_hex_wait_tx_rdy
    nop
    sw      $v0,UART_TX($s0)
    
    bnez    $a1,put_hex_loop
    addi    $a1,-4

    jr      $ra
    nop
    

#---- Constant data (note we keep it in the text section) ----------------------

put_hex_table:
    .ascii  "0123456789abcdef"

msg0: 
    .ascii  "\n\r"
    .asciz  "Scanning external memory at 0x"
msg1:
    .asciz  "Found XRAM top at           0x"
crlf:
    .asciz "\n\r"
space:
    .asciz "  "
msg_mirror:
    .asciz "hit mirror!\n\r"
msg_bad:
    .asciz "bad readback!\n\r"
    
    .set    reorder
    .end    entry
 
