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
#-------------------------------------------------------------------------------
#
#
#-------------------------------------------------------------------------------
# To be run from reset vector, standalone. Interrupts must be disabled.
#
################################################################################

    #---- Test parameters
    .ifndef XRAM_MAX
    .set XRAM_MAX,      1024                # max. no. of KB to test for
    .endif
    .ifndef XRAM_BASE
    .set XRAM_BASE,     0x00000000          # 1st XRAM address
    .endif


    #---- Set to >0 to enable a few debug messages
    .set DEBUG,         0

    #---- Cache parameters
    .set ICACHE_NUM_LINES, 256              # no. of lines in the I-Cache
    .set DCACHE_NUM_LINES, 256              # no. of lines in the D-Cache
    .set DCACHE_LINE_SIZE, 4                # D-Cache line size in words


    #---- UART stuff
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

    .ifgt   0
    #--- Trap handler address: we don't expect any traps -----------------------
    .org    0x0180
interrupt_vector:
    b       interrupt_vector    # just freeze there
    nop
    .endif

#-------------------------------------------------------------------------------

start_test:
    li      $a0,0x00000002
    mtc0    $a0,$12             # disable interrupts and cache

    .ifdef  TEST_CACHE          # if we're going to test the I-caches then
    jal     init_cache          # invalidate all the I-Cache lines now
    nop
    .endif

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

    la      $t0,XRAM_BASE+4     # address of memory word being tested
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

end_test:                       # test done, ramtop+4 in $t0, #KB in $t4


    la      $a0,msg1            # Print ramtop message...
    jal     puts
    nop

    addi    $a0,$t0,-4          # substract the +4 offset we added before
    move    $sp,$t0             # init SP at the top of RAM space
    addi    $sp,$sp,-16
    li      $a1,8
    jal     put_hex
    nop
    la      $a0,crlf
    jal     puts
    nop

    # Clear RAM (only first words, so that simulation is not eternal)
    .ifgt 1
    li      $a0,XRAM_BASE
    addi    $a1,$a0,80*4
clear_xram_loop:
    sw      $zero,0($a0)
    blt     $a0,$a1,clear_xram_loop
    addi    $a0,$a0,4
    .endif

    # Test code execution from SRAM. We copy the test_exec_sram routine to
    # the base of the SRAM and then jal to it
    li      $a1,64
    li      $a0,XRAM_BASE
    la      $a3,test_exec_sram
copy_to_sram:
    lw      $a2,0($a3)
    nop
    sw      $a2,0($a0)
    addi    $a1,$a1,-4
    addi    $a3,$a3,4
    bnez    $a1,copy_to_sram
    addi    $a0,$a0,4

    # Optionally dump first few words of XRAM and jump onto XRAM
    .ifgt 1
    la      $a0,msg5
    jal     puts
    nop
    li      $a0,XRAM_BASE
    jal     dump_hex
    ori     $a1,$zero,20
    .endif

    .ifgt   1
    li      $a0,XRAM_BASE
    nop
    jalr    $a0
    nop
    .endif

    # If all went well in the SRAM we'll have returned here
    .ifgt   0
    # FIXME now we should do some strong test on the RAM to see if it's wired
    # correctly, using the right timing, etc.

    # Ok, now we know we have some RAM and stack space we can do some further
    # testing.
    # dump the first few words of FLASH

    la      $a0,msg2
    jal     puts
    nop

    # FIXME flash base address is hardcoded
    li      $a0,0xb0000000
    jal     put_hex
    ori     $a1,$zero,8

    la      $a0,crlf
    jal     puts
    nop

    la      $a0,crlf
    jal     puts
    nop

    li      $a0,0xb0000000
    jal     dump_hex
    ori     $a1,$zero,6
    .endif


    # Optionally jump to FLASH. This is only useful in simulation.
    .ifgt 1
    .ifdef  EXEC_FLASH
    la      $a0,msg8
    jal     puts
    nop
    li      $a0,0xb0000000
    nop
    jalr    $a0
    nop
    .endif
    .endif

    la      $a0,msg4
    jal     puts
    nop


$DONE:
    j       $DONE               # ...and freeze here
    nop


    # This short routine will be copied to RAM and then executed there. This
    # will test the behavior of the I-Cache.
test_exec_sram:
    #jr      $ra
    #nop
    sw      $ra,0($sp)
    addi    $sp,$sp,-4
    la      $a0,msg3
    la      $a1,puts
    jalr    $a1
    nop
    lw      $ra,4($sp)
    jr      $ra
    addi    $sp,$sp,4
test_exec_sram_end:
    nop



#---- Functions ----------------------------------------------------------------

# void dump_hex(int *address {a0}, int len {a1})
dump_hex:
    move    $t7,$a0
    move    $t6,$a1
    sw      $ra,0($sp)
    addi    $sp,$sp,-4

dump_hex_loop_lines:
    move    $a0,$t7
    li      $a1,8
    jal     put_hex
    nop
    la      $a0,msg6
    jal     puts
    nop
    li      $t9,4

dump_hex_loop_words:
    lw      $a0,0($t7)
    jal     put_hex
    li      $a1,8

    la      $a0,space
    jal     puts
    addi    $t7,4

    addi    $t9,$t9,-1
    bnez    $t9,dump_hex_loop_words
    nop

    la      $a0,crlf
    jal     puts
    nop

    addi    $t6,$t6,-1
    bnez    $t6,dump_hex_loop_lines
    nop

    la      $a0,msg7
    jal     puts
    addi    $t7,4

    .ifgt 0
    lw      $ra,4($sp)
    move    $a0,$ra
    li      $a1,8
    jal     put_hex
    nop
qqqq:
    j       qqqq
    nop
    .endif

    nop
    lw      $ra,4($sp)
    jr      $ra
    addi    $sp,$sp,4


#---- Cache-related functions --------------------------------------------------

# void init_cache(void) -- invalidates all I-Cache lines (uses no RAM)
init_cache:
    li      $a0,0x00010000      # Disable cache, enable I-cache line invalidation
    mfc0    $a1,$12
    or      $a0,$a0,$a1
    mtc0    $a0,$12

    # In order to invalidate a I-Cache line we have to write its tag number to
    # any address while bits CP0[12].17:16=01. The write will be executed as a
    # regular write too, as a side effect, so we need to choose a harmless
    # target address.

    li      $a0,XRAM_BASE
    li      $a2,0
    li      $a1,ICACHE_NUM_LINES-1

inv_i_cache_loop:
    sw      $a2,0($a0)
    blt     $a2,$a1,inv_i_cache_loop
    addi    $a2,1

    mfc0    $a0,$12
    li      $a1,0x00020000      # Leave cache enabled
    or      $a0,$a0,$a1
    jr      $ra
    mtc0    $a0,$12



#--- Special functions that do not use any RAM ---------------------------------
# WARNING: Not for general use!
# All parameters in $a0..$a4, stack unused. No attempt to comply with any ABI
# has been made.
# Since we can't use any RAM, registers have been used with no regard for
# intended usage -- have to share reg bank with calling function.

# void puts(char *s) -- print zero-terminated string
puts:
    la      $a2,UART_BASE       # UART base address
puts_loop:
    lb      $v0,0($a0)
    beqz    $v0,puts_end
    addiu   $a0,1
puts_wait_tx_rdy:
    lw      $v1,UART_STATUS($a2)
    andi    $v1,$v1,0x02
    beqz    $v1,puts_wait_tx_rdy
    nop
    sw      $v0,UART_TX($a2)
    b       puts_loop
    nop

puts_end:
    jr      $ra
    nop

# void put_hex(int n, int d) -- print integer as d-digit hex
put_hex:
    la      $a2,UART_BASE
    la      $a3,put_hex_table
    addi    $a1,-1
    add     $a1,$a1,$a1
    add     $a1,$a1,$a1

put_hex_loop:
    srlv    $v0,$a0,$a1
    andi    $v0,$v0,0x0f
    addu    $s2,$a3,$v0
    lb      $v0,0($s2)
put_hex_wait_tx_rdy:
    lw      $v1,UART_STATUS($a2)
    andi    $v1,$v1,0x02
    beqz    $v1,put_hex_wait_tx_rdy
    nop
    sw      $v0,UART_TX($a2)

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
msg2:
    .asciz "\n\rDumping the first few words of FLASH at address 0x"
msg3:
    .ascii "\n\rTesting code execution from SRAM...  "
    .asciz "if you see this, it worked\n\r"
msg4:
    .ascii  "\n\r\n\r"
    .asciz  "End of test.\n\r\n\r"
msg5:
    .asciz  "Dump of first few words of XRAM after initialization:\n\r"
msg6:
    .asciiz ": "
msg7:
    .asciiz "<end of dump>"
msg8:
    .ascii  "\n\r"
    .asciiz "Testing execution from 8-bit static memory (FLASH)\n\r"

    .set    reorder
    .end    entry

