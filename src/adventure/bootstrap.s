################################################################################
# bootstrap.s -- Jumps to FLASH start address
#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
# To be run from reset vector, standalone. Interrupts must be disabled.
#
################################################################################

    #---- Set to >0 to enable a few debug messages
    .set DEBUG,         0

    #----
    #.set XRAM_BASE,     0x80000000          # 1st XRAM address
    .set FLASH_BASE,    0xb0000000          # FLASH address

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

    b       start_boot
    nop

    .ifgt   1
    #--- Trap handler address: we don't expect any traps -----------------------
    .org    0x0180
interrupt_vector:
    b       interrupt_vector
    nop
    .endif

#-------------------------------------------------------------------------------

start_boot:
    mtc0    $0,$12              # disable interrupts

    la      $a0,msg0
    jal     puts
    nop
    li      $a0,FLASH_BASE
    li      $a1,8
    jal     put_hex
    nop
    la      $a0,crlf
    jal     puts
    nop

    li      $a0,FLASH_BASE
    jr      $a0
    nop
    # We won't be coming back


$DONE:
    j       $DONE               # ...and freeze here
    nop


#---- Functions ----------------------------------------------------------------

# void dump_hex(int *address, int len)
dump_hex:
    move    $t7,$a0
    move    $t8,$a1
    sw      $ra,0($sp)
    addi    $sp,$sp,-4

dump_hex_loop:
    lw      $a0,0($t7)
    jal     put_hex
    li      $a1,8

    la      $a0,space
    jal     puts
    addi    $t7,4

    addi    $t8,$t8,-1
    bnez    $t8,dump_hex_loop
    nop

    lw      $ra,4($sp)
    jr      $ra
    addi    $sp,$sp,4


#--- Special functions that do not use any RAM ---------------------------------
# WARNING: Not for general use!
# All parameters in $a0..$a4, stack unused. No attempt to comply with any ABI
# has been made.
# Since we can't use any RAM, register have been used liberally with no regard
# for intended usage -- have to share reg bank with calling function.

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
    .asciz  "Boostrapping main program at 0x"
crlf:
    .asciz "\n\r"
space:
    .asciz "  "

    .set    reorder
    .end    entry

