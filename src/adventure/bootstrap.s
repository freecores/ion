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

    #---- Cache parameters
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

    jal     invalidate_i_cache
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

# void invalidate_i_cache(void) -- invalidates all I-Cache lines (uses no RAM)
invalidate_i_cache:
    li      $a0,0x00010000      # Disable cache, enable I-cache line invalidation
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
    
    li      $a1,0x00020000      # Leave with cache enabled
    jr      $ra
    mtc0    $a1,$12

#---- Constant data (note we keep it in the text section) ----------------------

put_hex_table:
    .ascii  "0123456789abcdef"

msg0:
    .ascii  "\n\r"
    .asciz  "Bootstrapping main program at 0x"
crlf:
    .asciz "\n\r"
space:
    .asciz "  "

    .set    reorder
    .end    entry

