#-------------------------------------------------------------------------------
# startup.s -- C startup code common to all baremetal C programs
#
# This code does the following:
# 1.- Initialize the stack ate the end of the bss area
# 2.- Clear the bss area
# 3.- Move the data section from FLASH to RAM (if applicable)
# 4.- Call main()
# 5.- Freeze in endless loop after main() returns, if it does
#
# Besides, the code includes a basic trap routine at the trap vector offset. 
# This will only be useful if the text segment is placed at 0xbfc00000:
# If the text segment is placed at the reset vector address, this code will 
# execute at reset and interrupt_service_routine will be at the trap vector
# address. Otherwise, the reset code should jump to this program and the 
# trap routine provided will be unused.
#
# This code was inherited from Plasma and still has traces of old code which 
# will be refactored eventually.
#-------------------------------------------------------------------------------

    #---- Cache parameters
    .set ICACHE_NUM_LINES, 256              # no. of lines in the I-Cache
    .set DCACHE_NUM_LINES, 256              # no. of lines in the D-Cache
    .set DCACHE_LINE_SIZE, 4                # D-Cache line size in words

    # The stack size can be defined from the assembler command line
    .ifndef STACK_SIZE
    .set    STACK_SIZE,         1024        # by default, reserve 1KB
    .endif

    # Reserve space for regular stack (BSS segment)
    .comm init_stack, STACK_SIZE

    .text
    .align 2
    .global entry
    .ent    entry
entry:
    .set noreorder

    # Initialize the cache
    jal     setup_cache
    nop
    
    # The linker script defined these symbols
    la      $gp, _gp                # initialize global pointer
    la      $5, __bss_start         # $5 = .sbss_start
    la      $4, _end                # $2 = .bss_end
    la      $sp, init_stack+STACK_SIZE-24 #initialize stack pointer
    
    # Clear BSS area
$BSS_CLEAR:
    sw      $0, 0($5)
    slt     $3, $5, $4
    bnez    $3, $BSS_CLEAR
    addiu   $5, $5, 4

    # Move data section image from flash to RAM, if necessary
    # (assume if program does not run from BRAM, it runs from FLASH)
    .ifndef  RUN_FROM_BRAM
    jal     copy_data_sections
    nop
    .endif
    
    jal     main                    # init done; call main()
    nop
$L1:
    j       $L1
    nop

   .end     entry


#-- FIXME inherited from Plasma, needs to be refactored
   .org     0x0180
   .global  interrupt_service_routine
   .ent     interrupt_service_routine
interrupt_service_routine:
   .set     noreorder
   .set     noat


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
    rfe

   .end     interrupt_service_routine
   .set     at


# void setup_cache(void) -- invalidates all I- and D-Cache lines (uses no RAM)
setup_cache:
    mfc0    $a0,$12
    lui     $a1,0x01            # Enable I-cache line invalidation
    andi    $a0,$a0,0xffff
    or      $a0,$a0,$a1
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
    
    mfc0    $a0,$12
    lui     $a1,0x0002          # Leave with cache enabled and in kernel mode
    andi    $a0,$a0,0xffff
    or      $a0,$a0,$a1
    jr      $ra
    mtc0    $a0,$12   
   
   
#-- FIXME inherited from Plasma, needs to be refactored
   .global  setjmp
   .ent     setjmp
setjmp:
   .set     noreorder
   sw       $16, 0($4)   #s0
   sw       $17, 4($4)   #s1
   sw       $18, 8($4)   #s2
   sw       $19, 12($4)  #s3
   sw       $20, 16($4)  #s4
   sw       $21, 20($4)  #s5
   sw       $22, 24($4)  #s6
   sw       $23, 28($4)  #s7
   sw       $30, 32($4)  #s8
   sw       $28, 36($4)  #gp
   sw       $29, 40($4)  #sp
   sw       $31, 44($4)  #lr
   jr       $31
   ori      $2,  $0, 0

   .set     reorder
   .end     setjmp


#-- FIXME inherited from Plasma, needs to be refactored
   .global  longjmp
   .ent     longjmp
longjmp:
   .set     noreorder
   lw       $16, 0($4)   #s0
   lw       $17, 4($4)   #s1
   lw       $18, 8($4)   #s2
   lw       $19, 12($4)  #s3
   lw       $20, 16($4)  #s4
   lw       $21, 20($4)  #s5
   lw       $22, 24($4)  #s6
   lw       $23, 28($4)  #s7
   lw       $30, 32($4)  #s8
   lw       $28, 36($4)  #gp
   lw       $29, 40($4)  #sp
   lw       $31, 44($4)  #lr
   nop      
   jr       $31
   ori      $2,  $5, 0

   .set     reorder
   .end     longjmp
