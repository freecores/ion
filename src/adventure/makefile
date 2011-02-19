# External memory test -- build simulation and execution VHDL modules


# Get common makefile stuff (toolchain & system config)
include ..\\common\\makefile
	
# We'll run the simulation for long enough to complete the test
SIM_LENGTH = 20000000

# FPGA Block RAM parameters
BRAM_START = 0xbfc00000
CODE_BRAM_SIZE = 2048
# External RAM parameters (size in words)
XRAM_SIZE = 200000
XRAM_START = 0x00000000
#
FLASH_START = 0xb0000000
FLASH_SIM_SIZE = 65536


LFLAGS_BOOTSTRAP = -Ttext $(BRAM_START) -Tdata $(XRAM_START) -eentry -I elf32-big
#LFLAGS_FLASH = -Ttext $(FLASH_START) -Tdata $(XRAM_START) -eentry -I elf32-big
LFLAGS_FLASH = -Tflash.lds -eentry -I elf32-big
	
	
clean:
	-$(RM) *.o *.obj *.map *.lst *.hex *.exe *.axf *.code *.data *.bin


adventure: actions1.c actions2.c main.c init.c misc.c score.c adv_baremetal.c
	$(AS_MIPS) -o startup.o $(SRC_DIR)/common/startup.s
	$(GCC_MIPS) $(CFLAGS) -o baremetal.o $(SRC_DIR)/common/baremetal.c
	$(GCC_MIPS) $(CFLAGS) -o actions1.o actions1.c
	$(GCC_MIPS) $(CFLAGS) -o main.o main.c
	$(GCC_MIPS) $(CFLAGS) -o actions2.o actions2.c
	$(GCC_MIPS) $(CFLAGS) -o init.o init.c
	$(GCC_MIPS) $(CFLAGS) -c -g -Wa,-a,-ad -o init.o init.c > init.lst
	$(GCC_MIPS) $(CFLAGS) -o misc.o misc.c
	$(GCC_MIPS) $(CFLAGS) -o score.o score.c
	$(GCC_MIPS) $(CFLAGS) -o adv_baremetal.o adv_baremetal.c
	$(LD_MIPS) $(LFLAGS_FLASH) -Map adventure.map -s -N -o adventure.axf startup.o \
	    main.o actions1.o actions2.o init.o misc.o score.o baremetal.o adv_baremetal.o
	-@$(DUMP_MIPS) -m mips --disassemble adventure.axf > adventure.lst
# Extract object code to be placed in code space
	$(COPY_MIPS) -I elf32-big -O binary adventure.axf adventure.bin



bootstrap: bootstrap.s
	$(AS_MIPS) -defsym XRAM_BASE=$(XRAM_START) -o bootstrap.o bootstrap.s
	$(LD_MIPS) $(LFLAGS_BOOTSTRAP) -Map bootstrap.map -s -N -o bootstrap.axf bootstrap.o
	-@$(DUMP_MIPS) -I elf32-big --disassemble bootstrap.axf > bootstrap.lst
	$(COPY_MIPS) -I elf32-big -j .text -j .rodata -O binary bootstrap.axf bootstrap.code
	$(COPY_MIPS) -I elf32-big -j .sbss -j .data -j .bss -O binary bootstrap.axf bootstrap.data



# Create VHDL file for simulation test bench using TB2 template
adventure_sim: bootstrap adventure
	$(TO_VHDL) --code bootstrap.code --log_trigger=b00097d8 \
		--flash adventure.bin --flash_size $(FLASH_SIM_SIZE) \
		--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb2_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2


# Create VHDL file for hardware demo
adventure_demo: bootstrap adventure
	$(TO_VHDL) --code bootstrap.code \
	--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
	-v $(SRC_DIR)/mips_mpu1_template.vhdl \
	-o $(DEMO_DIR)/mips_mpu.vhdl -e mips_mpu
