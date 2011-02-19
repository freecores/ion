# External memory test -- build simulation and execution VHDL modules


# Get common makefile stuff (toolchain & system config)
include ..\\common\\makefile
	
# We'll run the simulation for long enough to complete the test
SIM_LENGTH = 30000

# FPGA Block RAM parameters
BRAM_START = 0xbfc00000
CODE_BRAM_SIZE = 2048
# External RAM parameters (size in words)
XRAM_SIZE = 1024
XRAM_START = 0x00000000


LFLAGS = -Ttext $(BRAM_START) -Tdata $(XRAM_START) -eentry -I elf32-big
	
	
clean:
	-$(RM) *.o *.obj *.map *.lst *.hex *.exe *.axf *.code *.data

memtest:
	$(AS_MIPS) -defsym XRAM_BASE=$(XRAM_START) -o memtest.o memtest.s
	$(LD_MIPS) $(LFLAGS) -Map memtest.map -s -N -o memtest.axf memtest.o
	-@$(DUMP_MIPS) -I elf32-big --disassemble memtest.axf > memtest.lst
# Dump code and data to separate binaries (data binary will be empty but TB2 needs it)
	$(COPY_MIPS) -I elf32-big -j .text -j .rodata -O binary memtest.axf memtest.code
	$(COPY_MIPS) -I elf32-big -j .sbss -j .data -j .bss -O binary memtest.axf memtest.data



# Create VHDL file for simulation test bench using TB2 template
memtest_sim: memtest
	$(TO_VHDL) --code memtest.code --data memtest.data \
		--flash flash.bin --flash_size 128 \
		--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb2_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2


# Create VHDL file for hardware demo
memtest_demo: memtest
	$(TO_VHDL) --code memtest.code --data memtest.data \
	--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
	-v $(SRC_DIR)/mips_mpu1_template.vhdl \
	-o $(DEMO_DIR)/mips_mpu.vhdl -e mips_mpu
