# External memory test -- build simulation and execution VHDL modules


# Get common makefile stuff (toolchain & system config)
include ..\\common\\makefile
	
# We'll run the simulation for long enough to complete the test
SIM_LENGTH = 400000

# FPGA Block RAM parameters
BRAM_START = 0xbfc00000
CODE_BRAM_SIZE = 2048
FLASH_START = 0xb0000000
# External RAM parameters (size in words)
XRAM_SIZE = 1024
XRAM_START = 0x00000000


LFLAGS = -Ttext $(BRAM_START) -Tdata $(XRAM_START) -eentry -I elf32-big
LFLAGS_FLASH = -Ttext $(FLASH_START) -eflash_test -I elf32-big
	
	
clean:
	-$(RM) *.o *.obj *.map *.lst *.hex *.exe *.axf *.code *.data

# Use this target when you want to test execution from FLASH
memtest_main_noflash:
	$(AS_MIPS) -defsym TEST_CACHE=1 -defsym XRAM_BASE=$(XRAM_START) -o memtest.o memtest.s
	
# Use this target when you DON'T want to test execution from FLASH
memtest_main:
	$(AS_MIPS) -defsym TEST_CACHE=1 -defsym EXEC_FLASH=1 -defsym XRAM_BASE=$(XRAM_START) -o memtest.o memtest.s

memtest: memtest_main
	$(LD_MIPS) $(LFLAGS) -Map memtest.map -s -N -o memtest.axf memtest.o
	-@$(DUMP_MIPS) -I elf32-big --disassemble memtest.axf > memtest.lst
# Dump code and data to separate binaries (data binary will be empty but TB2 needs it)
	$(COPY_MIPS) -I elf32-big -j .text -j .rodata -O binary memtest.axf memtest.code
	$(COPY_MIPS) -I elf32-big -j .sbss -j .data -j .bss -O binary memtest.axf memtest.data

flashtest: memtest_main
	$(AS_MIPS) -defsym TEST_CACHE=1 -defsym FLASH_BASE=$(FLASH_START) -o flash.o flash.s
	$(LD_MIPS) $(LFLAGS_FLASH) -Map flash.map -s -N -o flash.axf flash.o
	-@$(DUMP_MIPS) -I elf32-big --disassemble flash.axf > flash.lst
	$(COPY_MIPS) -I elf32-big -j .text -j .rodata -O binary flash.axf flash.bin


# Create VHDL file for simulation test bench using TB2 template
memtest_sim: memtest flashtest
	$(TO_VHDL) --code memtest.code --data memtest.data --log_trigger=0xbfc00000 \
		--flash flash.bin --flash_size 1024 \
		--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb2_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2


# Create VHDL file for hardware demo
memtest_demo: memtest flashtest
	$(TO_VHDL) --code memtest.code --data memtest.data --log_trigger=0xbfc00000 \
	--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
	-v $(SRC_DIR)/mips_mpu1_template.vhdl \
	-o $(DEMO_DIR)/mips_mpu.vhdl -e mips_mpu