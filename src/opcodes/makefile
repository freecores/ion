
# Get common makefile stuff (toolchain & system config)
include ../common/makefile

# We'll run the simulation for long enough for the program to finish
SIM_LENGTH = 10000
	
# FPGA Block RAM parameters
BRAM_START = 0xbfc00000
CODE_BRAM_SIZE = 2048
# External RAM parameters (size in words)
XRAM_SIZE = 1024
XRAM_START = 0x00000000	

LFLAGS = -Ttext $(BRAM_START) -Tdata $(XRAM_START) -eentry -I elf32-big

clean:
	-$(RM) -f *.o *.obj *.map *.lst *.hex \
	*.exe *.axf *.code *.data *.bin

opcodes:
	$(AS_MIPS) -defsym XRAM_BASE=$(XRAM_START) -o opcodes.o opcodes.s
	$(LD_MIPS) $(LFLAGS) -Map opcodes.map -s -N -o opcodes.axf opcodes.o
	-@$(DUMP_MIPS) -I elf32-big --disassemble opcodes.axf > opcodes.lst
# Dump only text segment, no .rodata on this program
#	$(COPY_MIPS) -I elf32-big -j .text -O binary opcodes.axf opcodes.bin
# Dump data segment to file; will be empty but the TB2 template needs it
#	$(COPY_MIPS) -I elf32-big -j .data -O binary opcodes.axf opcodes.data
	$(COPY_MIPS) -I elf32-big -O binary opcodes.axf opcodes.bin
	$(COPY_MIPS) -I elf32-big -j.data -j.bss -O binary opcodes.axf opcodes.data

	
	
# Create VHDL file for simulation test bench from TB2 template
opcodes_sim: opcodes
	$(TO_VHDL) --code opcodes.bin --data opcodes.data --log_trigger=0xbfc00000 \
		--code_size $(CODE_BRAM_SIZE) --data_size $(DATA_BRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb2_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2
