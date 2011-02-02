
# Get common makefile stuff (toolchain & system config)
include ../common/makefile

# We'll run the simulation for 2000 clock cycles
SIM_LENGTH = 3000

clean:
	-$(RM) -f *.o *.obj *.map *.lst *.hex \
	*.exe *.axf *.code *.data *.bin

opcodes:
	$(AS_MIPS) -o opcodes.o opcodes.s
	$(LD_MIPS) -Ttext 0 -eentry -Map opcodes.map -s -N -o opcodes.axf opcodes.o
	-@$(DUMP_MIPS) -I elf32-big --disassemble opcodes.axf > opcodes.lst
# Dump only text segment, no .rodata on this program
	$(COPY_MIPS) -I elf32-big -j .text -O binary opcodes.axf opcodes.bin
# Dump data segment to file; will be empty but the TB2 template needs it
	$(COPY_MIPS) -I elf32-big -j .data -O binary opcodes.axf opcodes.data
	
	
# Create VHDL file for simulation test bench from TB1 template
opcodes_sim: opcodes
	$(TO_VHDL) --code opcodes.bin \
		--code_size $(CODE_BRAM_SIZE) --data_size $(DATA_BRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb0_template.vhdl \
		-o $(TB_DIR)\\mips_tb1.vhdl -e mips_tb1

# Create VHDL file for simulation test bench from TB2 template
opcodes_sim2: opcodes
	$(TO_VHDL) --code opcodes.bin --data opcodes.data\
		--code_size $(CODE_BRAM_SIZE) --data_size $(DATA_BRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb2_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2
