
# Get common makefile stuff (toolchain & system config)
include ../common/makefile

# We'll run the simulation for 2000 clock cycles
SIM_LENGTH = 2000

clean:
	-$(RM) -f *.o *.obj *.map *.lst *.hex \
	*.exe *.axf *.code *.data *.bin

opcodes:
	$(AS_MIPS) -o opcodes.o opcodes.s
	$(LD_MIPS) -Ttext 0 -eentry -Map opcodes.map -s -N -o opcodes.axf opcodes.o
	-@$(DUMP_MIPS) -I elf32-big --disassemble opcodes.axf > opcodes.lst
	@# Dump only text segment, no .rodata on this program
	$(COPY_MIPS) -I elf32-big -j .text -O binary opcodes.axf opcodes.bin
	
	
# Create VHDL file for simulation test bench
opcodes_sim: opcodes
	$(TO_VHDL) $(VHDL_FLAGS) --code opcodes.bin \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb0_template.vhdl \
		-o $(TB_DIR)\\mips_tb1.vhdl -e mips_tb1

