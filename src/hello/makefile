
# Get common makefile stuff (toolchain & system config)
include ../common/makefile
	
	
clean:
	-$(RM) *.o *.obj *.map *.lst *.hex *.exe *.axf *.code *.data

hello:
	$(AS_MIPS) -o boot.o $(SRC_DIR)/common/boot.s
	$(GCC_MIPS) -o no_os.o $(SRC_DIR)/common/no_os.c
	$(GCC_MIPS) -o hello.o hello.c
# NOTE: Startup code (boot.o) is linked in the link script; 
	$(LD_MIPS) $(LFLAGS) -Map hello.map -s -N -o hello.axf hello.o no_os.o 
	-@$(DUMP_MIPS) -m mips --disassemble hello.axf > hello.lst


# Extract object code to be placed in code space (see link script)
hello.code: hello
	$(COPY_MIPS) -I elf32-big -j.text -O binary hello.axf hello.code

# Extract object code to be placed in data space (see link script)
hello.data: hello
	$(COPY_MIPS) -I elf32-big -j.rodata -j.data -j.bss -O binary hello.axf hello.data
	
# Create VHDL file for simulation test bench
hello_sim: hello.code hello.data
	$(TO_VHDL) $(VHDL_FLAGS) --code hello.code --data hello.data \
	-v $(SRC_DIR)/mips_tb1_template.vhdl -o $(TB_DIR)/mips_tb1.vhdl -e mips_tb1

# Create VHDL file for hardware demo
hello_demo: hello.code hello.data
	$(TO_VHDL) $(VHDL_FLAGS) --code hello.code --data hello.data \
	-v $(SRC_DIR)/mips_mpu_template.vhdl -o $(DEMO_DIR)/mips_mpu.vhdl -e mips_mpu
