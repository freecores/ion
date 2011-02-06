
# Get common makefile stuff (toolchain & system config)
include ../common/makefile
	
# We'll run the simulation for 2000 clock cycles
SIM_LENGTH = 20000
	
clean:
	-$(RM) *.o *.obj *.map *.lst *.hex *.exe *.axf *.code *.data

hello:
	$(AS_MIPS) -o boot.o $(SRC_DIR)/common/boot.s
	$(GCC_MIPS) -o no_os.o $(SRC_DIR)/common/no_os.c
	$(GCC_MIPS) -o hello.o hello.c
	$(LD_MIPS) $(LFLAGS_BOOT) -Map hello.map -s -N -o hello.axf boot.o hello.o no_os.o 
	-@$(DUMP_MIPS) -m mips --disassemble hello.axf > hello.lst
# Extract object code to be placed in code space (see link script)
	$(COPY_MIPS) -I elf32-big -O binary hello.axf hello.code
#	$(COPY_MIPS) -I elf32-big -j.text -O binary hello.axf hello.code
# Extract object code to be placed in data space (see link script)
	$(COPY_MIPS) -I elf32-big -j.data -j.bss -O binary hello.axf hello.data
	

# Create VHDL file for simulation test bench using TB2 template
hello_sim: hello
	$(TO_VHDL) --code hello.code --data hello.data \
		--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb2_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2


# Create VHDL file for hardware demo
hello_demo: hello
	$(TO_VHDL) --code hello.code --data hello.data \
	--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
	-v $(SRC_DIR)/mips_mpu1_template.vhdl \
	-o $(DEMO_DIR)/mips_mpu.vhdl -e mips_mpu
