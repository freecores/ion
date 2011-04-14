
# Get common makefile stuff (toolchain & system config)
include ../common/makefile
	
# We'll run the simulation for 2000 clock cycles
SIM_LENGTH = 90000
	
# FPGA Block RAM parameters
BRAM_START = 0xbfc00000
CODE_BRAM_SIZE = 2048
# External RAM parameters (size in words)
XRAM_SIZE = 1024
XRAM_START = 0x00000000	
	
	
LFLAGS = -Ttext $(BRAM_START) -Tdata $(XRAM_START) -eentry -I elf32-big	
	
clean:
	-$(RM) *.o *.obj *.map *.lst *.hex *.exe *.axf *.code *.data

hello: hello.c
	$(AS_MIPS) -o startup.o --defsym RUN_FROM_BRAM=1 $(SRC_DIR)/common/startup.s
	$(GCC_MIPS) $(CFLAGS) -DRUN_FROM_BRAM -o baremetal.o $(SRC_DIR)/common/baremetal.c
	$(GCC_MIPS) -o hello.o hello.c
	$(LD_MIPS) $(LFLAGS) -Map hello.map -s -N -o hello.axf startup.o hello.o baremetal.o 
	-@$(DUMP_MIPS) -m mips --disassemble hello.axf > hello.lst
# Extract object code to be placed in code space
	$(COPY_MIPS) -I elf32-big -O binary hello.axf hello.code
# Extract object code to be placed in data space
	$(COPY_MIPS) -I elf32-big -j.data -j.bss -O binary hello.axf hello.data
	


# Create VHDL file for simulation test bench using TB2 template
hello_sim: hello
	$(TO_VHDL) --code hello.code --data hello.data \
		--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb2_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2

# Create VHDL file for simulation test bench using TB0 template
hello_sim_bram: hello
	$(TO_VHDL) --code hello.code --data hello.data \
		--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb0_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2


# Create VHDL file for hardware demo
hello_demo: hello
	$(TO_VHDL) --code hello.code --data hello.data \
	--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
	-v $(SRC_DIR)/mips_mpu1_template.vhdl \
	-o $(DEMO_DIR)/mips_mpu.vhdl -e mips_mpu
