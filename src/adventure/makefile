#-- Bring toolchain config parameters from the common makefile
include ../common/makefile

#-- Configure the VHDL simulation environment ----------------------------------

# Simulation length in cycles; long enough to let the program run to completion.
SIM_LENGTH = 400000000


#-- Configure the memory map ---------------------------------------------------
# FIXME this info is redundant with the linker script

# FPGA Block RAM parameters
BRAM_START = 0xbfc00000
CODE_BRAM_SIZE = 2048
# External RAM parameters (size in words)
XRAM_SIZE = 200000 
XRAM_START = 0x00000000
# External FLASH parameters (size in words) 
# Note te size is for simulation only
FLASH_START = 0xb0000000 
FLASH_SIM_SIZE = 65536 

#-- Configure the code ---------------------------------------------------------

# Options for 'boot' code running from BRAM at 0xbfc00000:
# 1.- Put the code in internal BRAM and the data in external SRAM.
# 2.- Do not use any of the standard libraries and use libsoc instead.
LFLAGS_BOOT = -L $(LIB_PATH) -Ttext $(BRAM_START) -Tdata $(XRAM_START) \
         -nodefaultlibs -nostdlib -ereset -I elf32-big -defsym entry=$(FLASH_START)
CFLAGS_BOOT = -O2 -Wall -c -s -fno-builtin -nodefaultlibs -nostdlib \
         -msoft-float -mips1 -G0
AFLAGS_BOOT = --defsym RUN_FROM_BRAM=1 --defsym XRAM_BASE=$(XRAM_START)


# Options for 'main' code running from FLASH at 0xb0000000:
# 1.- Put the code in FLASH and the data in external SRAM.
# 2.- Do not use any of the standard libraries and use libsoc instead.
LFLAGS_MAIN = -T$(LINK_SCRIPT) -L $(LIB_PATH) \
         -nodefaultlibs -nostdlib -eentry -I elf32-big
CFLAGS_MAIN = -O2 -Wall -c -s -fno-builtin -nodefaultlibs -nostdlib \
         -msoft-float -mips1 -G0
AFLAGS_MAIN = --defsym XRAM_BASE=$(XRAM_START)

# Object files for main program
OBJS = c_startup.o main.o actions1.o actions2.o init.o misc.o \
        score.o adv_baremetal.o
SRCS = actions1.c actions2.c main.c init.c misc.c score.c adv_baremetal.c


#-- Targets & rules ------------------------------------------------------------

adventure: adventure.bin bootstrap
	@# This comment prevents use of implicit rule so our rules are used instead.
	@# This will save us a few harmless linker warnings.

adventure.bin: adventure.axf
	# Extract object code to be placed in code space
	$(COPY) -I elf32-big -O binary adventure.axf adventure.bin
    
adventure.axf: $(OBJS)
	$(LD) $(LFLAGS_MAIN) -Map adventure.map -s -N -o adventure.axf $(OBJS) -lsoc
	-@$(DUMP) -m mips --disassemble adventure.axf > adventure.lst

c_startup.o: $(SRC_DIR)/common/c_startup.s
	$(AS) $(AFLAGS_MAIN) -o c_startup.o $(SRC_DIR)/common/c_startup.s


actions1.o:         misc.h main.h share.h funcs.h
main.o:             misc.h adv_baremetal.h
actions2.o:         misc.h main.h share.h funcs.h
init.o:             misc.h main.h share.h funcs.h
misc.o:             misc.h main.h
score.o:            misc.h main.h share.h
adv_baremetal.o:    adv_baremetal.h


#-- Compilation of 'boot' code to be run from BRAM

bootstrap.o: $(SRC_DIR)/common/bootstrap.s
	$(AS) -defsym XRAM_BASE=$(XRAM_START) -o bootstrap.o $(SRC_DIR)/common/bootstrap.s

opcode_emu.o: $(SRC_DIR)/common/opcode_emu.s
	$(AS) $(AFLAGS) -o opcode_emu.o $(SRC_DIR)/common/opcode_emu.s

bootstrap.axf: bootstrap.o opcode_emu.o
	$(LD) $(LFLAGS_BOOT) -Map bootstrap.map -s -N -o bootstrap.axf bootstrap.o opcode_emu.o
	-@$(DUMP) -I elf32-big --disassemble bootstrap.axf > bootstrap.lst

bootstrap: bootstrap.axf
	$(COPY) -I elf32-big -j .text -j .rodata -O binary bootstrap.axf bootstrap.code
	$(COPY) -I elf32-big -j .sbss -j .data -j .bss -O binary bootstrap.axf bootstrap.data

    
#-- Let's make some implicit rules explicit for clarity (well, 'clarity')

%.o: %.c
	$(CC) $(CFLAGS_MAIN) $< -o $@


#-- Targets that build the synthesizable vhdl; meant for direct invocation -----



# Create VHDL file for simulation test bench using TB2 template
sim: bootstrap adventure
	$(TO_VHDL) --code bootstrap.code --log_trigger=b0000000 \
		--flash adventure.bin --flash_size $(FLASH_SIM_SIZE) \
		--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
		-s $(SIM_LENGTH) -v $(SRC_DIR)\\mips_tb2_template.vhdl \
		-o $(TB_DIR)\\mips_tb2.vhdl -e mips_tb2


# Create VHDL file for hardware demo
demo: bootstrap adventure
	$(TO_VHDL) --code bootstrap.code \
	--code_size $(CODE_BRAM_SIZE) --data_size $(XRAM_SIZE) \
	-v $(SRC_DIR)/code_rom_template.vhdl -n "Adventure" \
	-o $(DEMO_DIR)/code_rom_pkg.vhdl


#-- And now the usual housekeeping stuff ---------------------------------------

.PHONY: clean

clean:
	-$(RM) *.o *.obj *.map *.lst *.hex *.exe *.axf *.code *.data *.bin