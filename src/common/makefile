#-------------------------------------------------------------------------------
# This makefile does not contain any targets, only definitions used by the 
# makefiles of all the code samples. 
# It is meant to be included and not used standalone.
#-------------------------------------------------------------------------------
# SYSTEM PARAMETERS
#
# You can define here the default size and address of the memory blocks of the 
# system as seen by the linker, though you can't change the number and type of 
# blocks here (see /vhdl/mips_cache_*.vhdl and /vhdl/mips_pkg.vhdl).
#
# The samples' makefiles will probably redefine the values, anyway.
#
#-------------------------------------------------------------------------------
# NOTE:
# A link script '/src/ion_noxram.lds' was used in previous versions of the
# project. It is no longer used. It is known to be buggy and will be removed.
#-------------------------------------------------------------------------------


### Toolchain config ###########################################################

ifeq ($(LANG),)
#****  Customize for Windows/Cygwin

# Some common file commands (Cygwin/sh version, use your own)
CP = cp
RM = RM
DWIN32 = -DWIN32
LINUX_PWD =


# MIPS GCC cross-toolchain: CodeSourcery -- replace with your own

BIN_MIPS = C:/desarrollo/SourceryGpp/bin
GCC_MIPS  = $(BIN_MIPS)/mips-sde-elf-gcc.exe $(CFLAGS)
AS_MIPS   = $(BIN_MIPS)/mips-sde-elf-as
LD_MIPS   = $(BIN_MIPS)/mips-sde-elf-ld
DUMP_MIPS = $(BIN_MIPS)/mips-sde-elf-objdump
COPY_MIPS = $(BIN_MIPS)/mips-sde-elf-objcopy
TO_VHDL   = python ../bin2hdl.py

else
#**** Customize for Linux

# MIPS GCC cross-toolchain: BuildRoot toolchain in my home directory -- replace with your own
# NOTE: we will not use gcc builtin functions or libc

BIN_MIPS = /home/jaruiz/desarrollo/uClinux/MIPS/buildroot/buildroot-2010.11/output/staging/usr/bin
GCC_MIPS = $(BIN_MIPS)/mips-unknown-linux-uclibc-gcc $(CFLAGS)
AS_MIPS = $(BIN_MIPS)/mips-unknown-linux-uclibc-as
LD_MIPS = $(BIN_MIPS)/mips-unknown-linux-uclibc-ld
DUMP_MIPS = $(BIN_MIPS)/mips-unknown-linux-uclibc-objdump
COPY_MIPS = $(BIN_MIPS)/mips-unknown-linux-uclibc-objcopy
TO_VHDL = python ../bin2hdl.py

endif

### System parameters ##########################################################

# FIXME clean up parameter names
# 'CODE_BRAM' is meant to be a small BRAM (2 to 4KB) used for bootstrapping.
# 'DATA_BRAM' is a small BRAM connected to the data ports, used for debugging.
# 'XRAM' is meant to be the main external RAM, either SRAM or SDRAM.


# Default location of code BRAM is on the reset vector address
CODE_BRAM_ADDRESS = 0
# Default size of code BRAM in 32-bit words
CODE_BRAM_SIZE = 1024
# Default size of data BRAM in 32-bit words
DATA_BRAM_SIZE = 256
# Default address of BRAM -- used in some simulation-only tests, see makefiles
DATA_BRAM_ADDRESS = 0x10000
# Default size of data external RAM (XRAM) in 32-bit words (for simulation)
XRAM_SIZE = 2048
# Default address of XRAM
XRAM_ADDRESS = 0x80000000


### Build options ##############################################################

# Don't use gcc builtin functions, and try to target MIPS-I architecture
# This will prevent usage of unimplemented opcodes but will insert nops after
# load instructions, which Ion does not need.
# (See comment above about -G0 flag)
CFLAGS = -O2 -Wall -c -s -fno-builtin -mips1 -G0

# LFLAGS: linker options are in the respective makefiles

### Project directories ########################################################

# VHDL test bench directory, where VHDL output files will be created
TB_DIR = ../../vhdl/tb
# VHDL DE-1 board demo root directory, for vhdl output
DEMO_DIR = ../../vhdl/demo
# Root test code source directory, where python script and vhdl templates are
SRC_DIR = ..
