#-------------------------------------------------------------------------------
# This makefile does not contain any targets, only definitions used by the 
# makefiles of all the code samples. 
# It is meant to be included and not used standalone.
#-------------------------------------------------------------------------------
# KNOWN PROBLEMS AND WORKAROUNDS
#
# 1.- LINK PROBLEM IF FLAG '-G0' NOT USED
#       If flag '-G0' is not used on gcc, linker fails with 'relocation 
#       truncated to fit: R_MIPS_GPREL16' error message.
#       This only happens when you use global or static veriables, initialized 
#       or not.
#       (See explaination in the project docs about $gp indexed addressing in
#       MIPS architectures and the -G0 flag).
#
#     SUSPECTED CAUSE:
#       I'm sure there is something wrong with my linker script.
#       With the default link scripts this does not happen. Yet we need to use
#       a script so that we can split code and data (including read-only) to
#       different sections (and later to different ram blocks).
#       
#     WORKAROUND: 
#       Use -G0 flag so that _gp indexing is disabled. There is a performance
#       hit, though. In effect we're telling the compiler to NOT use $gp for
#       indexed access to any global variables.
#       This is only necessary for the 'bare' target (no external ram and no
#       cache) and will have to be fixed for regular targets (by using a 
#       standard link script or fixing mine).
#-------------------------------------------------------------------------------
# SYSTEM PARAMETERS
#
# All you can define here is the size of the VHDL RAM tables.
# Some other system parameters are defined in the linker script (ion*.lds).
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

# Default size of code BRAM in 32-bit words
CODE_SIZE = 1024
# Default size of data BRAM in 32-bit words
DATA_SIZE = 256

### Build options ##############################################################

# Don't use gcc builtin functions, and try to target MIPS-I architecture
# (See comment above about -G0 flag)
CFLAGS = -O2 -Wall -c -s -fno-builtin -mips1 -G0
# Use project 'bare cpu' linker script and build elf-bigendian object file
LFLAGS = -T../ion_noxram.lds -I elf32-big -eentry
# Pass BRAM sizes to VHDL conversion script
VHDL_FLAGS = --code_size $(CODE_SIZE) --data_size $(DATA_SIZE)

### Project directories ########################################################

# VHDL test bench directory, where VHDL output files will be created
TB_DIR = ../../vhdl/tb
# VHDL DE-1 board demo root directory, for vhdl output
DEMO_DIR = ../../vhdl/demo
# Root test code source directory, where python script and vhdl templates are
SRC_DIR = ..
