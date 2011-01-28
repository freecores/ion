This is a basic opcode test bench which tries all supported opcodes. See the
source comments. This code has been lifted whole from the Plasma project.

Build the program with:

make opcodes
or
make opcodes_sim

Read ../readme.txt for some warnings on the makefile configuration.


The makefile will build a binary that you can run in the software simulator:

    slite opcodes.bin

It will build a vhdl test bench at /vhdl/tb/mips_tb1.vhdl (overwriting) that you
can try on your VHDL simulator (the provided script and the VHDL code have some
dependence on Modelsim, see project readme file).
