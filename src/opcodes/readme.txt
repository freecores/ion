This is a basic opcode test bench which tries all supported opcodes. See the
source comments. This code has been lifted whole from the Plasma project.

Build the program with:

make opcodes
or
make opcodes_sim

Read ../readme.txt for some warnings on the makefile configuration.


It will build a vhdl test bench at /vhdl/tb/mips_tb2.vhdl (overwriting) that you
can try on your VHDL simulator with script sim_tb2.do. The provided script and 
the VHDL code have some dependence on Modelsim, see project readme file.


The makefile will too bouild some bionaries that you can run in the software 
simulator:

    slite --bram=opcodes.bin --xram=opcodes.data


This code can't be used on real hardware (i/o is far too simple).

WARNING: the gnu assembler expands DIV* instructions, inserting code that 
handles division by zero. Bear that in mind when reading the listing file.
