Code samples:
=============

This directory contains a few test applications that can be simulated and run
on real hardware (except for the opcode test which can only be simulated). See
the readme file and the makefile for each program.

The makefiles have been tested with the CodeSourcery toolchain for windows (that
can be downloaded from www.codesourcery.com). They should work with other 
toolchains and have been occasionally tested with the Buildroot toolchain
for GNU/Linux.

Most makefiles have two targets, to create a simulation test bench and a 
synthesizable demo.

Target 'sim' will build a the simulation test bench package as vhdl file 
'/vhdl/tb/sim_params_pkg.vhdl'. This is the default test bench expected by the
simulation script '/sim/mips_tb.do'. The template used to build the package is 
file '/src/sim_params_template.pkg' and the tool used to insert the data into
the template is the python script '/src/bin2hdl.py'.

Target 'demo' will build a package for the synthesizable demo as file 
'/vhdl/demo/code_rom_pkg.vhdl', from template file '/code_rom_template.vhdl',
using the same python script. 

The build process will produce a number of binary files that can be run on the 
software simulator. A DOS BATCH file has been provided for each sample that 
runs the simulator with the proper parameters (swsim.bat).

The simulation log produced by the software simulator can be compared to the log
produced by Modelsim (the only hdl simulator supported yet); they should be
identical (but see notes on the project doc).



Support code library:
=====================

Many of the code samples use support code from an ad-hoc library included with 
the project (src/common/libsoc). Before making any of the samples you should 
make the library ('make' with no target). That command will build lib file 
'src/common/libsoc/libsoc.a'.



Building VHDL code from templates:
==================================

The python script 'bin2hdl.py' is used by all the samples to insert binary data 
on vhdl templates. 
Assuming you have Python 2.5 or later in your machine, call the script with 

    python bin2hdl.py --help

to get a short description and usage instructions.
There's a more detailed description in the project main doc (Well, I hope there 
is one by the time you read this, documentation has been falling behind lately).
