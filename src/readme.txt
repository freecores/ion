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

Target 'sim' will build the simulation test bench as vhdl file 
'/vhdl/tb/mips_tb2.vhdl'. This is the default test bench expected by the
simulation script '/sim/mips_tb2.do'.

Target 'demo' will build a synthesizable demo as '/vhdl/demo/mips_mpu.vhdl'. 

The build process will produce a number of binary files that can be run on the 
software simulator. A DOS BATCH file has been provided for each sample that 
runs the simulator with the proper parameters (swsim.bat).


Support code library:
=====================

Many of the code samples use support code from an ad-hoc library included with 
the project (src/common/libsoc). Before making any of the samples you should 
make the library ('make' with no target). That will build file 
'src/common/libsoc/libsoc.a'.



Building VHDL code from templates:
==================================

The python script 'bin2hdl.py' is used by all the samples to insert binary data 
on vhdl templates. 
Assuming you have Python 2.5 or later in your machine, call the script with 

    python bin2hdl.py --help

to get a short description.
There's a more detailed description in the project main doc.
