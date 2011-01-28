This directory contains a few test applications that can be simulated and run
on real hardware, except for the opcode test which can only be simulated). See
the readme file and the makefile for each program.

The makefiles have been tested with the CodeSourcery toolchain for windows (that
can be downloaded from www.codesourcery.com) and with the Buildroot toolchain
for GNU/Linux.

Most makefiles have two targets, to create a simulation test bench and a 
synthesizable demo.

Target '*_sim' (e.g. 'hello_sim') will build the simulation test bench as vhdl
file '/vhdl/tb/mips_tb1.vhdl'. This is the default test bench expected by the
simulation script '/sim/mips_tb1.do'.

Target '*_demo' will build a synthesizable demo as '/vhdl/demo/mips_mpu.vhdl'. 

The build process will produce two binary files '*.code' and '*.data' that can
be run on the software simulator:

    slite hello.code hello.data
    
Plus a listing file (*.lst) handy for debugging.


The python script 'bin2hdl.py' is used to insert binary data on vhdl templates.
Assuming you have Python 2.5 or later in your machine, call the script with 

    python bin2hdl.py --help

to get a short description.
There's a more detailed description in the project main doc.
