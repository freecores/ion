# assumed to run from /<project directory>/syn
# change the path to the libraries in the vmap commands to match your setup
# some unused modules' vcom calls have been commented out
vlib work

vcom -reportprogress 300 -work work ../vhdl/mips_pkg.vhdl
vcom -reportprogress 300 -work work ../vhdl/mips_shifter.vhdl
vcom -reportprogress 300 -work work ../vhdl/mips_alu.vhdl
vcom -reportprogress 300 -work work ../vhdl/mips_mult.vhdl
vcom -reportprogress 300 -work work ../vhdl/mips_cpu.vhdl
vcom -reportprogress 300 -work work ../vhdl/mips_cache_stub.vhdl

vcom -reportprogress 300 -work work ../vhdl/tb/txt_util.vhdl
vcom -reportprogress 300 -work work ../vhdl/tb/mips_tb_pkg.vhdl
vcom -reportprogress 300 -work work ../vhdl/tb/mips_tb2.vhdl

vsim -t ps work.mips_tb2(testbench)
do ./mips_tb2_wave.do
set PrefMain(font) {Courier 9 roman normal}