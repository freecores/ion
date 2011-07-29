This code sample tests access to external (off-FPGA) memory, including both 
16-bit SRAM and 8-bit FLASH present in the DE-1 board.

Can be simulated (both Modelsim and SW simulator) and synthesized to a hardware
demo (see makefiles).

File flash.bin is meant to be loaded at the start of the flash of the DE-1 
board using the Altera/Terasic tool provided for that purpose. This program will 
eventually jump to flash (see the sources and makefile) so if you leave it 
unprogrammed you will skip the final part of the test (execution from 8-bit 
static memory).


