onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -format Logic /mips_tb2/clk
add wave -noupdate -format Logic /mips_tb2/reset
add wave -noupdate -color {Medium Blue} -format Logic /mips_tb2/cpu_code_rd_vma
add wave -noupdate -color {Cadet Blue} -format Literal -radix hexadecimal /mips_tb2/full_code_addr
add wave -noupdate -color {Cornflower Blue} -format Literal -radix hexadecimal /mips_tb2/cpu_code_rd
add wave -noupdate -color {Olive Drab} -format Logic /mips_tb2/cpu_data_rd_vma
add wave -noupdate -color {Forest Green} -format Literal -radix hexadecimal /mips_tb2/cpu_data_rd_addr
add wave -noupdate -color {Lime Green} -format Literal -radix hexadecimal /mips_tb2/cpu_data_rd
add wave -noupdate -color Sienna -format Literal -radix hexadecimal /mips_tb2/full_wr_addr
add wave -noupdate -color Khaki -format Literal -radix hexadecimal /mips_tb2/cpu_data_wr
add wave -noupdate -color Salmon -format Literal /mips_tb2/cpu_byte_we
add wave -noupdate -color Firebrick -format Logic /mips_tb2/cpu_mem_wait
add wave -noupdate -format Literal -radix hexadecimal /mips_tb2/cache/data_addr_reg
add wave -noupdate -group SDRAM
add wave -noupdate -group SDRAM -color {Slate Blue} -format Literal -radix hexadecimal /mips_tb2/sram_address
add wave -noupdate -group SDRAM -color Plum -format Literal -radix hexadecimal /mips_tb2/sram_databus
add wave -noupdate -group SDRAM -color Violet -format Literal /mips_tb2/sram_byte_we_n
add wave -noupdate -group SDRAM -color {Dark Orchid} -format Logic /mips_tb2/sram_oe_n
add wave -noupdate -divider Cache
add wave -noupdate -color Pink -format Literal /mips_tb2/cache/ps
add wave -noupdate -divider Debug
add wave -noupdate -group STALL
add wave -noupdate -group STALL -format Logic /mips_tb2/cpu/load_interlock
add wave -noupdate -group STALL -format Logic /mips_tb2/cpu/pipeline_interlocked
add wave -noupdate -group STALL -format Logic /mips_tb2/cpu/pipeline_stalled
add wave -noupdate -group STALL -format Logic /mips_tb2/cpu/stall_pipeline
add wave -noupdate -color Gold -format Literal -radix hexadecimal /mips_tb2/cpu/p1_ir_reg
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {32290000 ps} 0}
configure wave -namecolwidth 150
configure wave -valuecolwidth 51
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
update
WaveRestoreZoom {439834018 ps} {440103473 ps}
