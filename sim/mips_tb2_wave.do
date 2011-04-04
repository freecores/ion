onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -format Logic /mips_tb2/clk
add wave -noupdate -format Logic /mips_tb2/reset
add wave -noupdate -color {Medium Blue} -format Logic /mips_tb2/cpu_code_rd_vma
add wave -noupdate -color {Slate Blue} -format Literal -radix hexadecimal /mips_tb2/cpu_code_rd_addr
add wave -noupdate -color {Cornflower Blue} -format Literal -radix hexadecimal /mips_tb2/cpu_code_rd
add wave -noupdate -color {Olive Drab} -format Logic /mips_tb2/cpu_data_rd_vma
add wave -noupdate -color {Dark Olive Green} -format Literal -radix hexadecimal /mips_tb2/cpu_data_addr
add wave -noupdate -color {Lime Green} -format Literal -radix hexadecimal /mips_tb2/cpu_data_rd
add wave -noupdate -color Khaki -format Literal -radix hexadecimal /mips_tb2/cpu_data_wr
add wave -noupdate -color Salmon -format Literal /mips_tb2/cpu_byte_we
add wave -noupdate -color Firebrick -format Logic /mips_tb2/cpu_mem_wait
add wave -noupdate -color Gold -format Literal -radix hexadecimal /mips_tb2/cpu/p1_ir_reg
add wave -noupdate -divider Debug
add wave -noupdate -format Literal -radix hexadecimal /mips_tb2/log_info
add wave -noupdate -color Gold -format Literal -radix hexadecimal /mips_tb2/log_info.debug
add wave -noupdate -format Logic -radix hexadecimal /mips_tb2/log_info.code_rd_vma
add wave -noupdate -format Logic -radix hexadecimal /mips_tb2/log_info.write_pending
add wave -noupdate -color {Light Blue} -format Literal -radix hexadecimal /mips_tb2/log_info.pending_data_wr_addr
add wave -noupdate -color {Light Blue} -format Literal -radix hexadecimal /mips_tb2/log_info.pending_data_wr_pc
add wave -noupdate -color {Light Blue} -format Literal -radix hexadecimal /mips_tb2/log_info.pending_data_wr
add wave -noupdate -color {Light Blue} -format Literal -radix binary /mips_tb2/log_info.pending_data_wr_we
add wave -noupdate -divider Cache
add wave -noupdate -format Literal -radix hexadecimal /mips_tb2/cache/code_line_table
add wave -noupdate -format Literal -radix hexadecimal /mips_tb2/cache/code_line_addr
add wave -noupdate -format Literal /mips_tb2/cache/code_cache_tag
add wave -noupdate -format Literal /mips_tb2/cache/code_tag
add wave -noupdate -color Pink -format Literal /mips_tb2/cache/ps
add wave -noupdate -format Literal /mips_tb2/cache/code_refill_ctr
add wave -noupdate -group SDRAM
add wave -noupdate -group SDRAM -color {Cornflower Blue} -format Literal -radix hexadecimal /mips_tb2/sram_data_rd
add wave -noupdate -group SDRAM -color {Cadet Blue} -format Literal -radix hexadecimal /mips_tb2/sram_data_wr
add wave -noupdate -group SDRAM -format Literal -radix hexadecimal -expand /mips_tb2/sram_chip_addr
add wave -noupdate -group SDRAM -color {Slate Blue} -format Literal -radix hexadecimal /mips_tb2/sram_address
add wave -noupdate -group SDRAM -color Violet -format Literal /mips_tb2/sram_byte_we_n
add wave -noupdate -group SDRAM -color {Dark Orchid} -format Logic /mips_tb2/sram_oe_n
add wave -noupdate -group I-Cache
add wave -noupdate -group I-Cache -color Orange -format Logic /mips_tb2/cache/code_miss
add wave -noupdate -group I-Cache -format Literal -radix hexadecimal /mips_tb2/cache/code_refill_addr
add wave -noupdate -group I-Cache -format Literal -radix hexadecimal /mips_tb2/cache/code_rd_addr_reg
add wave -noupdate -group D-Cache
add wave -noupdate -group D-Cache -color {Forest Green} -format Logic /mips_tb2/cache/read_pending
add wave -noupdate -group D-Cache -color Khaki -format Logic /mips_tb2/cache/write_pending
add wave -noupdate -group D-Cache -format Literal -radix hexadecimal /mips_tb2/cache/data_cache_store
add wave -noupdate -group STALL
add wave -noupdate -group STALL -format Logic /mips_tb2/cpu/stalled_interlock
add wave -noupdate -group STALL -format Logic /mips_tb2/cpu/stalled_memwait
add wave -noupdate -group STALL -format Logic /mips_tb2/cpu/stalled_muldiv
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {2710000 ps} 0}
configure wave -namecolwidth 150
configure wave -valuecolwidth 64
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
WaveRestoreZoom {2668953 ps} {2884729 ps}
