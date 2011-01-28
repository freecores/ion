onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -format Logic /mips_tb1/clk
add wave -noupdate -format Logic /mips_tb1/reset
add wave -noupdate -color Goldenrod -format Literal -radix hexadecimal /mips_tb1/full_code_addr
add wave -noupdate -color Gold -format Literal -radix hexadecimal /mips_tb1/code_r
add wave -noupdate -color Sienna -format Logic /mips_tb1/vma_code
add wave -noupdate -color {Cornflower Blue} -format Literal -radix hexadecimal /mips_tb1/full_rd_addr
add wave -noupdate -color {Light Steel Blue} -format Literal -radix hexadecimal /mips_tb1/data_r
add wave -noupdate -color {Medium Slate Blue} -format Logic /mips_tb1/vma_data
add wave -noupdate -color Wheat -format Literal -radix hexadecimal /mips_tb1/full_wr_addr
add wave -noupdate -color Khaki -format Literal -radix hexadecimal /mips_tb1/data_w
add wave -noupdate -color Coral -format Literal /mips_tb1/byte_we
add wave -noupdate -color Gray90 -format Logic /mips_tb1/interrupt
add wave -noupdate -color Orchid -format Logic /mips_tb1/mem_wait
add wave -noupdate -divider Internal
add wave -noupdate -color {Dark Olive Green} -format Logic /mips_tb1/uut/stall_pipeline
add wave -noupdate -color {Dark Olive Green} -format Logic /mips_tb1/uut/pipeline_stalled
add wave -noupdate -color {Indian Red} -format Literal -radix hexadecimal /mips_tb1/uut/p1_ir_reg
add wave -noupdate -divider Debug
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {13810000 ps} 0}
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
WaveRestoreZoom {0 ps} {42094500 ps}
