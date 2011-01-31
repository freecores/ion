--------------------------------------------------------------------------------
-- ion_cpu.vhdl -- MIPS-I(tm) compatible CPU core
--------------------------------------------------------------------------------
-- project:       ION (http://www.opencores.org/project,ion_cpu)
-- author:        Jose A. Ruiz (ja_rd@hotmail.com)
-- created:       Jan/11/2011
-- last modified: Jan/31/2011 (ja_rd@hotmail.com)
--------------------------------------------------------------------------------
-- Software placed into the public domain by the author. Use under the terms of
-- the GPL.
-- Software 'as is' without warranty.  Author liable for nothing.
--
--------------------------------------------------------------------------------
--### MIPS-I things not implemented
--  # Invalid instruction trapping:
--      * invalid opcodes do trap but the logic that prevents bad opcodes from
--        having side affects has not been tested yet.
--  # Kernel/user status
--  # RTE instruction (or ERET)
--  # Most of the CP0 registers and of course all of the CP1
--  # External interrupts
--
--### Things implemented but not tested
--  # Memory pause input
--
--### Things with provisional implementation
-- 
-- 1.- Load interlocks: the pipeline is stalled for every load instruction, even
--     if the target register is not used in the following instruction. So that
--     every load takes two cycles.
--     The interlock logic should check register indices.
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use work.mips_pkg.all;

entity mips_cpu is
    generic(
        XILINX_REGBANK  : string  := "distributed" -- {distributed|block}
    );
    port(
        clk             : in std_logic;
        reset           : in std_logic;
        interrupt       : in std_logic;

        data_rd_addr    : out std_logic_vector(31 downto 0);
        data_rd         : in std_logic_vector(31 downto 0);
        data_rd_vma     : out std_logic;
        
        code_rd_addr    : out std_logic_vector(31 downto 2);
        code_rd         : in std_logic_vector(31 downto 0);
        code_rd_vma     : out std_logic;
        
        data_wr_addr    : out std_logic_vector(31 downto 2);
        byte_we         : out std_logic_vector(3 downto 0);
        data_wr         : out std_logic_vector(31 downto 0);
      
        mem_wait        : in std_logic
    );
end; --entity mips_cpu

architecture rtl of mips_cpu is

--------------------------------------------------------------------------------
-- Pipeline stage 0

signal p0_pc_reg :          t_pc;
signal p0_pc_restart :      t_pc;
signal p0_pc_incremented :  t_pc;
signal p0_pc_jump :         t_pc;
signal p0_pc_branch :       t_pc;
signal p0_pc_target :       t_pc;
signal p0_pc_next :         t_pc;
signal p0_rs_num :          t_regnum;
signal p0_rt_num :          t_regnum;
signal p0_jump_cond_value : std_logic;
signal p0_rbank_rs_hazard : std_logic;
signal p0_rbank_rt_hazard : std_logic;
signal p0_uses_rs1 :        std_logic;
signal p0_uses_rs2 :        std_logic;

signal p1_rs1_hazard :      std_logic;
signal p1_rs2_hazard :      std_logic;

--------------------------------------------------------------------------------
-- Pipeline stage 1


signal p1_rbank :           t_rbank := (others => X"00000000");

-- IMPORTANT: This attribute is used by Xilinx tools to select how to implement
-- the register bank. If we don't use it, by default XST would infer 2 BRAMs for
-- the 1024-bit 3-port reg bank, which you probably don't want.
-- This can take the values {distributed|block}.
attribute ram_style :       string;
attribute ram_style of p1_rbank : signal is XILINX_REGBANK;

signal p1_rs, p1_rt :       t_word;
signal p1_rs_rbank :        t_word;
signal p1_rt_rbank :        t_word;
signal p1_rbank_forward :   t_word;
signal p1_rd_num :          t_regnum;
signal p1_c0_rs_num :       t_regnum;
signal p1_rbank_wr_addr :   t_regnum;
signal p1_rbank_we :        std_logic;
signal p1_rbank_wr_data :   t_word;
signal p1_alu_inp1 :        t_word;
signal p1_alu_inp2 :        t_word;
signal p1_alu_outp :        t_word;
-- ALU control inputs (shortened name for brevity in expressions)
signal p1_ac :              t_alu_control;
-- ALU flag outputs (comparison results)
signal p1_alu_flags :       t_alu_flags;
-- immediate data, sign- or zero-extended as required by IR
signal p1_data_imm :        t_word;
signal p1_branch_offset :   t_pc;
signal p1_branch_offset_sex:std_logic_vector(31 downto 18);
signal p1_rbank_rs_hazard : std_logic;
signal p1_rbank_rt_hazard : std_logic;
signal p1_jump_type_set0 :  std_logic_vector(1 downto 0);
signal p1_jump_type_set1 :  std_logic_vector(1 downto 0);
signal p1_ir_reg :          std_logic_vector(31 downto 0);
signal p1_ir_op :           std_logic_vector(31 downto 26);
signal p1_ir_fn :           std_logic_vector(5 downto 0);
signal p1_op_special :      std_logic;
signal p1_exception :       std_logic;
signal p1_do_reg_jump :     std_logic;
signal p1_do_zero_ext_imm : std_logic;
signal p1_set_cp0 :         std_logic;
signal p1_get_cp0 :         std_logic;
signal p1_alu_op2_sel :     std_logic_vector(1 downto 0);
signal p1_alu_op2_sel_set0: std_logic_vector(1 downto 0);
signal p1_alu_op2_sel_set1: std_logic_vector(1 downto 0);
signal p1_do_load :         std_logic;
signal p1_do_store :        std_logic;
signal p1_store_size :      std_logic_vector(1 downto 0);
signal p1_we_control :      std_logic_vector(5 downto 0);
signal p1_load_alu :        std_logic;
signal p1_load_alu_set0 :   std_logic;
signal p1_load_alu_set1 :   std_logic;
signal p1_ld_upper_hword :  std_logic;
signal p1_ld_upper_byte :   std_logic;
signal p1_ld_unsigned :     std_logic;
signal p1_jump_type :       std_logic_vector(1 downto 0);
signal p1_link :            std_logic;
signal p1_jump_cond_sel :   std_logic_vector(2 downto 0);
signal p1_data_addr :       t_addr;
signal p1_data_offset :     t_addr;

signal p1_muldiv_result :   t_word;
signal p1_muldiv_func :     t_mult_function; 
signal p1_muldiv_running :  std_logic;
signal p1_muldiv_started :  std_logic;
signal p1_muldiv_stall :    std_logic;

signal p1_unknown_opcode :  std_logic;

--------------------------------------------------------------------------------
-- Pipeline stage 2

signal p2_muldiv_started :  std_logic;
signal p2_exception :       std_logic;
signal p2_rd_addr :         std_logic_vector(1 downto 0);
signal p2_rd_mux_control :  std_logic_vector(3 downto 0);
signal p2_load_target :     t_regnum;
signal p2_do_load :         std_logic;
signal p2_ld_upper_hword :  std_logic;
signal p2_ld_upper_byte :   std_logic;
signal p2_ld_unsigned :     std_logic;
signal p2_wback_mux_sel :   std_logic_vector(1 downto 0);
signal p2_data_word_rd :    t_word;
signal p2_data_word_ext :   std_logic;

--------------------------------------------------------------------------------
-- Global control signals 

signal load_interlock :     std_logic;
signal stall_pipeline :     std_logic;
-- pipeline is stalled for any reason
signal pipeline_stalled :   std_logic;
-- pipeline is stalled because of a load instruction interlock
signal pipeline_interlocked:std_logic;

--------------------------------------------------------------------------------
-- CP0 registers and signals

-- CP0[12]: status register 
signal cp0_status :         std_logic_vector(1 downto 0);
-- Output of CP0 register bank (only a few regs are implemented)
signal cp0_reg_read :       t_word;
-- CP0[14]: EPC register (PC value saved at exceptions)
signal cp0_epc :            t_pc;
-- CP0[13]: 'Cause' register (cause and attributes of exception)
signal cp0_cause :          t_word;
signal cp0_in_delay_slot :  std_logic;
signal cp0_cause_bd :       std_logic;
signal cp0_cause_ce :       std_logic_vector(1 downto 0);
signal cp0_cause_exc_code : std_logic_vector(4 downto 0);

begin

--##############################################################################
-- Register bank & datapath

-- Register indices are 'decoded' out of the instruction word BEFORE loading IR
p0_rs_num <= std_logic_vector(code_rd(25 downto 21));
with p1_ir_reg(31 downto 26) select p1_rd_num <= 
    p1_ir_reg(15 downto 11)    when "000000",
    p1_ir_reg(20 downto 16)    when others;

p0_rt_num <= std_logic_vector(code_rd(20 downto 16)); -- also called rs2 in the docs

--------------------------------------------------------------------------------
-- Data input shifter & masker (LB,LBU,LH,LHU,LW)

p2_rd_mux_control <= p2_ld_upper_hword & p2_ld_upper_byte & p2_rd_addr;

-- Extension for unused bits will be zero or the sign (bit 7 or bit 15)
p2_data_word_ext <= '0'         when p2_ld_unsigned='1' else
                    data_rd(15)  when p2_ld_upper_byte='1' else
                    data_rd(7)   when p2_rd_addr="11" else
                    data_rd(15)  when p2_rd_addr="10" else
                    data_rd(23);

-- byte 0 may come from any of the 4 bytes of the input word
with p2_rd_mux_control select p2_data_word_rd(7 downto 0) <=
    data_rd(31 downto 24)        when "0000",
    data_rd(23 downto 16)        when "0001",
    data_rd(23 downto 16)        when "0100",
    data_rd(15 downto  8)        when "0010",
    data_rd( 7 downto  0)        when others;
    
-- byte 1 may come from input bytes 1 or 3 or may be extended for LB, LBU
with p2_rd_mux_control select p2_data_word_rd(15 downto 8) <=
    data_rd(31 downto 24)        when "0100",
    data_rd(15 downto  8)        when "0110",
    data_rd(15 downto  8)        when "1100",
    data_rd(15 downto  8)        when "1101",
    data_rd(15 downto  8)        when "1110",
    data_rd(15 downto  8)        when "1111",
    (others => p2_data_word_ext) when others;

-- bytes 2,3 come straight from input or are extended for LH,LHU
with p2_ld_upper_hword select p2_data_word_rd(31 downto 16) <=
    (others => p2_data_word_ext)    when '0',
    data_rd(31 downto 16)            when others;

-- Select which data is to be written back to the reg bank and where
p1_rbank_wr_addr <= p1_rd_num   when p2_do_load='0' and p1_link='0' else
                    "11111"     when p2_do_load='0' and p1_link='1' else 
                    p2_load_target;

p2_wback_mux_sel <= 
    "00" when p2_do_load='0' and p1_get_cp0='0' and p1_link='0' else
    "01" when p2_do_load='1' and p1_get_cp0='0' and p1_link='0' else
    "10" when p2_do_load='0' and p1_get_cp0='1' and p1_link='0' else
    "11";

with (p2_wback_mux_sel) select p1_rbank_wr_data <=
    p1_alu_outp                when "00",
    p2_data_word_rd            when "01",
    p0_pc_incremented & "00"   when "11",
    cp0_reg_read               when others;

p1_rbank_we <= '1' when (p2_do_load='1' or p1_load_alu='1' or 
                        p1_link='1' or p1_get_cp0='1') and 
                        p1_rbank_wr_addr/="00000" and
                        -- on exception, abort next instruction (by preventing 
                        -- regbank writeback).
                        p2_exception='0'
                else '0';

-- Register bank as triple-port RAM. Should synth to 2 BRAMs unless you use
-- synth attributes to prevent it (see 'ram_style' attribute above) or your
-- FPGA has 3-port BRAMS, or has none.
synchronous_reg_bank:
process(clk)
begin
    if clk'event and clk='1' then
        if p1_rbank_we='1' and 
           (pipeline_stalled='0' or pipeline_interlocked='1') then -- @note1
            p1_rbank(conv_integer(p1_rbank_wr_addr)) <= p1_rbank_wr_data;
        end if;
        p1_rt_rbank <= p1_rbank(conv_integer(p0_rt_num));
        p1_rs_rbank <= p1_rbank(conv_integer(p0_rs_num));
    end if;
end process synchronous_reg_bank;

-- Register writeback data in case it needs to be forwarded.
data_forward_register:
process(clk)
begin
    if clk'event and clk='1' then
        if p1_rbank_we='1' then -- no need to check for stall cycles
            p1_rbank_forward <= p1_rbank_wr_data;
        end if;
    end if;
end process data_forward_register;

-- Bypass sync RAM if we're reading and writing to the same address. This saves
-- 1 stall cycle and fixes the data hazard.
p0_rbank_rs_hazard <= '1' when p1_rbank_wr_addr=p0_rs_num and p1_rbank_we='1' 
                      else '0';
p0_rbank_rt_hazard <= '1' when p1_rbank_wr_addr=p0_rt_num and p1_rbank_we='1' 
                      else '0';

p1_rs <= p1_rs_rbank when p1_rbank_rs_hazard='0' else p1_rbank_forward;
p1_rt <= p1_rt_rbank when p1_rbank_rt_hazard='0' else p1_rbank_forward;

-- Zero extension/Sign extension for instruction immediate data
p1_data_imm(15 downto 0)  <= p1_ir_reg(15 downto 0);

with p1_do_zero_ext_imm select p1_data_imm(31 downto 16) <= 
    (others => '0')             when '1',
    (others => p1_ir_reg(15))   when others;


--------------------------------------------------------------------------------
-- ALU & ALU input multiplexors

p1_alu_inp1 <= p1_rs;

with p1_alu_op2_sel select p1_alu_inp2 <= 
    p1_data_imm         when "11",
    p1_muldiv_result    when "01",
    --p1_muldiv_result    when "10", -- FIXME mux input wasted!
    p1_rt               when others;

alu_inst : entity work.mips_alu
    port map (
        clk             => clk,
        reset           => reset,
        ac              => p1_ac,
        flags           => p1_alu_flags,
        
        inp1            => p1_alu_inp1,
        inp2            => p1_alu_inp2,
        outp            => p1_alu_outp
    );


--------------------------------------------------------------------------------
-- Mul/Div block interface

-- Compute the mdiv block function word. If p1_muldiv_func has any value other
-- than MULT_NOTHING a new mdiv operation will start, truncating whatever other
-- operation that may have been in course.
-- So we encode here the function to be performed and make sure the value stays
-- there for only one cycle (the first ALU cycle of the mul/div instruction).

-- This will be '1' for all mul/div operations other than NOP...
p1_muldiv_func(3) <= '1' when p1_op_special='1' and 
                              p1_ir_fn(5 downto 4)="01" and
                              -- ...but only if the mdiv is not already running
                              p2_muldiv_started = '0' and
                              p1_muldiv_running ='0'
                      else '0';

-- When bit(3) is zero, the rest are zeroed too. Otherwise, they come from IR
p1_muldiv_func(2 downto 0) <= 
    p1_ir_fn(3) & p1_ir_fn(1 downto 0) when p1_muldiv_func(3)='1'
    else "000";

mult_div: entity work.mips_mult
    port map (
        a           => p1_rs,
        b           => p1_rt,
        c_mult      => p1_muldiv_result,
        pause_out   => p1_muldiv_running,
        mult_func   => p1_muldiv_func,
        clk         => clk,
        reset_in    => reset
    );

-- Active only for the 1st ALU cycle of any mul/div instruction
p1_muldiv_started <= '1' when p1_op_special='1' and 
                              p1_ir_fn(5 downto 3)="011" and
                              -- 
                              p1_muldiv_running='0'
                      else '0';

-- Stall the pipeline to enable mdiv operation completion.
-- We need p2_muldiv_started to distinguish the cycle before p1_muldiv_running
-- is asserted and the cycle after it deasserts.
-- Otherwise we would reexecute the same muldiv endlessly instruction after 
-- deassertion of p1_muldiv_running, since the IR was stalled and still contains 
-- the mul opcode...
p1_muldiv_stall <= '1' when
        -- Active for the cycle immediately before p1_muldiv_running asserts
        -- and NOT for the cycle after it deasserts
        (p1_muldiv_started='1' and p2_muldiv_started='0') or
        -- Active until operation is complete
        p1_muldiv_running = '1'
        else '0';


--##############################################################################
-- PC register and branch logic

-- p0_pc_reg will not be incremented on stall cycles
p0_pc_incremented <= p0_pc_reg + (not stall_pipeline);

-- main pc mux: jump or continue
p0_pc_next <= 
    p0_pc_target when
        -- We jump on jump instructions whose condition is met...
        ((p1_jump_type(1)='1' and p0_jump_cond_value='1' and 
        -- ...except we abort any jump that follows the victim of an exception
          p2_exception='0') or
        -- We jump on exceptions too...
        p1_exception='1') 
        -- ... but we only jump at all if the pipeline is not stalled
        and stall_pipeline='0'
    else p0_pc_incremented;

pc_register:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            -- reset to 0xffffffff so that 1st fetch addr is 0x00000000
            p0_pc_reg <= (others => '1');
        else
            -- p0_pc_reg holds the same value as external sync ram addr register
            p0_pc_reg <= p0_pc_next;
            -- p0_pc_restart = addr saved to EPC on interrupts (@note2)
            -- It's the addr of the instruction triggering the exception,
            -- except when the triggering instruction is in a delay slot. In 
            -- that case, this is the previous jump instruction address.
            -- I.e. all as per the mips-1 specs.
            if (p1_jump_type="00" or p0_jump_cond_value='0') then 
                p0_pc_restart <= p0_pc_reg;
                -- remember if we are in a delay slot, in case there's a trap
                cp0_in_delay_slot <= '0'; -- NOT in a delay slot
            else
                cp0_in_delay_slot <= '1'; -- in a delay slot
            end if;
        end if;
    end if;
end process pc_register;

-- p0_pc_reg holds the same addr as the addr register of the external synchronous 
-- memory; what we put on the addr bus is p0_pc_next.
data_rd_addr <= p1_data_addr(31 downto 0);

-- FIXME these two need to pushed behind a register, they are glitch-prone
data_rd_vma <= p1_do_load and not pipeline_stalled; -- FIXME register
code_rd_vma <= not stall_pipeline; -- FIXME register

code_rd_addr <= p0_pc_next;

data_wr_addr <= p1_data_addr(31 downto 2);

-- compute target of J/JR instructions
p0_pc_jump <=   p1_rs(31 downto 2) when p1_do_reg_jump='1' else
                p0_pc_reg(31 downto 28) & p1_ir_reg(25 downto 0); 

-- compute target of relative branch instructions
p1_branch_offset_sex <= (others => p1_ir_reg(15));
p1_branch_offset <= p1_branch_offset_sex & p1_ir_reg(15 downto 0);
-- p0_pc_reg is the addr of the instruction in delay slot
p0_pc_branch <= p0_pc_reg + p1_branch_offset;

-- decide which jump target is to be used
p0_pc_target <= X"0000003"&"11"     when p1_exception='1' else 
             p0_pc_jump             when p1_jump_type(0)='1' else 
             p0_pc_branch;


--##############################################################################
-- Instruction decoding and IR

instruction_register:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            p1_ir_reg <= (others => '0');
        elsif stall_pipeline='0' then
            p1_ir_reg <= code_rd;
        end if;
    end if;
end process instruction_register;

-- 'Extract' main fields from IR, for convenience
p1_ir_op <= p1_ir_reg(31 downto 26);
p1_ir_fn <= p1_ir_reg(5 downto 0);

-- Decode jump type, if any, for instructions with op/=0
with p1_ir_op select p1_jump_type_set0 <=
    -- FIXME weed out invalid instructions
    "10" when "000001", -- BLTZ, BGEZ, BLTZAL, BGTZAL
    "11" when "000010", -- J
    "11" when "000011", -- JAL
    "10" when "000100", -- BEQ
    "10" when "000101", -- BNE
    "10" when "000110", -- BLEZ
    "10" when "000111", -- BGTZ
    "00" when others;   -- no jump

-- Decode jump type, if any, for instructions with op=0
p1_jump_type_set1 <= "11" when p1_op_special='1' and 
                               p1_ir_reg(5 downto 1)="00100" 
                     else "00";

-- Decode jump type for the instruction in IR (composite of two formats)
p1_jump_type <= p1_jump_type_set0 or p1_jump_type_set1;

p1_link <= '1' when (p1_ir_op="000000" and p1_ir_reg(5 downto 0)="001001") or
                    (p1_ir_op="000001" and p1_ir_reg(20)='1') or
                    (p1_ir_op="000011")
           else '0';

-- Decode jump condition: encode a mux control signal from IR...
p1_jump_cond_sel <= 
    "001" when p1_ir_op="000001" and p1_ir_reg(16)='0' else --   op1 < 0   BLTZ*
    "101" when p1_ir_op="000001" and p1_ir_reg(16)='1' else -- !(op1 < 0) BNLTZ*
    "010" when p1_ir_op="000100" else                       --   op1 == op2  BEQ
    "110" when p1_ir_op="000101" else                       -- !(op1 == op2) BNE
    "011" when p1_ir_op="000110" else                       --   op1 <= 0   BLEZ
    "111" when p1_ir_op="000111" else                       -- !(op1 <= 0)  BGTZ
    "000";                                                  -- always

-- ... and use mux control signal to select the condition value
with p1_jump_cond_sel select p0_jump_cond_value <=
        p1_alu_flags.inp1_lt_zero       when "001",
    not p1_alu_flags.inp1_lt_zero       when "101",
        p1_alu_flags.inp1_eq_inp2       when "010",
    not p1_alu_flags.inp1_eq_inp2       when "110",
        (p1_alu_flags.inp1_lt_inp2 or 
         p1_alu_flags.inp1_eq_inp2)     when "011",
    not (p1_alu_flags.inp1_lt_inp2 or 
         p1_alu_flags.inp1_eq_inp2)     when "111",
    '1'                                 when others;

-- Decode instructions that launch exceptions
p1_exception <= '1' when 
    (p1_op_special='1' and p1_ir_reg(5 downto 1)="00110") or
    p1_unknown_opcode='1'
    else '0';

-- Decode MTC0/MFC0 instructions
p1_set_cp0 <= '1' when p1_ir_reg(31 downto 21)="01000000100" else '0';
p1_get_cp0 <= '1' when p1_ir_reg(31 downto 21)="01000000000" else '0';

-- FIXME elaborate and explain this

p1_op_special <= '1' when p1_ir_op="000000" else '0';

p1_do_reg_jump <= '1' when p1_op_special='1' and p1_ir_fn(5 downto 1)="00100" else '0';
p1_do_zero_ext_imm <= '1' when (p1_ir_op(31 downto 28)="0011") else '0';

-- Decode input data mux control (LW, LH, LB, LBU, LHU) and load enable
p1_do_load <= '1' when 
    p1_ir_op(31 downto 29)="100" and
    p1_ir_op(28 downto 26)/="010" and -- LWL
    p1_ir_op(28 downto 26)/="110" and -- LWR
    p1_ir_op(28 downto 26)/="111" and -- LWR
    p2_exception='0'  -- abort load if previous instruction triggered trap
    else '0';

p1_load_alu_set0 <= '1' 
    when p1_op_special='1' and 
        ((p1_ir_op(31 downto 29)="000" and p1_ir_op(27 downto 26)="00") or
         (p1_ir_op(31 downto 29)="000" and p1_ir_op(27 downto 26)="10") or
         (p1_ir_op(31 downto 29)="000" and p1_ir_op(27 downto 26)="11") or
         (p1_ir_op(31 downto 29)="000" and p1_ir_op(27 downto 26)="00") or
         (p1_ir_op(31 downto 28)="0100" and p1_ir_op(27 downto 26)="00") or
         (p1_ir_op(31 downto 28)="0100" and p1_ir_op(27 downto 26)="10") or
         (p1_ir_op(31 downto 28)="1000") or
         (p1_ir_op(31 downto 28)="1001") or
         (p1_ir_op(31 downto 28)="1010" and p1_ir_op(27 downto 26)="10") or
         (p1_ir_op(31 downto 28)="1010" and p1_ir_op(27 downto 26)="11") or
         (p1_ir_op(31 downto 28)="0010" and p1_ir_op(27 downto 26)="01"))
    else '0';
    
with p1_ir_op select p1_load_alu_set1 <= 
    '1' when "001000",  -- addi
    '1' when "001001",  -- addiu
    '1' when "001010",  -- slti
    '1' when "001011",  -- sltiu
    '1' when "001100",  -- andi
    '1' when "001101",  -- ori
    '1' when "001110",  -- xori
    '1' when "001111",  -- lui
    '0' when others;
p1_load_alu <= (p1_load_alu_set0 or p1_load_alu_set1) and
                not p1_unknown_opcode;

p1_ld_upper_hword <= p1_ir_op(27); -- use input upper hword vs. sign extend/zero
p1_ld_upper_byte <= p1_ir_op(26);  -- use input upper byte vs. sign extend/zero
p1_ld_unsigned <= p1_ir_op(28);    -- sign extend vs. zero extend

-- ALU input-2 selection: use external data for 2x opcodes (loads)
p1_alu_op2_sel_set0 <= 
    "11" when    p1_ir_op(31 downto 30)="10" or p1_ir_op(29)='1' else 
    "00";

-- ALU input-2 selection: use registers Hi and Lo for MFHI, MFLO
p1_alu_op2_sel_set1 <= 
    "01" when p1_op_special='1' and (p1_ir_fn="010000" or p1_ir_fn="010010")
    else "00";
    
-- ALU input-2 final selection
p1_alu_op2_sel <= p1_alu_op2_sel_set0 or p1_alu_op2_sel_set1;

-- Decode store operations
p1_do_store <= '1' when 
    p1_ir_op(31 downto 29)="101" and
    (p1_ir_op(28 downto 26)="000" or -- SB
     p1_ir_op(28 downto 26)="001" or -- SH
     p1_ir_op(28 downto 26)="011") and -- SWH
    p2_exception='0'    -- abort when previous instruction triggered exception
    else '0';
p1_store_size <= p1_ir_op(27 downto 26);


-- Extract source and destination C0 register indices
p1_c0_rs_num <= p1_ir_reg(15 downto 11);

-- Decode ALU control dignals

p1_ac.use_slt <= '1' when (p1_ir_op="000001" and p1_ir_reg(20 downto 17)="01000") or
                        (p1_ir_op="000000" and p1_ir_reg(5 downto 1)="10101") or
                        p1_ir_op="001010" or p1_ir_op="001011"
               else '0';
p1_ac.arith_unsigned <= p1_ac.use_slt and p1_ir_reg(0);

p1_ac.use_logic(0) <= '1' when (p1_op_special='1' and p1_ir_fn(5 downto 3)/="000") or
                    -- all immediate arith and logic
                    p1_ir_op(31 downto 29)="001"
                 else '0';
p1_ac.use_logic(1) <= '1' when (p1_op_special='1' and p1_ir_fn="100111") else '0';

p1_ac.use_arith <= '1' when p1_ir_op(31 downto 28)="0010" or 
                            (p1_op_special='1' and 
                                (p1_ir_fn(5 downto 2)="1000" or
                                p1_ir_fn(5 downto 2)="1010"))
                 else '0';

-- selection of 2nd internal alu operand: {i2, /i2, i2<<16, 0x0}
p1_ac.neg_sel(1)<= '1' when p1_ir_op(29 downto 26) = "1111" else '0';
p1_ac.neg_sel(0)<= '1' when   p1_ir_op="001010" or 
                            p1_ir_op="001011" or
                            p1_ir_op(31 downto 28)="0001" or
                            (p1_op_special='1' and
                                (p1_ir_fn="100010" or
                                 p1_ir_fn="100011" or
                                 p1_ir_fn(5 downto 2)="1010"))
                 else '0';
p1_ac.cy_in <= p1_ac.neg_sel(0);

p1_ac.shift_sel <= p1_ir_fn(1 downto 0);

p1_ac.logic_sel <= "00" when (p1_op_special='1' and p1_ir_fn="100100") else
                 "01" when (p1_op_special='1' and p1_ir_fn="100101") else
                 "10" when (p1_op_special='1' and p1_ir_fn="100110") else
                 "01" when (p1_op_special='1' and p1_ir_fn="100111") else
                 "00" when (p1_ir_op="001100") else
                 "01" when (p1_ir_op="001101") else
                 "10" when (p1_ir_op="001110") else
                 "11";

p1_ac.shift_amount <= p1_ir_reg(10 downto 6) when p1_ir_fn(2)='0' else p1_rs(4 downto 0);


--------------------------------------------------------------------------------
-- Decoding of unimplemented and privileged instructions

-- Unimplemented instructions include:
--  1.- All instructions above architecture MIPS-I except:
--      1.1.- eret
--  2.- Unaligned stores and loads (LWL,LWR,SWL,SWR)
--  3.- All CP0 instructions other than mfc0 and mtc0
--  4.- All CPi instructions
--  5.- All cache instructions
-- For the time being, we'll decode them all together.

-- FIXME: some of these should trap but others should just NOP (e.g. EHB)

p1_unknown_opcode <= '1' when
    -- decode by 'opcode' field
    p1_ir_op(31 downto 29)="011" or
    p1_ir_op(31 downto 29)="110" or
    p1_ir_op(31 downto 29)="111" or
    (p1_ir_op(31 downto 29)="010" and p1_ir_op(28 downto 26)/="000") or
    p1_ir_op="101111" or    -- CACHE
    p1_ir_op="100010" or    -- LWL
    p1_ir_op="100110" or    -- LWR
    p1_ir_op="101010" or    -- SWL
    p1_ir_op="101110" or    -- SWR
    p1_ir_op="100111" or
    p1_ir_op="101100" or
    p1_ir_op="101101" or
    -- decode instructions in the 'special' opcode group
    (p1_ir_op="000000" and 
                (p1_ir_fn(5 downto 4)="11" or
                 p1_ir_fn="000001" or
                 p1_ir_fn="000101" or
                 p1_ir_fn="001010" or
                 p1_ir_fn="001011" or
                 p1_ir_fn="001110" or
                 p1_ir_fn(5 downto 2)="0101" or
                 p1_ir_fn(5 downto 2)="0111" or
                 p1_ir_fn(5 downto 2)="1011")) or
    -- decode instructions in the 'regimm' opcode group
    (p1_ir_op="000001" and 
                (p1_ir_reg(20 downto 16)/="00000" and -- BLTZ is valid
                 p1_ir_reg(20 downto 16)/="00001" and -- BGEZ is valid
                 p1_ir_reg(20 downto 16)/="10000" and -- BLTZAL is valid 
                 p1_ir_reg(20 downto 16)/="10001")) -- BGEZAL is valid

    else '0';

--------------------------------------------------------------------------------

-- Stage 1 pipeline register. Involved in ALU control.
pipeline_stage1_register:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            p1_rbank_rs_hazard <= '0';
            p1_rbank_rt_hazard <= '0';
        elsif stall_pipeline='0' then
            p1_rbank_rs_hazard <= p0_rbank_rs_hazard;
            p1_rbank_rt_hazard <= p0_rbank_rt_hazard;
        end if;
    end if;
end process pipeline_stage1_register;

pipeline_stage1_register2:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            p2_muldiv_started <= '0';
        else
            p2_muldiv_started <= p1_muldiv_running;
        end if;
    end if;
end process pipeline_stage1_register2;


-- Stage 2 pipeline register. Split in two for convenience.
-- This register deals with two kinds of stalls:
-- * When the pipeline stalls because of a load interlock, this register is 
--   allowed to update so that the load operation can complete while the rest of
--   the pipeline is frozen.
-- * When the stall is caused by any other reason, this register freezes with 
--   the rest of the machine.

-- Part of stage 2 register that controls load operation
pipeline_stage2_register_load_control:
process(clk)
begin
    if clk'event and clk='1' then
        -- Clear load control, effectively preventing load, at reset or if
        -- the previous instruction raised an exception.
        if reset='1' or p2_exception='1' then
            p2_do_load <= '0';
            p2_ld_upper_hword <= '0';
            p2_ld_upper_byte <= '0';
            p2_ld_unsigned <= '0';
            p2_load_target <= "00000";
        
        -- Load signals from previous stage only if there is no pipeline stall
        -- unless the stall is caused by interlock (@note1).
        elsif (stall_pipeline='0' or load_interlock='1') then
            -- Disable reg bank writeback if pipeline is stalled; this prevents
            -- duplicate writes in case the stall is a mem_wait.
            if pipeline_stalled='0' then
                p2_do_load <= p1_do_load;
            else
                p2_do_load <= '0';
            end if;
            p2_load_target <= p1_rd_num;
            p2_ld_upper_hword <= p1_ld_upper_hword;
            p2_ld_upper_byte <= p1_ld_upper_byte;
            p2_ld_unsigned <= p1_ld_unsigned;
        end if;
    end if;
end process pipeline_stage2_register_load_control;

-- All the rest of the stage 2 register
pipeline_stage2_register_others:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            p2_exception <= '0';
            
        -- Load signals from previous stage only if there is no pipeline stall
        -- unless the stall is caused by interlock (@note1).
        elsif (stall_pipeline='0' or load_interlock='1') then
            p2_rd_addr <= p1_data_addr(1 downto 0);
            p2_exception <= p1_exception;
        end if;
    end if;
end process pipeline_stage2_register_others;

--------------------------------------------------------------------------------

-- FIXME stall when needed: mem pause, mdiv pause and load interlock

pipeline_stall_registers:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            pipeline_stalled <= '0';
            pipeline_interlocked <= '0';
        else
            if stall_pipeline='1' then
                pipeline_stalled <= '1';
            else
                pipeline_stalled <= '0';
            end if;
            if load_interlock='1' then
                pipeline_interlocked <= '1';
            else
                pipeline_interlocked <= '0';
            end if;
        end if;
    end if;
end process pipeline_stall_registers;

-- FIXME make sure this combinational will not have bad glitches
stall_pipeline <= mem_wait or load_interlock or p1_muldiv_stall;


-- FIXME load interlock should happen only if the instruction following 
-- the load actually uses the load target register. Something like this:
-- (p1_do_load='1' and (p1_rd_num=p0_rs_num or p1_rd_num=p0_rt_num))
load_interlock <= '1' when 
    p1_do_load='1' and      -- this is a load instruction
    pipeline_stalled='0' and -- not already stalled (i.e. assert for 1 cycle)
    (p1_rs1_hazard='1' or p1_rs2_hazard='1')
    
    else '0';

p1_rs1_hazard <= '1'; --'1' when p0_uses_rs1='1' and p1_rd_num=p0_rs_num else '0';
p1_rs2_hazard <= '1'; --'1' when p0_uses_rs2='1' and p1_rd_num=p0_rt_num else '0';

with p1_ir_op select p0_uses_rs1 <= 
    '0' when "000010",
    '0' when "000011",
    '0' when "001111",
    '0' when "001000",
    '1' when others;
    
with p1_ir_op select p0_uses_rs2 <= 
    '1' when "000000",
    '1' when "000100",
    '1' when "000101",
    '1' when "000110",
    '1' when "000111",
    '1' when "010000",
    '1' when "101000",
    '1' when "101001",
    '1' when "101010",
    '1' when "101011",
    '1' when "101110",
    '0' when others;
    

--------------------------------------------------------------------------------

p1_data_offset(31 downto 16) <= (others => p1_data_imm(15));
p1_data_offset(15 downto 0) <= p1_data_imm(15 downto 0);

p1_data_addr <= p1_rs + p1_data_offset;

--------------------------------------------------------------------------------

-- byte_we is a function of the write size and alignment
-- size = {00=1,01=2,11=4}; we 3 is MSB, 0 is LSB; big endian => 00 is msb
p1_we_control <= pipeline_stalled & p1_do_store & p1_store_size & p1_data_addr(1 downto 0);

with p1_we_control select byte_we <=
    "1000"  when "010000",    -- SB %0
    "0100"  when "010001",    -- SB %1
    "0010"  when "010010",    -- SB %2
    "0001"  when "010011",    -- SB %3
    "1100"  when "010100",    -- SH %0
    "0011"  when "010110",    -- SH %2
    "1111"  when "011100",    -- SW %4
    "0000"  when others; -- all other combinations are spurious so don't write

-- Data to be stored always comes straight from the reg bank, but it needs to 
-- be shifted so that the LSB is aligned to the write address:

data_wr(7 downto 0) <= p1_rt(7 downto 0);

with p1_we_control select data_wr(15 downto 8) <= 
    p1_rt( 7 downto  0) when "010010",  -- SB %2
    p1_rt(15 downto  8) when others;

with p1_we_control select data_wr(23 downto 16) <= 
    p1_rt( 7 downto  0) when "010001",  -- SB %1
    p1_rt( 7 downto  0) when "010100",  -- SH %0
    p1_rt(23 downto 16) when others;
    
with p1_we_control select data_wr(31 downto 24) <= 
    p1_rt( 7 downto  0) when "010000",  -- SB %0
    p1_rt(15 downto  8) when "010100",  -- SH %0
    p1_rt(31 downto 24) when others;


--##############################################################################
-- CP0 (what little is implemented of it)

process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            -- "10" => mode=kernel; ints=disabled
            cp0_status <= "10";
            cp0_cause_exc_code <= "00000";
            cp0_cause_bd <= '0';
        else
            -- no need to check for stall cycles when loading these
            if p1_set_cp0='1' then
                -- FIXME check for CP0 reg index
                cp0_status <= p1_rs(cp0_status'high downto 0);
            end if;
            if p1_exception='1' then
                cp0_epc <= p0_pc_restart;
                
                if p1_unknown_opcode='1' then
                    cp0_cause_exc_code <= "01010"; -- bad opcode
                else
                    if p1_ir_fn(0)='0' then
                        cp0_cause_exc_code <= "01000"; -- syscall
                    else
                        cp0_cause_exc_code <= "01001"; -- break
                    end if;
                end if;
                
                cp0_cause_bd <= cp0_in_delay_slot;
            end if;
        end if;
    end if;
end process;

cp0_cause_ce <= "00"; -- FIXME CP* traps merged with unimplemented opcode traps
cp0_cause <= cp0_cause_bd & '0' & cp0_cause_ce & 
             X"00000" & "000" & 
             cp0_cause_exc_code;

-- FIXME the mux should mask to zero for any unused reg index
with p1_c0_rs_num select cp0_reg_read <=
    X"0000000" & "00" & cp0_status  when "01100",
    cp0_cause                       when "01101",
    cp0_epc & "00"                  when others;


end architecture rtl;

--------------------------------------------------------------------------------
-- Implementation notes
--------------------------------------------------------------------------------
-- @note1 : 
--
-- This is the meaning of these two signals:
-- pipeline_stalled & pipeline_interlocked =>
--  "00" => normal state
--  "01" => normal state (makes for easier decoding)
--  "10" => all stages of pipeline stalled, including rbank
--  "11" => all stages of pipeline stalled, except reg bank write port
-- 
-- Just to clarify, 'stage X stalled' here means that the registers named 
-- pX_* don't load.
--
-- The register bank WE is enabled when the pipeline is not stalled and when 
-- it is stalled because of a load interlock; so that in case of interlock the
-- load operation can complete while the rest of the pipeline is frozen.
--------------------------------------------------------------------------------
