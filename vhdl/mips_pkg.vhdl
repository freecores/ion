--------------------------------------------------------------------------------
-- mips_pkg.vhdl -- Configuration constants & utility types and functions
--------------------------------------------------------------------------------
-- IMPORTANT:
-- Here's where you define the memory map of the system, in the implementation 
-- of function decode_addr. 
-- You need to change that function to change the memory map, independent of any
-- additional address decoding you may do out of the FPGA (e.g. if you have more
-- than one chip on any data bus) or out of the MCU module (e.g. when you add
-- new IO registers).
-- Please see the module c2sb_demo and mips_mcu for examples of memory decoding.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

package mips_pkg is

---- Basic types ---------------------------------------------------------------

subtype t_word is std_logic_vector(31 downto 0);


---- System configuration constants --------------------------------------------

-- True to use standard-ish MIPS-1 memory map, false to use Plasma's
-- (see implementation of function decode_addr below).
constant USE_MIPS1_ADDR_MAP : boolean := true;

-- Reset vector address minus 4 (0xfffffffc for Plasma, 0xbfbffffc for mips1)
constant RESET_VECTOR_M4 : t_word   := X"bfbffffc";

-- Trap vector address (0x0000003c for Plasma, 0xbfc00180 for mips1)
constant TRAP_VECTOR : t_word       := X"bfc00180";


---- Address decoding ----------------------------------------------------------

-- Note: it is the cache module that does all internal address decoding --------

-- This is the slice of the address that will be used to decode memory areas
subtype t_addr_decode is std_logic_vector(31 downto 24);

-- Part of the memory area attribute: the type of memory determines how the
-- cache module handles each block
subtype t_memory_type is std_logic_vector(7 downto 5);
-- These are all the types the cache knows about
constant MT_BRAM : t_memory_type            := "000";
constant MT_IO_SYNC : t_memory_type         := "001";
constant MT_SRAM_16B : t_memory_type        := "010";
constant MT_SRAM_8B : t_memory_type         := "011";
constant MT_DDR_16B : t_memory_type         := "100";
constant MT_UNMAPPED : t_memory_type        := "111";

-- Wait state counter -- we're supporting static memory from 10 to >100 ns
subtype t_wait_state_count is std_logic_vector(2 downto 0);

-- 'Attributes' of some memory block -- used when decoding memory addresses
type t_range_attr is record
    mem_type :          t_memory_type;
    writeable :         std_logic;
    cacheable :         std_logic;    
    wait_states :       t_wait_state_count;
end record t_range_attr;



---- More basic types and constants --------------------------------------------

subtype t_addr is std_logic_vector(31 downto 0);
subtype t_dword is std_logic_vector(63 downto 0);
subtype t_regnum is std_logic_vector(4 downto 0);
type t_rbank is array(0 to 31) of t_word;
subtype t_pc is std_logic_vector(31 downto 2);
-- This is used as a textual shortcut only
constant ZERO : t_word := (others => '0');
-- control word for ALU
type t_alu_control is record
    logic_sel :         std_logic_vector(1 downto 0);
    shift_sel :         std_logic_vector(1 downto 0);
    shift_amount :      std_logic_vector(4 downto 0);
    neg_sel :           std_logic_vector(1 downto 0);
    use_arith :         std_logic;
    use_logic :         std_logic_vector(1 downto 0);
    cy_in :             std_logic;
    use_slt :           std_logic;
    arith_unsigned :    std_logic;
end record t_alu_control;
-- Flags coming from the ALU
type t_alu_flags is record
    inp1_lt_zero :      std_logic;
    inp1_eq_zero :      std_logic;
    inp1_lt_inp2 :      std_logic;
    inp1_eq_inp2 :      std_logic;
end record t_alu_flags;

-- Debug info output by sinthesizable MPU core; meant to debug the core itself, 
-- not to debug software!
type t_debug_info is record
    cache_enabled :     std_logic;
    unmapped_access :   std_logic;
end record t_debug_info;


-- 32-cycle mul/div module control. Bits 4-3 & 1-0 of IR.
subtype t_mult_function is std_logic_vector(3 downto 0);
constant MULT_NOTHING       : t_mult_function := "0000";
constant MULT_READ_LO       : t_mult_function := "1010"; -- 18
constant MULT_READ_HI       : t_mult_function := "1000"; -- 16
constant MULT_WRITE_LO      : t_mult_function := "1011"; -- 19
constant MULT_WRITE_HI      : t_mult_function := "1001"; -- 17
constant MULT_MULT          : t_mult_function := "1101"; -- 25
constant MULT_SIGNED_MULT   : t_mult_function := "1100"; -- 24
constant MULT_DIVIDE        : t_mult_function := "1111"; -- 26
constant MULT_SIGNED_DIVIDE : t_mult_function := "1110"; -- 27

-- Computes ceil(log2(A)), e.g. address width of memory block
-- CAN BE USED IN SYNTHESIZABLE CODE as long as called with constant arguments
function log2(A : natural) return natural;

-- Decodes a memory address, gives the type of memory
-- CAN BE USED IN SYNTHESIZABLE CODE, argument does not need to be constant
function decode_addr(addr : t_addr_decode) return t_range_attr;


end package;

package body mips_pkg is

function log2(A : natural) return natural is
begin
    for I in 1 to 30 loop -- Works for up to 32 bit integers
        if(2**I >= A) then 
            return(I);
        end if;
    end loop;
    return(30);
end function log2;

-- Address decoding for Plasma-like system
function decode_addr_plasma(addr : t_addr_decode) return t_range_attr is
begin

    case addr(31 downto 27) is 
    when "00000"    => return (MT_BRAM     ,'0','0',"000"); -- useg
    when "10000"    => return (MT_SRAM_16B ,'1','1',"000"); -- kseg0
    when "00100"    => return (MT_IO_SYNC  ,'1','0',"000"); -- kseg1 i/o
    when others     => return (MT_UNMAPPED ,'0','0',"000"); -- stray
    end case;

end function decode_addr_plasma;

-- Address decoding for MIPS-I-like system as implemented in target hardware
function decode_addr_mips1(addr : t_addr_decode) return t_range_attr is
begin

    case addr(31 downto 27) is 
    when "00000"    => return (MT_SRAM_16B ,'1','1',"010"); -- useg
    when "10000"    => return (MT_SRAM_16B ,'1','1',"010"); -- kseg0
    --when "10100"    => return (MT_IO_SYNC  ,'1','0',"000"); -- kseg1 i/o
    when "00100"    => return (MT_IO_SYNC  ,'1','0',"000"); -- kseg1 i/o
    when "10110"    => return (MT_SRAM_8B  ,'0','0',"111"); -- kseg1 flash
    when "10111"    => return (MT_BRAM     ,'0','0',"000"); -- kseg1 boot rom
    when others     => return (MT_UNMAPPED ,'0','0',"000"); -- stray
    end case;

end function decode_addr_mips1;


function decode_addr(addr : t_addr_decode) return t_range_attr is
begin
    if USE_MIPS1_ADDR_MAP then
        return decode_addr_mips1(addr);
    else
        return decode_addr_plasma(addr);
    end if;

end function decode_addr;

end package body;
