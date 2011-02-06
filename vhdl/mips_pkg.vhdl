library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

package mips_pkg is

-- FIXME this stuff belongs in the cache module where address decoding is made
-- (besides, they should be module generics, not package constants)
subtype t_addr_decode is std_logic_vector(31 downto 24);
constant ADDR_BOOT : t_addr_decode      := X"00";
constant ADDR_XRAM : t_addr_decode      := X"80";
constant ADDR_IO : t_addr_decode        := X"20";


subtype t_addr is std_logic_vector(31 downto 0);
subtype t_word is std_logic_vector(31 downto 0);
subtype t_dword is std_logic_vector(63 downto 0);
subtype t_regnum is std_logic_vector(4 downto 0);

type t_rbank is array(0 to 31) of t_word;

subtype t_pc is std_logic_vector(31 downto 2);

constant ZERO : t_word := (others => '0');

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

type t_alu_flags is record
    inp1_lt_zero :      std_logic;
    inp1_eq_zero :      std_logic;
    inp1_lt_inp2 :      std_logic;
    inp1_eq_inp2 :      std_logic;
end record t_alu_flags;

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


end package;

package body mips_pkg is

function log2(A : natural) return natural is
begin
    for I in 1 to 30 loop -- Works for up to 32 bit integers
        if(2**I > A) then 
            return(I-1);
        end if;
    end loop;
    return(30);
end function log2;

end package body;
