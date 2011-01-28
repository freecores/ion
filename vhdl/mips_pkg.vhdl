library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

package mips_pkg is

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

end package;
