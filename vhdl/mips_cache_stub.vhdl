--------------------------------------------------------------------------------
-- mips_cache_stub.vhdl -- cache module with no actual cache memory.
--
-- This module has the same interface as a real cache but has no cache memory.
-- It just interfaces the CPU to the following:
--
--  1.- Internal 32-bit-wide BRAM for read and write
--  2.- Internal 32-bit I/O bus
--  3.- External 16-bit wide SRAM
--
-- The SRAM memory interface signals are meant to connect directly to FPGA pins 
-- and all outputs are registered (tco should be minimal).
-- SRAM data inputs are NOT registered, though. They go through a couple muxes
-- before reaching the first register so watch out for tsetup.
-- The SRAM is assumed to be fast enough to read or write in a clock cycle.
--
-- Obviously this module provides no performance gain; on the contrary, by 
-- coupling the CPU to slow external memory (16 bit bus) it actually slows it
-- down. The purpose of this module is just to test the SRAM interface.
--
-- FIXME there HAS to be some explaination of the logic, it's not obvious!
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use work.mips_pkg.all;

entity mips_cache_stub is
    generic (
        BRAM_ADDR_SIZE : integer := 10;
        SRAM_ADDR_SIZE : integer := 17
    );
    port(
        clk             : in std_logic;
        reset           : in std_logic;
        
        -- Interface to CPU core
        data_rd_addr    : in std_logic_vector(31 downto 0);
        data_rd         : out std_logic_vector(31 downto 0);
        data_rd_vma     : in std_logic;
        
        code_rd_addr    : in std_logic_vector(31 downto 2);
        code_rd         : out std_logic_vector(31 downto 0);
        code_rd_vma     : in std_logic;
        
        data_wr_addr    : in std_logic_vector(31 downto 2);
        byte_we         : in std_logic_vector(3 downto 0);
        data_wr         : in std_logic_vector(31 downto 0);
      
        mem_wait        : out std_logic;
        
        -- interface to FPGA i/o devices
        io_rd_data      : in std_logic_vector(31 downto 0);
        io_rd_addr      : out std_logic_vector(31 downto 2);
        io_wr_addr      : out std_logic_vector(31 downto 2);
        io_wr_data      : out std_logic_vector(31 downto 0);
        io_rd_vma       : out std_logic;
        io_byte_we      : out std_logic_vector(3 downto 0);
        
        -- interface to synchronous 32-bit-wide FPGA BRAM (possibly used as ROM)
        bram_rd_data    : in std_logic_vector(31 downto 0);
        bram_wr_data    : out std_logic_vector(31 downto 0);
        bram_rd_addr    : out std_logic_vector(BRAM_ADDR_SIZE+1 downto 2);
        bram_wr_addr    : out std_logic_vector(BRAM_ADDR_SIZE+1 downto 2);
        bram_byte_we    : out std_logic_vector(3 downto 0); 
        
        -- interface to asynchronous 16-bit-wide EXTERNAL SRAM
        sram_address    : out std_logic_vector(SRAM_ADDR_SIZE-1 downto 1);
        sram_databus    : inout std_logic_vector(15 downto 0);
        sram_byte_we_n  : out std_logic_vector(1 downto 0);
        sram_oe_n       : out std_logic
    );
end entity mips_cache_stub;



architecture stub of mips_cache_stub is

type t_cache_state is (
    idle,
    
    read_bram_data_0,
    read_bram_data_1,
    
    read_data_0,
    read_data_1,
    
    read_code_0,
    read_code_1,

    write_0,
    write_1
   );

signal ps, ns :             t_cache_state;


signal use_sram_wr :        std_logic;
signal use_sram_rd :        std_logic;
signal use_io_wr :          std_logic;
signal use_io_rd :          std_logic;
signal data_addr_reg :      std_logic_vector(SRAM_ADDR_SIZE-1 downto 2);
signal data_wr_reg :        std_logic_vector(31 downto 0);
signal data_input_reg :     std_logic_vector(15 downto 0);
signal bram_rd_data_reg :   std_logic_vector(31 downto 0);
signal byte_we_reg :        std_logic_vector(3 downto 0);

begin


cache_state_machine_reg:
process(clk)
begin
   if clk'event and clk='1' then
        if reset='1' then
            ps <= idle; --wait_idle;
        else
            ps <= ns;
        end if;
    end if;
end process cache_state_machine_reg;



cache_state_machine_transitions:
process(clk,ps)
begin
    case ps is
    when idle =>
        ns <= ps;
    
        if code_rd_vma='1' and use_sram_rd='1' then
            ns <= read_code_0;
        elsif data_rd_vma='1' and use_sram_rd='1' then
            ns <= read_data_0;
        elsif data_rd_vma='1' and use_sram_rd='0' then
            ns <= read_bram_data_0;
        elsif byte_we/="0000" and use_sram_wr='1' then
            ns <= write_0;
        else
            ns <= ps;
        end if;

    when read_bram_data_0 =>
        ns <= read_bram_data_1;

    when read_bram_data_1 =>
        ns <= idle;

    when read_code_0 =>
        ns <= read_code_1;
        
    when read_code_1 =>
        if data_rd_vma='1' and use_sram_rd='1' then
            ns <= read_data_0;
        elsif byte_we/="0000" and use_sram_wr='1' then
            ns <= write_0;
        else
            ns <= idle;
        end if;
    
    when read_data_0 =>
        ns <= read_data_1;
        
    when read_data_1 =>
        if byte_we/="0000" and use_sram_wr='1' then
            ns <= write_0;
        else
            ns <= idle;
        end if;

    when write_0 =>
        ns <= write_1;
        
    when write_1 =>
        ns <= idle;
        
    when others =>
        -- BUG: should raise some debug signal
        ns <= idle;
    end case;
end process cache_state_machine_transitions;

sram_address(sram_address'high downto 2) <= 
                                data_addr_reg(sram_address'high downto 2);


with ps select sram_address(1) <= 
    '0'     when read_data_0,
    '1'     when read_data_1,
    '0'     when read_code_0,
    '1'     when read_code_1,
    '0'     when write_0,
    '1'     when write_1,
    '0'     when others;

with ps select sram_oe_n <=
    '0'     when read_data_0,
    '0'     when read_data_1,
    '0'     when read_code_0,
    '0'     when read_code_1,
    '1'     when others;

with ps select sram_byte_we_n <=
    not byte_we_reg(3 downto 2)     when write_0,
    not byte_we_reg(1 downto 0)     when write_1,
    "11"                            when others;
    
with ps select sram_databus <= 
    data_wr_reg(31 downto 16)   when write_0,
    data_wr_reg(15 downto  0)   when write_1,
    (others => 'Z')             when others;

sdram_address_register:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            data_addr_reg <= (others => '0');
        else
            if data_rd_vma='1' then
                data_addr_reg <= data_rd_addr(sram_address'high downto 2);
            elsif byte_we/="0000" then
                data_addr_reg <= data_wr_addr(sram_address'high downto 2);
            end if;
        end if;
    end if;
end process sdram_address_register;


data_input_register:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            data_input_reg <= (others => '0');
        else
            if ps=read_data_0 then
                data_input_reg <= sram_databus;
            end if;
            bram_rd_data_reg <= bram_rd_data;
            if byte_we/="0000" then
                byte_we_reg <= byte_we;
                data_wr_reg <= data_wr;
            end if;
        end if;
    end if;
end process data_input_register;


with ps select code_rd <=
    data_input_reg & sram_databus   when read_code_1,
    bram_rd_data                    when others;


data_rd <= 
    data_input_reg & sram_databus when ps=read_data_1 else
    bram_rd_data_reg;
    -- FIXME IO RD data missing
    --io_rd_data      when (ps=idle and use_io_rd='1') else

mem_wait <= '1' when
    ps=read_bram_data_0 or
    ps=read_data_0 or
    ps=write_0 or
    (ps=idle and use_sram_wr='1' and byte_we/="0000")
    else '0';

use_sram_rd <= '1'
    when (addr_decode(data_rd_addr,ADDR_XRAM)='1' and data_rd_vma='1') or 
         (addr_decode(code_rd_addr,ADDR_XRAM)='1' and code_rd_vma='1')
    else '0';

use_sram_wr <= '1'
    when addr_decode(data_wr_addr,ADDR_XRAM)='1'
    else '0';

use_io_rd <= '1'
    when addr_decode(data_rd_addr,ADDR_IO)='1' and data_rd_vma='1'
    else '0';

use_io_wr <= '1'
    when addr_decode(data_wr_addr,ADDR_IO)='1' and byte_we/="0000"
    else '0';

--------------------------------------------------------------------------------

bram_rd_addr <= data_rd_addr(bram_rd_addr'high downto 2) when
    (ps=idle and use_sram_rd='0' and data_rd_vma='1')
    else code_rd_addr(bram_rd_addr'high downto 2);
    
bram_wr_addr <= data_wr_addr(bram_wr_addr'high downto 2);
bram_byte_we <= byte_we when addr_decode(data_wr_addr,ADDR_BOOT)='1' else "0000";


io_wr_addr <= data_wr_addr;
io_rd_addr <= data_rd_addr(31  downto 2);
io_wr_data <= data_wr;
io_byte_we <= byte_we when addr_decode(data_wr_addr,ADDR_IO)='1' else "0000";


end architecture stub;
