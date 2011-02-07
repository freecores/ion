--------------------------------------------------------------------------------
-- This file was generated automatically from '/src/mips_mpu2_template.vhdl'.
--------------------------------------------------------------------------------
-- Synthesizable MPU -- CPU + cache + bootstrap BRAM + UART
--
-- This module uses the 'stub' version of the cache: a cache which actually is 
-- only an interface between the cpu and external static memory. This is useful 
-- to test external memory interface and cache-cpu interface without the cache
-- functionality getting in the way.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use work.mips_pkg.all;

entity mips_mpu is
    generic (
        SRAM_ADDR_SIZE : integer := 17
    );
    port(
        clk             : in std_logic;
        reset           : in std_logic;
        interrupt       : in std_logic;
        
        -- interface to FPGA i/o devices
        io_rd_data      : in std_logic_vector(31 downto 0);
        io_rd_addr      : out std_logic_vector(31 downto 2);
        io_wr_addr      : out std_logic_vector(31 downto 2);
        io_wr_data      : out std_logic_vector(31 downto 0);
        io_rd_vma       : out std_logic;
        io_byte_we      : out std_logic_vector(3 downto 0);
        
        -- interface to asynchronous 16-bit-wide EXTERNAL SRAM
        sram_address    : out std_logic_vector(SRAM_ADDR_SIZE downto 1);
        sram_databus    : inout std_logic_vector(15 downto 0);
        sram_byte_we_n  : out std_logic_vector(1 downto 0);
        sram_oe_n       : out std_logic;

        -- UART 
        uart_rxd        : in std_logic;
        uart_txd        : out std_logic
    );
end; --entity mips_mpu

architecture rtl of mips_mpu is

-- interface cpu-cache
signal cpu_data_rd_addr :   t_word;
signal cpu_data_rd_vma :    std_logic;
signal cpu_data_rd :        t_word;
signal cpu_code_rd_addr :   t_pc;
signal cpu_code_rd :        t_word;
signal cpu_code_rd_vma :    std_logic;
signal cpu_data_wr_addr :   t_pc;
signal cpu_data_wr :        t_word;
signal cpu_byte_we :        std_logic_vector(3 downto 0);
signal cpu_mem_wait :       std_logic;

-- interface to i/o
signal mpu_io_rd_data :     std_logic_vector(31 downto 0);
signal mpu_io_wr_data :     std_logic_vector(31 downto 0);
signal mpu_io_rd_addr :     std_logic_vector(31 downto 2);
signal mpu_io_wr_addr :     std_logic_vector(31 downto 2);
signal mpu_io_rd_vma :      std_logic;
signal mpu_io_byte_we :     std_logic_vector(3 downto 0);

-- interface to UARTs
signal data_uart :          t_word;
signal data_uart_status :   t_word;
signal uart_tx_rdy :        std_logic := '1';
signal uart_rx_rdy :        std_logic := '1';
signal uart_write_tx :      std_logic;
signal uart_read_rx :       std_logic;


-- Block ram
constant BRAM_SIZE : integer := @code_table_size@;
constant BRAM_ADDR_SIZE : integer := log2(BRAM_SIZE);

--type t_bram is array(0 to BRAM_SIZE-1) of std_logic_vector(7 downto 0);
type t_bram is array(0 to (BRAM_SIZE)-1) of t_word;

-- bram0 is LSB, bram3 is MSB
--signal bram3 :              t_bram := (@ code3@);
--signal bram2 :              t_bram := (@ code2@);
--signal bram1 :              t_bram := (@ code1@);
--signal bram0 :              t_bram := (@ code0@);

signal bram :               t_bram := (@code-32bit@);

subtype t_bram_address is std_logic_vector(BRAM_ADDR_SIZE-1 downto 0);

signal bram_rd_addr :       t_bram_address; 
signal bram_wr_addr :       t_bram_address;
signal bram_rd_data :       t_word;
signal bram_wr_data :       t_word;
signal bram_byte_we :       std_logic_vector(3 downto 0);


--------------------------------------------------------------------------------
begin

cpu: entity work.mips_cpu
    port map (
        interrupt   => '0',
        
        data_rd_addr=> cpu_data_rd_addr,
        data_rd_vma => cpu_data_rd_vma,
        data_rd     => cpu_data_rd,
        
        code_rd_addr=> cpu_code_rd_addr,
        code_rd     => cpu_code_rd,
        code_rd_vma => cpu_code_rd_vma,
        
        data_wr_addr=> cpu_data_wr_addr,
        data_wr     => cpu_data_wr,
        byte_we     => cpu_byte_we,
    
        mem_wait    => cpu_mem_wait,
        
        clk         => clk,
        reset       => reset
    );

cache: entity work.mips_cache_stub
    generic map (
        BRAM_ADDR_SIZE => BRAM_ADDR_SIZE,
        SRAM_ADDR_SIZE => SRAM_ADDR_SIZE
    )
    port map (
        clk             => clk,
        reset           => reset,
        
        -- Interface to CPU core
        data_rd_addr    => cpu_data_rd_addr,
        data_rd         => cpu_data_rd,
        data_rd_vma     => cpu_data_rd_vma,
                        
        code_rd_addr    => cpu_code_rd_addr,
        code_rd         => cpu_code_rd,
        code_rd_vma     => cpu_code_rd_vma,
                        
        data_wr_addr    => cpu_data_wr_addr,
        byte_we         => cpu_byte_we,
        data_wr         => cpu_data_wr,
                        
        mem_wait        => cpu_mem_wait,
        cache_enable    => '1',
        
        -- interface to FPGA i/o devices
        io_rd_data      => mpu_io_rd_data,
        io_wr_data      => mpu_io_wr_data,
        io_rd_addr      => mpu_io_rd_addr,
        io_wr_addr      => mpu_io_wr_addr,
        io_rd_vma       => mpu_io_rd_vma,
        io_byte_we      => mpu_io_byte_we,
    
        -- interface to synchronous 32-bit-wide FPGA BRAM
        bram_rd_data    => bram_rd_data,
        bram_wr_data    => bram_wr_data,
        bram_rd_addr    => bram_rd_addr,
        bram_wr_addr    => bram_wr_addr,
        bram_byte_we    => bram_byte_we,
        
        -- interface to asynchronous 16-bit-wide external SRAM
        sram_address    => sram_address,
        sram_databus    => sram_databus,
        sram_byte_we_n  => sram_byte_we_n,
        sram_oe_n       => sram_oe_n
    );


--------------------------------------------------------------------------------
-- BRAM interface 

fpga_ram_block:
process(clk)
begin
    if clk'event and clk='1' then
            
        --bram_rd_data <= 
        --    bram3(conv_integer(unsigned(bram_rd_addr))) &
        --    bram2(conv_integer(unsigned(bram_rd_addr))) &
        --    bram1(conv_integer(unsigned(bram_rd_addr))) &
        --    bram0(conv_integer(unsigned(bram_rd_addr)));
        bram_rd_data <= bram(conv_integer(unsigned(bram_rd_addr)));
        
    end if;
end process fpga_ram_block;


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

serial_rx : entity work.rs232_rx 
    port map(
        rxd =>      uart_rxd,
        data_rx =>  OPEN, --rs232_data_rx,
        rx_rdy =>   uart_rx_rdy,
        read_rx =>  '1', --read_rx,
        clk =>      clk,
        reset =>    reset
    );


uart_write_tx <= '1' 
    when mpu_io_byte_we/="0000" and mpu_io_wr_addr(31 downto 28)=X"2" 
    else '0';

serial_tx : entity work.rs232_tx 
    port map(
        clk =>      clk,
        reset =>    reset,
        rdy =>      uart_tx_rdy,
        load =>     uart_write_tx,
        data_i =>   mpu_io_wr_data(7 downto 0),
        txd =>      uart_txd
    );

-- UART read registers; only status, and hardwired, for the time being
data_uart <= data_uart_status; -- FIXME no data rx yet
data_uart_status <= X"0000000" & "00" & uart_tx_rdy & uart_rx_rdy;

mpu_io_rd_data <= data_uart;

-- io_rd_data 
io_rd_addr <= mpu_io_rd_addr;
io_wr_addr <= mpu_io_wr_addr;
io_wr_data <= mpu_io_wr_data;
io_rd_vma <= mpu_io_rd_vma;
io_byte_we <= mpu_io_byte_we;


end architecture rtl;
