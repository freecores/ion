--------------------------------------------------------------------------------
-- DEPRECATED -- Do not use, see new makefiles.
-- Should use mpu1_template instead. This file is to be removed.
--------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use work.mips_pkg.all;

entity mips_mpu is
    generic (
        BRAM_ADDR_SIZE : integer := 10;
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

-- Data RAM table and interface signals ----------------------------------------
constant DATA_RAM_SIZE : integer := @data_table_size@;
constant DATA_ADDR_SIZE : integer := @data_addr_size@;
subtype t_data_address is std_logic_vector(DATA_ADDR_SIZE-1 downto 0);
-- (this table holds one byte-slice; the RAM will have 4 of these)
type t_data_ram is array(0 to DATA_RAM_SIZE-1) of std_logic_vector(7 downto 0);

signal data_addr_rd :       t_data_address; 
signal data_addr_wr :       t_data_address;

-- ram0 is LSB, ram3 is MSB
signal ram3 : t_data_ram := (@data3@);
signal ram2 : t_data_ram := (@data2@);
signal ram1 : t_data_ram := (@data1@);
signal ram0 : t_data_ram := (@data0@);


-- Code RAM table and interface signals ----------------------------------------
constant CODE_RAM_SIZE : integer := @code_table_size@;
constant CODE_ADDR_SIZE : integer := @code_addr_size@;
subtype t_code_address is std_logic_vector(CODE_ADDR_SIZE-1 downto 0);
-- (this table holds one byte-slice; the RAM will have 4 of these)
type t_code_ram is array(0 to CODE_RAM_SIZE-1) of std_logic_vector(7 downto 0);

signal code_addr_rd :        t_code_address;

-- ram0 is LSB, ram3 is MSB
signal rom3 : t_code_ram := (@code3@);
signal rom2 : t_code_ram := (@code2@);
signal rom1 : t_code_ram := (@code1@);
signal rom0 : t_code_ram := (@code0@);

--------------------------------------------------------------------------------

signal reset_sync :         std_logic_vector(2 downto 0);
signal cpu_rd_addr :        std_logic_vector(31 downto 0);
signal prev_rd_addr :       std_logic_vector(31 downto 28);
signal cpu_vma_data :       std_logic;
signal cpu_vma_code :       std_logic;
signal cpu_wr_addr :        std_logic_vector(31 downto 2);
signal cpu_byte_we :        std_logic_vector(3 downto 0);
signal cpu_data_r :         std_logic_vector(31 downto 0);
signal data_ram :           std_logic_vector(31 downto 0);
signal data_uart :          std_logic_vector(31 downto 0);
signal data_uart_status :   std_logic_vector(31 downto 0);
signal uart_tx_rdy :        std_logic;
signal uart_rx_rdy :        std_logic;
signal uart_write_tx :      std_logic;
signal uart_read_rx :       std_logic;
signal cpu_data_w :         std_logic_vector(31 downto 0);
signal cpu_code_addr :      std_logic_vector(31 downto 2);
signal cpu_code_r :         std_logic_vector(31 downto 0);


begin

    cpu: entity work.mips_cpu
    port map (
        interrupt   => '0',
        
        data_rd_addr=> cpu_rd_addr,
        data_rd_vma => cpu_vma_data,
        data_rd     => cpu_data_r,
        
        code_rd_addr=> cpu_code_addr,
        code_rd     => cpu_code_r,
        code_rd_vma => cpu_vma_code,
        
        data_wr_addr=> cpu_wr_addr,
        data_wr     => cpu_data_w,
        byte_we     => cpu_byte_we,

        mem_wait    => '0',
        
        clk         => clk,
        reset       => reset_sync(0)
    );


    -- RAM vs. IO data read mux
    cpu_data_r <= data_ram when prev_rd_addr/=X"2" else data_uart;
    

    -- Take the slices of the addr buses that will reach the ram blocks
    data_addr_rd <= cpu_rd_addr(DATA_ADDR_SIZE-1+2 downto 2);
    data_addr_wr <= cpu_wr_addr(DATA_ADDR_SIZE-1+2 downto 2);
    code_addr_rd <= cpu_code_addr(CODE_ADDR_SIZE-1+2 downto 2);


    data_ram_block:
    process(clk)
    begin
        if clk'event and clk='1' then
            prev_rd_addr <= cpu_rd_addr(31 downto 28);
                
            data_ram <= 
                ram3(conv_integer(unsigned(data_addr_rd))) &
                ram2(conv_integer(unsigned(data_addr_rd))) &
                ram1(conv_integer(unsigned(data_addr_rd))) &
                ram0(conv_integer(unsigned(data_addr_rd)));
            
            if cpu_byte_we/="0000" and cpu_wr_addr(31 downto 28)/=X"2" then
                -- Write to RAM
                if cpu_byte_we(3)='1' then
                    ram3(conv_integer(unsigned(data_addr_wr))) <= cpu_data_w(31 downto 24);
                end if;
                if cpu_byte_we(2)='1' then
                    ram2(conv_integer(unsigned(data_addr_wr))) <= cpu_data_w(23 downto 16);
                end if;
                if cpu_byte_we(1)='1' then
                    ram1(conv_integer(unsigned(data_addr_wr))) <= cpu_data_w(15 downto  8);
                end if;
                if cpu_byte_we(0)='1' then
                    ram0(conv_integer(unsigned(data_addr_wr))) <= cpu_data_w( 7 downto  0);
                end if;
            end if;
        end if;
    end process data_ram_block;

    code_ram_block:
    process(clk)
    begin
        if clk'event and clk='1' then
            cpu_code_r <= 
                rom3(conv_integer(unsigned(code_addr_rd))) &
                rom2(conv_integer(unsigned(code_addr_rd))) &
                rom1(conv_integer(unsigned(code_addr_rd))) &
                rom0(conv_integer(unsigned(code_addr_rd)));
        end if;
    end process code_ram_block;

    reset_synchronization:
    process(clk)
    begin
        if clk'event and clk='1' then
            reset_sync(2) <= reset;
            reset_sync(1) <= reset_sync(2);
            reset_sync(0) <= reset_sync(1);
        end if;
    end process reset_synchronization;

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

serial_rx : entity work.rs232_rx port map(
    rxd =>      uart_rxd,
    data_rx =>  OPEN, --rs232_data_rx,
    rx_rdy =>   uart_rx_rdy,
    read_rx =>  '1', --read_rx,
    clk =>      clk,
    reset =>    reset_sync(0)
);


uart_write_tx <= '1' when cpu_byte_we/="0000" and cpu_wr_addr(31 downto 28)=X"2" 
                 else '0';

serial_tx : entity work.rs232_tx port map(
    clk =>      clk,
    reset =>    reset_sync(0),
    rdy =>      uart_tx_rdy,
    load =>     uart_write_tx,
    data_i =>   cpu_data_w(7 downto 0),
    txd =>      uart_txd
);

-- UART read registers; only status, and hardwired, for the time being
data_uart <= data_uart_status; -- FIXME no data rx yet
data_uart_status <= X"0000000" & "00" & uart_tx_rdy & uart_rx_rdy;


end architecture rtl;
