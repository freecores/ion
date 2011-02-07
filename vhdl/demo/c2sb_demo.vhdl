--##############################################################################
-- ION MIPS-compatible CPU demo on Terasic DE-1 Cyclone-II starter board
--##############################################################################
-- This module is little more than a wrapper around the CPU and its memories.
--##############################################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- FPGA i/o for Terasic DE-1 board
-- (Many of the board's i/o devices will go unused in this demo)
entity c2sb_demo is
    port (
        -- ***** Clocks
        clk_50MHz     : in std_logic;

        -- ***** Flash 4MB
        flash_addr    : out std_logic_vector(21 downto 0);
        flash_data    : in std_logic_vector(7 downto 0);
        flash_oe_n    : out std_logic;
        flash_we_n    : out std_logic;
        flash_reset_n : out std_logic;

        -- ***** SRAM 256K x 16
        sram_addr     : out std_logic_vector(17 downto 0);
        sram_data     : inout std_logic_vector(15 downto 0);
        sram_oe_n     : out std_logic;
        sram_ub_n     : out std_logic;
        sram_lb_n     : out std_logic;
        sram_ce_n     : out std_logic;
        sram_we_n     : out std_logic;

        -- ***** RS-232
        rxd           : in std_logic;
        txd           : out std_logic;

        -- ***** Switches and buttons
        switches      : in std_logic_vector(9 downto 0);
        buttons       : in std_logic_vector(3 downto 0);

        -- ***** Quad 7-seg displays
        hex0          : out std_logic_vector(0 to 6);
        hex1          : out std_logic_vector(0 to 6);
        hex2          : out std_logic_vector(0 to 6);
        hex3          : out std_logic_vector(0 to 6);

        -- ***** Leds
        red_leds      : out std_logic_vector(9 downto 0);
        green_leds    : out std_logic_vector(7 downto 0);

        -- ***** SD Card
        sd_data       : in  std_logic;
        sd_cs         : out std_logic;
        sd_cmd        : out std_logic;
        sd_clk        : out std_logic
    );
end c2sb_demo;

architecture minimal of c2sb_demo is


--##############################################################################
--

constant SRAM_ADDR_SIZE : integer := 18;

--##############################################################################
-- RS232 interface signals

signal rx_rdy :             std_logic;
signal tx_rdy :             std_logic;
signal rs232_data_rx :      std_logic_vector(7 downto 0);
signal rs232_status :       std_logic_vector(7 downto 0);
signal data_io_out :        std_logic_vector(7 downto 0);
signal io_port :            std_logic_vector(7 downto 0);
signal read_rx :            std_logic;
signal write_tx :           std_logic;


--##############################################################################
--


-- CPU access to hex display
signal reg_display :        std_logic_vector(15 downto 0);



--##############################################################################
-- DE-1 board interface signals

-- Synchronization FF chain for asynchronous reset input
signal reset_sync :         std_logic_vector(2 downto 0);

-- Quad 7-segment display (non multiplexed) & LEDS
signal display_data :       std_logic_vector(15 downto 0);
signal reg_gleds :          std_logic_vector(7 downto 0);

-- Clock & reset signals
signal clk_1hz :            std_logic;
signal clk_master :         std_logic;
signal counter_1hz :        std_logic_vector(25 downto 0);
signal reset :              std_logic;
signal clk :                std_logic;

-- SD control signals
signal sd_in :              std_logic;
signal reg_sd_dout :        std_logic;
signal reg_sd_clk :         std_logic;
signal reg_sd_cs :          std_logic;

-- MPU interface signals
signal data_uart :          std_logic_vector(31 downto 0);
signal data_uart_status :   std_logic_vector(31 downto 0);
signal uart_tx_rdy :        std_logic := '1';
signal uart_rx_rdy :        std_logic := '1';

signal io_rd_data :         std_logic_vector(31 downto 0);
signal io_rd_addr :         std_logic_vector(31 downto 2);
signal io_wr_addr :         std_logic_vector(31 downto 2);
signal io_wr_data :         std_logic_vector(31 downto 0);
signal io_rd_vma :          std_logic;
signal io_byte_we :         std_logic_vector(3 downto 0);

signal mpu_sram_address :   std_logic_vector(SRAM_ADDR_SIZE downto 1);
signal mpu_sram_databus :   std_logic_vector(15 downto 0);
signal mpu_sram_byte_we_n : std_logic_vector(1 downto 0);
signal mpu_sram_oe_n :      std_logic;

-- Converts hex nibble to 7-segment
-- Segments ordered as "GFEDCBA"; '0' is ON, '1' is OFF
function nibble_to_7seg(nibble : std_logic_vector(3 downto 0))
                        return std_logic_vector is
begin
    case nibble is
    when X"0"       => return "0000001";
    when X"1"       => return "1001111";
    when X"2"       => return "0010010";
    when X"3"       => return "0000110";
    when X"4"       => return "1001100";
    when X"5"       => return "0100100";
    when X"6"       => return "0100000";
    when X"7"       => return "0001111";
    when X"8"       => return "0000000";
    when X"9"       => return "0000100";
    when X"a"       => return "0001000";
    when X"b"       => return "1100000";
    when X"c"       => return "0110001";
    when X"d"       => return "1000010";
    when X"e"       => return "0110000";
    when X"f"       => return "0111000";
    when others     => return "0111111"; -- can't happen
    end case;
end function nibble_to_7seg;


begin

    mpu: entity work.mips_mpu
    generic map (
        SRAM_ADDR_SIZE => SRAM_ADDR_SIZE
    )
    port map (
        interrupt   => '0',

        -- interface to FPGA i/o devices
        io_rd_data  => io_rd_data,
        io_rd_addr  => io_rd_addr,
        io_wr_addr  => io_wr_addr,
        io_wr_data  => io_wr_data,
        io_rd_vma   => io_rd_vma,
        io_byte_we  => io_byte_we,

        -- interface to asynchronous 16-bit-wide EXTERNAL SRAM
        sram_address    => mpu_sram_address,
        sram_databus    => sram_data,
        sram_byte_we_n  => mpu_sram_byte_we_n,
        sram_oe_n       => mpu_sram_oe_n,


        uart_rxd    => rxd,
        uart_txd    => txd,

        clk         => clk,
        reset       => reset
    );


reg_display <= io_wr_data(15 downto 0);
reg_gleds <= io_rd_vma & "000" & io_byte_we;

-- red leds (light with '1') -- some CPU control signals
red_leds(0) <= '0';
red_leds(1) <= '0';
red_leds(2) <= '0';
red_leds(3) <= '0';
red_leds(4) <= '0';
red_leds(5) <= '0';
red_leds(6) <= '0';
red_leds(7) <= '0';
red_leds(8) <= '0';
red_leds(9) <= clk_1hz;


--##############################################################################
-- terasIC Cyclone II STARTER KIT BOARD -- interface to on-board devices
--##############################################################################

--##############################################################################
-- FLASH (flash is unused in this demo)
--##############################################################################

flash_addr <= (others => '0');
flash_we_n <= '1'; -- all enable signals inactive
flash_oe_n <= '1';
flash_reset_n <= '1';


--##############################################################################
-- SRAM (used as 64K x 8)
--
-- NOTE: All writes go to SRAM independent of rom paging status
--##############################################################################

sram_addr <= mpu_sram_address;
sram_oe_n <= mpu_sram_oe_n;
sram_ub_n <= mpu_sram_byte_we_n(1) and mpu_sram_oe_n;
sram_lb_n <= mpu_sram_byte_we_n(0) and mpu_sram_oe_n;
sram_ce_n <= '0';
sram_we_n <= mpu_sram_byte_we_n(1) and mpu_sram_byte_we_n(0);


--##############################################################################
-- RESET, CLOCK
--##############################################################################

-- Use button 3 as reset
reset_synchronization:
process(clk)
begin
    if clk'event and clk='1' then
        reset_sync(2) <= not buttons(3);
        reset_sync(1) <= reset_sync(2);
        reset_sync(0) <= reset_sync(1);
    end if;
end process reset_synchronization;

reset <= reset_sync(0);


-- Generate a 1-Hz 'clock' to flash a LED for visual reference.
process(clk_50MHz)
begin
  if clk_50MHz'event and clk_50MHz='1' then
    if reset = '1' then
      clk_1hz <= '0';
      counter_1hz <= (others => '0');
    else
      if conv_integer(counter_1hz) = 50000000 then
        counter_1hz <= (others => '0');
        clk_1hz <= not clk_1hz;
      else
        counter_1hz <= counter_1hz + 1;
      end if;
    end if;
  end if;
end process;

-- Master clock is external 50MHz oscillator
clk <= clk_50MHz;


--##############################################################################
-- LEDS, SWITCHES
--##############################################################################

-- Display the contents of a debug register at the green leds bar
green_leds <= reg_gleds;


--##############################################################################
-- QUAD 7-SEGMENT DISPLAYS
--##############################################################################

-- Show contents of debug register in hex display
display_data <= reg_display;


-- 7-segment encoders; the dev board displays are not multiplexed or encoded
hex3 <= nibble_to_7seg(display_data(15 downto 12));
hex2 <= nibble_to_7seg(display_data(11 downto  8));
hex1 <= nibble_to_7seg(display_data( 7 downto  4));
hex0 <= nibble_to_7seg(display_data( 3 downto  0));

--##############################################################################
-- SD card interface
--##############################################################################

-- unused in this demo
sd_cs     <= '0';
sd_cmd    <= '0';
sd_clk    <= '0';
sd_in     <= 'Z';


--##############################################################################
-- SERIAL
--##############################################################################

--  Embedded in the MPU entity

end minimal;
