--##############################################################################
-- Simulation test bench -- not synthesizable.
--
-- Simulates the MCU core connected to a simulated external static RAM on a 
-- 16-bit bus, plus an optional 8-bit static ROM. This setup is more or less 
-- that of develoment board DE-1 from Terasic.
--------------------------------------------------------------------------------
-- Console logging:
--
-- Console output (at addresses compatible to Plasma's) is logged to text file
-- "hw_sim_console_log.txt".
--
-- IMPORTANT: The code that echoes UART TX data to the simulation console does
-- line buffering; it will not print anything until it gets a CR (0x0d), and
-- will ifnore LFs (0x0a). Bear this in mind if you see no output when you 
-- expect it.
--
-- Console logging is done by monitoring CPU writes to the UART, NOT by looking
-- at the TxD pin. It will NOT catch baud-related problems, etc.
--------------------------------------------------------------------------------
-- WARNING: Will only work on Modelsim; uses custom library SignalSpy.
--##############################################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use std.textio.all;

use work.mips_pkg.all;
use work.mips_tb_pkg.all;
use work.sim_params_pkg.all;
use work.txt_util.all;

entity mips_tb is
end;


architecture testbench of mips_tb is

-- NOTE: simulation parameters are defined in sim_params_pkg


-- External SRAM and interface signals -----------------------------------------

signal sram1 : t_sram := ( others => X"00");
signal sram0 : t_sram := ( others => X"00");

signal sram_chip_addr :     std_logic_vector(SRAM_ADDR_SIZE downto 1);
signal sram_output :        std_logic_vector(15 downto 0);


-- PROM table and interface signals --------------------------------------------

-- We'll simulate a 16-bit-wide static PROM (e.g. a Flash) with some serious
-- cycle time (70 or 90 ns).

signal prom_rd_addr :       t_prom_address; 
signal prom_output :        std_logic_vector(7 downto 0);
signal prom_oe_n :          std_logic;

signal prom : t_prom := ( PROM_DATA );



-- I/O devices -----------------------------------------------------------------

signal data_uart :          std_logic_vector(31 downto 0);
signal data_uart_status :   std_logic_vector(31 downto 0);
signal uart_tx_rdy :        std_logic := '1';
signal uart_rx_rdy :        std_logic := '1';

--------------------------------------------------------------------------------

signal clk :                std_logic := '0';
signal reset :              std_logic := '1';
signal interrupt :          std_logic := '0';
signal done :               std_logic := '0';

-- interface to asynchronous 16-bit-wide external SRAM
signal mpu_sram_address :   t_word;
signal mpu_sram_data_rd :   std_logic_vector(15 downto 0);
signal mpu_sram_data_wr :   std_logic_vector(15 downto 0);
signal mpu_sram_byte_we_n : std_logic_vector(1 downto 0);
signal mpu_sram_oe_n :      std_logic;

-- interface to i/o
signal io_rd_data :         std_logic_vector(31 downto 0);
signal io_wr_data :         std_logic_vector(31 downto 0);
signal io_rd_addr :         std_logic_vector(31 downto 2);
signal io_wr_addr :         std_logic_vector(31 downto 2);
signal io_rd_vma :          std_logic;
signal io_byte_we :         std_logic_vector(3 downto 0);

signal rxd :                std_logic;
signal txd :                std_logic;


--------------------------------------------------------------------------------
-- Logging signals


-- Log file
file log_file: TEXT open write_mode is "hw_sim_log.txt";

-- Console output log file
file con_file: TEXT open write_mode is "hw_sim_console_log.txt";

-- All the info needed by the logger is here
signal log_info :           t_log_info;

-- Debug signals ---------------------------------------------------------------



begin

    -- UUT instantiation -------------------------------------------------------
    mpu: entity work.mips_mpu
    generic map (
        CLOCK_FREQ     => 50000000,
        SRAM_ADDR_SIZE => 32
    )
    port map (
        interrupt       => '0',

        -- interface to FPGA i/o devices
        io_rd_data      => io_rd_data,
        io_rd_addr      => io_rd_addr,
        io_wr_addr      => io_wr_addr,
        io_wr_data      => io_wr_data,
        io_rd_vma       => io_rd_vma,
        io_byte_we      => io_byte_we,

        -- interface to asynchronous 16-bit-wide EXTERNAL SRAM
        sram_address    => mpu_sram_address,
        sram_data_rd    => mpu_sram_data_rd,
        sram_data_wr    => mpu_sram_data_wr,
        sram_byte_we_n  => mpu_sram_byte_we_n,
        sram_oe_n       => mpu_sram_oe_n,

        uart_rxd        => rxd,
        uart_txd        => txd,

        debug_info      => OPEN,
        
        clk             => clk,
        reset           => reset
    );


    -- Master clock: free running clock used as main module clock --------------
    run_master_clock:
    process(done, clk)
    begin
        if done = '0' then
            clk <= not clk after T/2;
        end if;
    end process run_master_clock;

    -- Main simulation process: reset MCU and wait for fixed period ------------
    drive_uut:
    process
    variable l : line;
    begin
        wait for T*4;
        reset <= '0';
        
        wait for T*SIMULATION_LENGTH;

        -- Flush console output to log console file (in case the end of the
        -- simulation caugh an unterminated line in the buffer)
        if log_info.con_line_ix > 1 then
            write(l, log_info.con_line_buf(1 to log_info.con_line_ix));
            writeline(con_file, l);
        end if;

        print("TB finished");
        done <= '1';
        wait;
        
    end process drive_uut;



    -- SRAM/FLASH mux (on a real board this would be a simple address decoder)
    mpu_sram_data_rd <= 
        X"00" & prom_output when mpu_sram_address(31 downto 27)="10110" else
        sram_output;
            

    -- Do a very basic simulation of an external SRAM --------------------------

    sram_chip_addr <= mpu_sram_address(SRAM_ADDR_SIZE downto 1);

    -- FIXME should add some verification of /WE 
    sram_output <=
        sram1(conv_integer(unsigned(sram_chip_addr))) &
        sram0(conv_integer(unsigned(sram_chip_addr)))   when mpu_sram_oe_n='0'
        else (others => 'Z');

    simulated_sram_write:
    process(mpu_sram_byte_we_n, mpu_sram_address, mpu_sram_oe_n)
    begin
        -- Write cycle
        -- FIXME should add OE\ to write control logic
        if mpu_sram_byte_we_n'event or mpu_sram_address'event then
            if mpu_sram_byte_we_n(1)='0' then
                sram1(conv_integer(unsigned(sram_chip_addr))) <= mpu_sram_data_wr(15 downto  8);
            end if;
            if mpu_sram_byte_we_n(0)='0' then
                sram0(conv_integer(unsigned(sram_chip_addr))) <= mpu_sram_data_wr( 7 downto  0);
            end if;            
        end if;
    end process simulated_sram_write;


    -- Do a very basic simulation of an external PROM (FLASH) ------------------
    -- (wired to the same bus as the sram and both are static).
    
    prom_rd_addr <= mpu_sram_address(PROM_ADDR_SIZE+1 downto 2);
    
    prom_oe_n <= mpu_sram_oe_n;
    
    prom_output <=
        prom(conv_integer(unsigned(prom_rd_addr)))(31 downto 24) when prom_oe_n='0' and mpu_sram_address(1 downto 0)="00" else
        prom(conv_integer(unsigned(prom_rd_addr)))(23 downto 16) when prom_oe_n='0' and mpu_sram_address(1 downto 0)="01" else
        prom(conv_integer(unsigned(prom_rd_addr)))(15 downto  8) when prom_oe_n='0' and mpu_sram_address(1 downto 0)="10" else
        prom(conv_integer(unsigned(prom_rd_addr)))( 7 downto  0) when prom_oe_n='0' and mpu_sram_address(1 downto 0)="11" else
        (others => 'Z');
    

    -- Simulate dummy I/O traffic external to the MCU --------------------------
    -- FIXME console logging missing! IO too!

    -- This is useless (the simulated UART will not be actually used)
    -- but at least prevents the simulator from optimizing the logic away.
    rxd <= txd;
    
    
    -- Logging process: launch logger function ---------------------------------
    log_execution:
    process
    begin
        log_cpu_activity(clk, reset, done, 
                         "mips_tb/mpu", "cpu",
                         log_info, "log_info", 
                         LOG_TRIGGER_ADDRESS, log_file, con_file);
        wait;
    end process log_execution;

    
end architecture testbench;
