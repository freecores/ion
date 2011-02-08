--##############################################################################
-- This file was generated automatically from '/src/mips_tb1_template.vhdl'.
--
-- WARNING: As of rev. 55 this module is obsolete and has been left behind
--------------------------------------------------------------------------------
-- Simulation test bench TB1 -- not synthesizable.
--
-- Simulates the CPU core connected to two memory blocks, a read-only block
-- initialized with code and a read-write block initialized with all data, 
-- including read-only data. The makefile for the source samples include targets
-- to build simulation test benches using this template -- those source samples
-- that support this template.
--
-- The memory setup is meant to test the 'bare' cpu, without cache. 
-- Address decoding is harcoded to that of Plasma system, for the time being.
-- 
-- Console output (at addresses compatible to Plasma's) is logged to text file
-- "hw_sim_console_log.txt".
-- IMPORTANT: The code that echoes UART TX data to the simulation console does
-- line buffering; it will not print anything until it gets a CR (0x0d), and
-- will ifnore LFs (0x0a). Bear this in mind if you see no output when you 
-- expect it.
--
-- WARNING: Will only work on Modelsim; uses custom library SignalSpy.
--##############################################################################

library ieee, modelsim_lib;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

use work.mips_pkg.all;

use modelsim_lib.util.all;
use std.textio.all;
use work.txt_util.all;


entity @entity_name@ is
end @entity_name@;

architecture @arch_name@ of @entity_name@ is

--------------------------------------------------------------------------------
-- Simulation parameters

-- Master clock period
constant T : time           := 20 ns;
-- Time the URAT is unavailable after writing to the TX register
-- WARNING: slite does not simulate this. The logs may be different when > 0.0!
constant SIMULATED_UART_TX_TIME : time := 0.0 us;
               
-- 2000 is enough for 'hello' sample, 22000 enough for 10 digits of pi
constant SIMULATION_LENGTH : integer := @sim_len@;

--------------------------------------------------------------------------------
-- UUT & interface signals

signal rd_addr :            std_logic_vector(31 downto 0);
signal prev_rd_addr :       std_logic_vector(31 downto 0);
signal vma_data :           std_logic;
signal vma_code :           std_logic;
signal wr_addr :            std_logic_vector(31 downto 2);
signal full_rd_addr :       std_logic_vector(31 downto 0);
signal full_wr_addr :       std_logic_vector(31 downto 0);
signal byte_we :            std_logic_vector(3 downto 0);
signal data_r :             std_logic_vector(31 downto 0);
signal data_ram :           std_logic_vector(31 downto 0);
signal data_uart :          std_logic_vector(31 downto 0);
signal data_uart_status :   std_logic_vector(31 downto 0);
signal uart_tx_rdy :        std_logic := '1';
signal uart_rx_rdy :        std_logic := '1';
signal data_w :             std_logic_vector(31 downto 0);
signal mem_wait :           std_logic := '0';
signal interrupt :          std_logic := '0';
signal code_addr :          std_logic_vector(31 downto 2);
signal full_code_addr :     std_logic_vector(31 downto 0);
signal code_r :             std_logic_vector(31 downto 0);

--------------------------------------------------------------------------------

signal clk :                std_logic := '0';
signal reset :              std_logic := '1';
signal done :               std_logic := '0';
signal test :               integer := 0;

--------------------------------------------------------------------------------
-- Logging signals

-- These are internal CPU signal mirrored using Modelsim's SignalSpy
signal rbank :              t_rbank;
signal pc, cp0_epc :        std_logic_vector(31 downto 2);
signal reg_hi, reg_lo :     t_word;
signal negate_reg_lo :      std_logic;
signal ld_upper_byte :      std_logic;
signal ld_upper_hword :     std_logic;

-- Log file
file l_file: TEXT open write_mode is "hw_sim_log.txt";

-- Console output log file
file con_file: TEXT open write_mode is "hw_sim_console_log.txt";

-- Maximum line size of for console output log. Lines longer than this will be
-- truncated.
constant CONSOLE_LOG_LINE_SIZE : integer := 1024*4;

-- Console log line buffer
signal con_line_buf :       string(1 to CONSOLE_LOG_LINE_SIZE);
signal con_line_ix :        integer := 1;



--------------------------------------------------------------------------------

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

signal code_addr_rd :        t_data_address;

-- rom0 is LSB, rom3 is MSB
signal rom3 : t_code_ram := (@code3@);
signal rom2 : t_code_ram := (@code2@);
signal rom1 : t_code_ram := (@code1@);
signal rom0 : t_code_ram := (@code0@);


begin

    uut: entity work.mips_cpu
    port map (
        interrupt   => interrupt,
        
        data_rd_addr=> rd_addr,
        data_rd_vma => vma_data,
        data_rd     => data_r,
        
        code_rd_addr=> code_addr,
        code_rd     => code_r,
        code_rd_vma => vma_code,
        
        data_wr_addr=> wr_addr,
        data_wr     => data_w,
        byte_we     => byte_we,

        mem_wait    => mem_wait,
        
        clk         => clk,
        reset       => reset
    );

    ---------------------------------------------------------------------------
    -- Master clock: free running clock used as main module clock
    run_master_clock:
    process(done, clk)
    begin
        if done = '0' then
            clk <= not clk after T/2;
        end if;
    end process run_master_clock;

    drive_uut:
    process
    variable l : line;
    begin
        wait for T*4;
        reset <= '0';
        
        wait for T*SIMULATION_LENGTH;
        
        -- Flush console output to log console file (in case the end of the
        -- simulation caugh an unterminated line in the buffer)
        if con_line_ix > 1 then
            write(l, con_line_buf(1 to con_line_ix));
            writeline(con_file, l);
        end if;
        
        print("TB1 finished");
        done <= '1';
        wait;
        
    end process drive_uut;
    
    mem_wait <= '0'; -- memory wait input not simulated in this test bench
    

    -- RAM vs. IO data read mux
    data_r <= data_ram when prev_rd_addr(31 downto 28)/=X"2" else data_uart;
    
    -- UART read registers; only status, and hardwired, for the time being
    data_uart <= data_uart_status;
    data_uart_status <= X"0000000" & "00" & uart_tx_rdy & uart_rx_rdy;


    -- 'full' read address, used for simulation display only
    full_rd_addr <= rd_addr;
    full_wr_addr <= wr_addr & "00";
    full_code_addr <= code_addr & "00";

    data_addr_rd <= full_rd_addr(DATA_ADDR_SIZE-1+2 downto 2);
    data_addr_wr <= full_wr_addr(DATA_ADDR_SIZE-1+2 downto 2);
    code_addr_rd <= full_code_addr(DATA_ADDR_SIZE-1+2 downto 2);

    write_process:
    process(clk)
    variable i : integer;
    variable uart_data : integer;
    variable s: string(1 to 100);
    variable si : integer := 1;
    begin
        if clk'event and clk='1' then
            if reset='1' then
                data_ram <= (others =>'0');
            else
                prev_rd_addr <= rd_addr;
                
                data_ram <= 
                  ram3(conv_integer(unsigned(data_addr_rd))) &
                  ram2(conv_integer(unsigned(data_addr_rd))) &
                  ram1(conv_integer(unsigned(data_addr_rd))) &
                  ram0(conv_integer(unsigned(data_addr_rd)));
                  
                code_r <= 
                  rom3(conv_integer(unsigned(code_addr_rd))) &
                  rom2(conv_integer(unsigned(code_addr_rd))) &
                  rom1(conv_integer(unsigned(code_addr_rd))) &
                  rom0(conv_integer(unsigned(code_addr_rd)));
            end if;
            
            if byte_we/="0000" then
                if full_wr_addr(31 downto 28)=X"2" then
                    -- Write to UART
                    
                    -- If we're simulating the UART TX time, pulse RDY low
                    if SIMULATED_UART_TX_TIME > 0 us then
                        uart_tx_rdy <= '0', '1' after SIMULATED_UART_TX_TIME;
                    end if;
                    
                    -- TX data may come from the high or low byte (opcodes.s
                    -- uses high byte, no_op.c uses low)
                    if byte_we(0)='1' then
                        uart_data := conv_integer(unsigned(data_w(7 downto 0)));
                    else
                        uart_data := conv_integer(unsigned(data_w(31 downto 24)));
                    end if;
                    
                    -- UART TX data goes to output after a bit of line-buffering
                    -- and editing
                    if uart_data = 10 then
                        -- CR received: print output string and clear it
                        print(con_file, con_line_buf(1 to con_line_ix));
                        con_line_ix <= 1;
                        for i in 1 to con_line_buf'high loop
                           con_line_buf(i) <= ' ';
                        end loop;
                    elsif uart_data = 13 then
                        -- ignore LF
                    else
                        -- append char to output string
                        if con_line_ix < con_line_buf'high then
                            con_line_buf(con_line_ix) <= character'val(uart_data);
                            con_line_ix <= con_line_ix + 1;
                        end if;
                    end if;
                else
                    -- Write to RAM
                    if byte_we(3)='1' then
                        ram3(conv_integer(unsigned(data_addr_wr))) <= data_w(31 downto 24);
                    end if;
                    if byte_we(2)='1' then
                        ram2(conv_integer(unsigned(data_addr_wr))) <= data_w(23 downto 16);
                    end if;
                    if byte_we(1)='1' then
                        ram1(conv_integer(unsigned(data_addr_wr))) <= data_w(15 downto  8);
                    end if;
                    if byte_we(0)='1' then
                        ram0(conv_integer(unsigned(data_addr_wr))) <= data_w( 7 downto  0);
                    end if;
                end if;
            end if;
        end if;
    end process write_process;
    
    signalspy_rbank:
    process
    begin
        init_signal_spy("/@entity_name@/uut/p1_rbank", "rbank", 0, -1);
        init_signal_spy("/@entity_name@/uut/p0_pc_reg", "pc", 0, -1);
        init_signal_spy("/@entity_name@/uut/mult_div/upper_reg", "reg_hi", 0, -1);
        init_signal_spy("/@entity_name@/uut/mult_div/lower_reg", "reg_lo", 0, -1);
        init_signal_spy("/@entity_name@/uut/mult_div/negate_reg", "negate_reg_lo", 0, -1);
        init_signal_spy("/@entity_name@/uut/mult_div/negate_reg", "negate_reg_lo", 0, -1);
        init_signal_spy("/@entity_name@/uut/cp0_epc", "cp0_epc", 0, -1);
        init_signal_spy("/@entity_name@/uut/p2_ld_upper_byte", "ld_upper_byte", 0, -1);
        init_signal_spy("/@entity_name@/uut/p2_ld_upper_byte", "ld_upper_hword", 0, -1);
        wait;
    end process signalspy_rbank;

    log_cpu_activity:
    process(clk)
    variable prev_rbank : t_rbank := (others => X"00000000");
    variable ri : std_logic_vector(7 downto 0);
    variable full_pc : t_word := (others => '0');
    variable prev_pc : t_word := (others => '0');
    variable prev_hi : t_word := (others => '0');
    variable prev_lo : t_word := (others => '0');
    variable prev_epc : std_logic_vector(31 downto 2) := (others => '0');
    variable wr_data : t_word := (others => '0');
    variable temp : t_word := (others => '0');
    variable size : std_logic_vector(7 downto 0) := X"00";
    variable prev_vma_data : std_logic := '0';
    variable prev_rd_addr : t_word := (others => '0');
    variable rd_size : std_logic_vector(7 downto 0) := X"00";
    begin
        -- we'll be sampling control & data signals at falling edge, when 
        -- they're stable
        if clk'event and clk='0' then
            if reset='0' then
            
                -- log memory loads (data only)
                -- IMPORTANT: memory reads should be logged first because we're
                -- logging them the cycle after they actually happen. If you put
                -- the log code after any other log, the order of the operations 
                -- will appear wrong in the log even though it is not.
                if prev_vma_data='1' then
                    if ld_upper_hword='1' then
                        rd_size := X"04";
                    elsif ld_upper_byte='1' then
                        rd_size := X"02";
                    else
                        rd_size := X"01";
                    end if;
                    print(l_file, "("& hstr(prev_pc) &") ["& hstr(prev_rd_addr) &"] <"&
                          "**"&
                          --hstr(rd_size)& 
                          ">="& hstr(data_r)& " RD");
                end if;
                prev_vma_data := vma_data;
                prev_rd_addr := full_rd_addr;
                
                -- log register changes
                ri := X"00";
                for i in 0 to 31 loop
                    if prev_rbank(i)/=rbank(i) then
                        print(l_file, "("& hstr(full_pc)& ") ["& hstr(ri)& "]="& hstr(rbank(i)));
                    end if;
                    ri := ri + 1;
                end loop;

                -- log aux register changes
                if prev_hi /= reg_hi and reg_hi(0)/='U' then
                    -- we're observing the value of reg_lo, but the mult core
                    -- will output the negated value in some cases. We
                    -- have to mimic that behavior.
                    if negate_reg_lo='1' then
                        -- negate reg_lo before displaying
                        prev_lo := not reg_lo;
                        prev_lo := prev_lo + 1;
                        print(l_file, "("& hstr(full_pc)& ") [LO]="& hstr(prev_lo));
                    else
                        print(l_file, "("& hstr(full_pc)& ") [LO]="& hstr(reg_lo));
                    end if;
                end if;
                if prev_lo /= reg_lo and reg_lo(0)/='U'  then
                    print(l_file, "("& hstr(full_pc)& ") [LO]="& hstr(reg_lo));
                end if;
                if prev_epc /= cp0_epc and cp0_epc(31)/='U'  then
                    temp := cp0_epc & "00";
                    print(l_file, "("& hstr(full_pc)& ") [EP]="& hstr(temp));
                end if;

                full_pc := pc & "00";
                prev_pc := full_pc;
                prev_rbank := rbank;
                prev_hi := reg_hi;
                prev_lo := reg_lo;
                prev_epc := cp0_epc;
                
                -- log memory writes
                if byte_we/="0000" then
                    wr_data := X"00000000";
                    if byte_we(3)='1' then
                        wr_data(31 downto 24) := data_w(31 downto 24);
                    end if;
                    if byte_we(2)='1' then
                        wr_data(23 downto 16) := data_w(23 downto 16);
                    end if;
                    if byte_we(1)='1' then
                        wr_data(15 downto  8) := data_w(15 downto  8);
                    end if;
                    if byte_we(0)='1' then
                        wr_data( 7 downto  0) := data_w( 7 downto  0);
                    end if;
                    size := "0000" & byte_we; -- mask, really
                    print(l_file, "("& hstr(full_pc) &") ["& hstr(full_wr_addr) &"] |"& hstr(size)& "|="& hstr(wr_data)& " WR" );
                end if;

            end if;
        end if;
    end process log_cpu_activity;

end @arch_name@;
