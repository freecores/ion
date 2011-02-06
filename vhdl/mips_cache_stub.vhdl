--------------------------------------------------------------------------------
-- mips_cache_stub.vhdl -- 1-word cache module
--
-- This module has the same interface and logic as a real cache but the cache
-- memory is just 1 word for each of code and data.
--
-- It interfaces the CPU to the following:
--
--  1.- Internal 32-bit-wide BRAM for read only
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
-- down. The purpose of this module is just to test the SRAM interface and the
-- cache logic and timing.
--
--------------------------------------------------------------------------------
-- External FPGA signals
--
-- This module has signals meant to connect directly to FPGA pins: the SRAM
-- interface. They are either direct register outputs or at most with an 
-- intervening 2-mux, in order to minimize the Tco (clock-to-output).
--
-- The Tco of these signals has to be accounted for in the real SRAM interface.
-- For example, under Quartus-2 and with a Cyclone-2 grade -7 device, the
-- worst Tco for the SRAM data pins is below 5 ns, enough to use a 10ns SRAM
-- with a 20 ns clock cycle.
-- Anyway, you need to take care of this yourself.
--
--------------------------------------------------------------------------------
-- Interface to CPU
--
-- 1.- All signals coming from the CPU are registered.
-- 2.- All CPU inputs come directly from a register, or at most have a 2-mux in
--     between.
-- 
-- This means this block will not degrade the timing performance of the system, 
-- as long as its logic is shallower than the current bottleneck (the ALU).
--
--------------------------------------------------------------------------------
-- KNOWN TROUBLE:
-- 
-- Apart from the very rough looks of the code, there's a few known problems:
--
-- 1.- Access to unmapped areas wil crash the CPU
--      A couple states are missing in the state machine for handling accesses 
--      to unmapped areas. I haven't yet decided how to handle that (return 
--      zero, trigger trap, mirror another mapped area...)
-- 2.- Code refills from SRAM is unimplemented yet
--      To be done for sheer lack of time.
-- 3.- Address decoding is hardcoded in mips_pkg
--      It should be done here using module generics and not package constants.
-- 4.- Does not work as a real 1-word cache yet
--      That functionality is still missing, all accesses 'miss'. It should be
--      implemented, as a way to test the real cache logic on a small scale.
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
        cache_enable    : in std_logic;

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
        bram_data_rd_vma: out std_logic;

        -- interface to asynchronous 16-bit-wide EXTERNAL SRAM
        sram_address    : out std_logic_vector(SRAM_ADDR_SIZE downto 1);
        sram_databus    : inout std_logic_vector(15 downto 0);
        sram_byte_we_n  : out std_logic_vector(1 downto 0);
        sram_oe_n       : out std_logic
    );
end entity mips_cache_stub;



architecture stub of mips_cache_stub is

-- state machines: definition of states -----------------------------

type t_code_cache_state is (
    code_normal,                -- 
    code_wait_for_dcache,       -- wait for D-cache to stop using the buses

    code_refill_bram_0,         -- pc in bram_rd_addr
    code_refill_bram_1,         -- op in bram_rd
    code_refill_bram_2,         -- op in code_rd 

    code_refill_sram_0,         -- FIXME code refill from SRAM unimplemented
    code_refill_sram_1,
    code_refill_sram_2,

    code_bug                    -- caught an error in the state machine
   );

-- I-cache state machine state register & next state
signal cps, cns :           t_code_cache_state;


type t_data_cache_state is (
    data_normal,

    data_refill_sram_0,         -- rd addr in SRAM addr bus (low hword)
    data_refill_sram_1,         -- rd addr in SRAM addr bus (high hword)

    data_refill_bram_0,         -- rd addr in bram_rd_addr
    data_refill_bram_1,         -- rd data in bram_rd_data
    
    data_read_io_0,             -- rd addr on io_rd_addr, io_vma active
    data_read_io_1,             -- rd data on io_rd_data
    
    data_write_io_0,            -- wr addr & data in io_wr_*, io_byte_we active

    data_writethrough_sram_0,   -- wr addr & data in SRAM buses (low hword)
    data_writethrough_sram_1,   -- wr addr & data in SRAM buses (high hword)
    
    data_ignore_write,          -- hook for raising error flag FIXME untested

    data_bug                    -- caught an error in the state machine
   );


-- D-cache state machine state register & next state
signal dps, dns :           t_data_cache_state;

-- CPU interface registers ------------------------------------------
signal data_rd_addr_reg :   t_pc;
signal data_wr_addr_reg :   t_pc;
signal code_rd_addr_reg :   t_pc;

signal data_wr_reg :        std_logic_vector(31 downto 0);
signal byte_we_reg :        std_logic_vector(3 downto 0);

-- SRAM interface ---------------------------------------------------
-- Stores first (high) HW read from SRAM
signal sram_rd_data_reg :   std_logic_vector(31 downto 16);
-- Data read from SRAM, valid in refill_1
signal sram_rd_data :       t_word;



-- I-cache -- most of this is unimplemented -------------------------

subtype t_code_tag is std_logic_vector(23 downto 2);
signal code_cache_tag :     t_code_tag;
signal code_cache_tag_store : t_code_tag;
signal code_cache_store :   t_word;
-- code word read from cache
signal code_cache_rd :      t_word;
-- raised whel code_cache_rd is not valid due to a cache miss
signal code_miss :          std_logic;

-- '1' when the I-cache state machine stalls the pipeline (mem_wait)
signal code_wait :          std_logic;

-- D-cache -- most of this is unimplemented -------------------------
subtype t_data_tag is std_logic_vector(23 downto 2);
signal data_cache_tag :     t_data_tag;
signal data_cache_tag_store : t_data_tag;
signal data_cache_store :   t_word;
-- active when there's a write waiting to be done
signal write_pending :      std_logic;
-- active when there's a read waiting to be done
signal read_pending :       std_logic;
-- data word read from cache
signal data_cache_rd :      t_word;
-- '1' when data_cache_rd is not valid due to a cache miss
signal data_miss :          std_logic;

-- '1' when the D-cache state machine stalls the pipeline (mem_wait)
signal data_wait :          std_logic;


-- Address decoding -------------------------------------------------

-- Address slices used to decode
signal code_rd_addr_mask :  t_addr_decode;
signal data_rd_addr_mask :  t_addr_decode;
signal data_wr_addr_mask :  t_addr_decode;

-- Memory map area being accessed for each of the 3 buses:
-- 00 -> BRAM (read only)
-- 01 -> SRAM
-- 10 -> IO
-- 11 -> Unmapped
signal code_rd_area :       std_logic_vector(1 downto 0);
signal data_rd_area :       std_logic_vector(1 downto 0);
signal data_wr_area :       std_logic_vector(1 downto 0);



begin

--------------------------------------------------------------------------------
-- Cache control state machines 

cache_state_machine_regs:
process(clk)
begin
   if clk'event and clk='1' then
        if reset='1' then
            cps <= code_normal;
            dps <= data_normal;
        else
            cps <= cns;
            dps <= dns;
        end if;
    end if;
end process cache_state_machine_regs;

-- The code state machine occasionally 'waits' for the 
code_state_machine_transitions:
process(cps, dps, code_rd_vma, code_miss, code_rd_area, 
        write_pending, read_pending)
begin
    case cps is
    when code_normal =>
        if code_rd_vma='1' and code_miss='1' and 
           read_pending='0' and write_pending='0' then
            cns <= code_refill_bram_0; -- FIXME check memory area, SRAM!
        else
            cns <= cps;
        end if;

    when code_refill_bram_0 =>
        cns <= code_refill_bram_1;

    when code_refill_bram_1 =>
        cns <= code_refill_bram_2;

    when code_refill_bram_2 =>
        if dps/=data_normal and read_pending='0' and write_pending='0' then
            cns <= code_wait_for_dcache;
        else
            cns <= code_normal;
        end if;
        
    when code_wait_for_dcache =>
        -- if D-cache is busy, wait for it to become idle
        if dps/=data_normal then
            cns <= cps;
        elsif code_miss='1' then
            cns <= code_refill_bram_1; -- FIXME check memory area
        else
            cns <= code_normal;
        end if;

    when code_bug =>  
        -- Something weird happened, we have 1 cycle to do something like raise
        -- an error flag, etc. After 1 cycle, back to normal.
        cns <= code_normal;

    when others =>
        -- We should never arrive here. If we do we handle it in state code_bug.
        cns <= code_bug;
    end case;
end process code_state_machine_transitions;


-- This state machine does not overlap IO/BRAM/SRAM accesses for simplicity.

data_state_machine_transitions:
process(dps, write_pending, read_pending, data_rd_area, data_wr_area)
begin
    case dps is
    when data_normal =>
        if write_pending='1' then
            case data_wr_area is
            when "00"   => dns <= data_ignore_write; -- Write to BRAM ignored
            when "01"   => dns <= data_writethrough_sram_0;
            when "10"   => dns <= data_write_io_0;
            when others => dns <= dps; -- Write to undecoded area ignored
            end case;
            
        elsif read_pending='1' then
            case data_rd_area is
            when "00"   => dns <= data_refill_bram_0;
            when "01"   => dns <= data_refill_sram_0;
            when "10"   => dns <= data_read_io_0;
            when others => dns <= dps; -- ignore read from undecoded area
                           -- FIXME should raise debug flag 
            end case;
        else
            dns <= dps;
        end if;

    when data_write_io_0 =>
        dns <= data_normal;
   
    when data_read_io_0 =>
        dns <= data_read_io_1;
   
    when data_read_io_1 =>
        dns <= data_normal;

    when data_refill_sram_0 =>
        dns <= data_refill_sram_1;

    when data_refill_sram_1 =>
        dns <= data_normal;

    when data_refill_bram_0 =>
        dns <= data_refill_bram_1;

    when data_refill_bram_1 =>
        dns <= data_normal;

    when data_writethrough_sram_0 =>
        dns <= data_writethrough_sram_1;

    when data_writethrough_sram_1 =>
        dns <= data_normal;

    when data_ignore_write =>
        dns <= data_normal;

    when data_bug =>
        -- Something weird happened, we have 1 cycle to do something like raise
        -- an error flag, etc. After 1 cycle, back to normal.    
        dns <= data_normal;

    when others =>
        -- Should never arrive here. If we do, we handle it in state data_bug.
        dns <= data_bug;
    end case;
end process data_state_machine_transitions;


--------------------------------------------------------------------------------
-- CPU interface registers and address decoding --------------------------------


-- Everything coming and going to the CPU is registered, so that the CPU has
-- some timing marging.

cpu_data_interface_registers:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            write_pending <= '0';
            read_pending <= '0';
            byte_we_reg <= "0000";
        else
            -- Raise 'read_pending' at the 1st cycle of a read, clear it when
            -- the read (and/or refill) operation has been done.
            -- data_rd_addr_reg always has the addr of any pending read
            if data_rd_vma='1' then
                read_pending <= '1';
                data_rd_addr_reg <= data_rd_addr(31 downto 2);
            elsif dps=data_refill_sram_1 or 
                  dps=data_refill_bram_1 or 
                  dps=data_read_io_0 then
                read_pending <= '0';
            end if;

            -- Raise 'write_pending' at the 1st cycle of a read, clear it when
            -- the write (writethrough actually) operation has been done.
            -- data_wr_addr_reg always has the addr of any pending write
            if byte_we/="0000" and dps=data_normal then
                byte_we_reg <= byte_we;
                data_wr_reg <= data_wr;
                data_wr_addr_reg <= data_wr_addr;
                write_pending <= '1';
            elsif dps=data_writethrough_sram_1 or
                  dps=data_write_io_0 or
                  dps=data_ignore_write then
                write_pending <= '0';
                byte_we_reg <= "0000";
            end if;
            
        end if;
    end if;
end process cpu_data_interface_registers;

cpu_code_interface_registers:
process(clk)
begin
    if clk'event and clk='1' then
        -- Register code fetch addresses only when they are valid; so that
        -- code_rd_addr_reg always holds the last fetch address.
        if (cps=code_normal and code_rd_vma='1') or 
            cps=code_refill_bram_2 then -- FIXME explain this term
            code_rd_addr_reg <= code_rd_addr;
        end if;
    end if;
end process cpu_code_interface_registers;


-- Address decoding ------------------------------------------------------------

-- Decoding is done on the high bits of the address only, there'll be mirroring.
-- Write to areas not explicitly decoded will be silently ignored. Reads will
-- get undefined data.

code_rd_addr_mask <= code_rd_addr_reg(31 downto t_addr_decode'low);
data_rd_addr_mask <= data_rd_addr_reg(31 downto t_addr_decode'low);
data_wr_addr_mask <= data_wr_addr_reg(31 downto t_addr_decode'low);


with code_rd_addr_mask select code_rd_area <=
    "00"    when ADDR_BOOT,
    "01"    when ADDR_XRAM,
    "11"    when others;

with data_rd_addr_mask select data_rd_area <=
    "00"    when ADDR_BOOT,
    "01"    when ADDR_XRAM,
    "10"    when ADDR_IO,
    "11"    when others;

with data_wr_addr_mask select data_wr_area <=
    "01"    when ADDR_XRAM,
    "10"    when ADDR_IO,
    "11"    when others;

--------------------------------------------------------------------------------
-- BRAM interface


-- BRAMm address can come from code or data buses
-- (note both inputs to this mux are register outputs)
bram_rd_addr <= 
    data_rd_addr_reg(bram_rd_addr'high downto 2) when dps=data_refill_bram_0
    else code_rd_addr_reg(bram_rd_addr'high downto 2) ;

bram_data_rd_vma <= '1' when dps=data_refill_bram_1 else '0';



--------------------------------------------------------------------------------
-- Code cache 

-- All the tag match logic is unfinished and will be simplified away in synth.

-- CPU is wired directly to cache output, no muxes
code_rd <= code_cache_rd;

-- FIXME Actual 1-word cache functionality is unimplemented yet
code_miss <= '1'; -- always miss

-- Read cache code and tag from code store
code_cache_rd <= code_cache_store;
code_cache_tag <= code_cache_tag_store;

code_cache_memory:
process(clk)
begin
    if clk'event and clk='1' then


        if reset='1' then
            -- in the real hardware the tag store can't be reset and it's up
            -- to the SW to initialize the cache.
            code_cache_tag_store <= (others => '0');
            code_cache_store <= (others => '0');
        else
            -- Refill cache if necessary
            if cps=code_refill_bram_1 then
                code_cache_tag_store <=
                    "01" & code_rd_addr_reg(t_code_tag'high-2 downto t_code_tag'low);
                code_cache_store <= bram_rd_data;
            --elsif cps=code_refill_sram_2 then
            --    code_cache_tag_store <=
            --        "01" & code_rd_addr_reg(t_code_tag'high-2 downto t_code_tag'low);
            --    code_cache_store <= sram_rd_data;
            end if;
        end if;
    end if;
end process code_cache_memory;


--------------------------------------------------------------------------------
-- Data cache

-- CPU data input mux: direct cache output OR uncached io input
with dps select data_rd <= 
    io_rd_data      when data_read_io_1,    
    data_cache_rd   when others;

-- All the tag match logic is unfinished and will be simplified away in synth.
-- The 'cache' is really a single register.
data_cache_rd <= data_cache_store;
data_cache_tag <= data_cache_tag_store;

data_cache_memory:
process(clk)
begin
    if clk'event and clk='1' then
        if reset='1' then
            -- in the real hardware the tag store can't be reset and it's up
            -- to the SW to initialize the cache.
            data_cache_tag_store <= (others => '0');
            data_cache_store <= (others => '0');
        else
            -- Refill data cache if necessary
            if dps=data_refill_sram_1 then
                data_cache_tag_store <=
                    "01" & data_rd_addr_reg(t_data_tag'high-2 downto t_data_tag'low);
                data_cache_store <= sram_rd_data;
            elsif dps=data_refill_bram_1 then
                data_cache_tag_store <=
                    "01" & data_rd_addr_reg(t_data_tag'high-2 downto t_data_tag'low);
                data_cache_store <= bram_rd_data;            
            end if;
        end if;
    end if;
end process data_cache_memory;


--------------------------------------------------------------------------------
-- SRAM interface

-- Note this signals are meantto be connected directly to FPGA pins (and then
-- to a SRAM, of course). They are the only signals whose tco we care about.

-- FIXME should add a SRAM CE\ signal

-- SRAM address bus (except for LSB) comes from cpu code or data addr registers
with dps select sram_address(sram_address'high downto 2) <=
    data_rd_addr_reg(sram_address'high downto 2)    when data_refill_sram_0,
    data_rd_addr_reg(sram_address'high downto 2)    when data_refill_sram_1,
    data_wr_addr_reg(sram_address'high downto 2)    when others;

-- SRAM addr bus LSB depends on the D-cache state because we read/write the
-- halfwords sequentially in successive cycles.
with dps select sram_address(1) <=
    '0'     when data_writethrough_sram_0,
    '1'     when data_writethrough_sram_1,
    '0'     when data_refill_sram_0,
    '1'     when data_refill_sram_1,
    '0'     when others;

-- SRAM databus i(when used for output) comes from either hword of the data
-- write register.
with dps select sram_databus <=
    data_wr_reg(31 downto 16)   when data_writethrough_sram_0,
    data_wr_reg(15 downto  0)   when data_writethrough_sram_1,
    (others => 'Z')             when others;

-- The byte_we is split in two similarly.
with dps select sram_byte_we_n <=
    not byte_we_reg(3 downto 2) when data_writethrough_sram_0,
    not byte_we_reg(1 downto 0) when data_writethrough_sram_1,
    "11"                        when others;

-- SRAM OE\ is only asserted low for read cycles
with dps select sram_oe_n <=
    '0' when data_refill_sram_0,
    '0' when data_refill_sram_1,
    '1' when others;

-- When eading from the SRAM, read word comes from read hword register and 
-- SRAM bus (read register is loaded in previous cycle).
sram_rd_data <= sram_rd_data_reg & sram_databus;

sram_input_halfword_register:
process(clk)
begin
    if clk'event and clk='1' then
        sram_rd_data_reg <= sram_databus;
    end if;
end process sram_input_halfword_register;


--------------------------------------------------------------------------------
-- I/O interface -- IO is assumed to behave like synchronous memory

io_byte_we <= byte_we_reg when dps=data_write_io_0 else "0000";
io_rd_addr <= data_rd_addr_reg;
io_wr_addr <= data_wr_addr_reg;
io_wr_data <= data_wr_reg;
io_rd_vma <= '1' when dps=data_read_io_0 else '0';


--------------------------------------------------------------------------------
-- CPU stall control

-- Stall the CPU when either state machine needs it
mem_wait <= (code_wait or data_wait) and not reset;

-- Assert code_wait until the cycle where the CPU has valid code word on its
-- code bus
with cps select code_wait <=
    '1' when code_refill_bram_0,
    '1' when code_refill_bram_1,
    '1' when code_refill_bram_2,
    '1' when code_wait_for_dcache,
    '0' when others;

-- Assert code_wait until the cycle where the CPU has valid data word on its
-- code bus AND no other operations are ongoing that may use the external buses.
with dps select data_wait <=
    '1' when data_writethrough_sram_0,
    '1' when data_writethrough_sram_1,
    '1' when data_refill_sram_0,
    '1' when data_refill_sram_1,
    '1' when data_refill_bram_0,
    '1' when data_refill_bram_1,
    '1' when data_read_io_0,
    '0' when others;

end architecture stub;
