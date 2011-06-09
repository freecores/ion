--##############################################################################
-- RS-232 transmitter, parametrizable bit rate through generics.
-- Bit rate defaults to 19200 bps @50MHz.
-- WARNING: Hacked up for light8080 demo. Poor performance, no formal testing!
-- I don't advise using this in for any general purpose.
--##############################################################################
-- Copyright (C) 2011 Jose A. Ruiz
--                                                              
-- This source file may be used and distributed without         
-- restriction provided that this copyright statement is not    
-- removed from the file and that any derivative work contains  
-- the original copyright notice and the associated disclaimer. 
--                                                              
-- This source file is free software; you can redistribute it   
-- and/or modify it under the terms of the GNU Lesser General   
-- Public License as published by the Free Software Foundation; 
-- either version 2.1 of the License, or (at your option) any   
-- later version.                                               
--                                                              
-- This source is distributed in the hope that it will be       
-- useful, but WITHOUT ANY WARRANTY; without even the implied   
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      
-- PURPOSE.  See the GNU Lesser General Public License for more 
-- details.                                                     
--                                                              
-- You should have received a copy of the GNU Lesser General    
-- Public License along with this source; if not, download it   
-- from http://www.opencores.org/lgpl.shtml
--##############################################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity rs232_tx is
  generic (
    BAUD_RATE     : integer := 19200;
    CLOCK_FREQ    : integer := 50000000);
  port (  
      clk         : in std_logic;
      reset       : in std_logic;
      rdy         : out std_logic;
      load        : in std_logic;
      data_i      : in std_logic_vector(7 downto 0);
      txd         : out std_logic);
end rs232_tx;

architecture hardwired of rs232_tx is

-- Bit period expressed in master clock cycles
constant BIT_PERIOD : integer := (CLOCK_FREQ / BAUD_RATE);

signal counter :      std_logic_vector(13 downto 0);
signal data :         std_logic_vector(10 downto 0);
signal ctr_bit :      std_logic_vector(3 downto 0);
signal tx :           std_logic;

begin

process(clk)
begin
if clk'event and clk='1' then
  
  if reset='1' then
    data <= "10111111111";
    tx <= '0';
    ctr_bit <= "0000";
    counter <= (others => '0');
  elsif load='1' and tx='0' then
    data <= "1"&data_i&"01";
    tx <= '1';
  else
    if tx='1' then
      if conv_integer(counter) = BIT_PERIOD then --e.g. 5200 for 9600 bps
        counter <= (others => '0');
        data(9 downto 0) <= data(10 downto 1);
        data(10) <= '1';
        if ctr_bit = "1010" then
           tx <= '0';
           ctr_bit <= "0000";
        else
           ctr_bit <= ctr_bit + 1;
        end if;
      else
        counter <= counter + 1;
      end if;
    end if;
  end if;
end if;
end process;

rdy <= not tx;
txd <= data(0);

end hardwired;