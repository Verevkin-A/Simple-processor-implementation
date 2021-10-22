-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2021 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Aleksandr Verevkin (xverev00)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- ram[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_WREN  : out std_logic;                    -- cteni z pameti (DATA_WREN='0') / zapis do pameti (DATA_WREN='1')
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA obsahuje stisknuty znak klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna pokud IN_VLD='1'
   IN_REQ    : out std_logic;                     -- pozadavek na vstup dat z klavesnice
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- pokud OUT_BUSY='1', LCD je zaneprazdnen, nelze zapisovat,  OUT_WREN musi byt '0'
   OUT_WREN : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

	--signal initialization
	signal PC_AM  : std_logic_vector (11 downto 0);
	signal CNT_AM : std_logic_vector (11 downto 0);
	signal PTR_AM : std_logic_vector (9 downto 0);
	
	signal PC_INC  : std_logic;
	signal CNT_INC : std_logic;
	signal PTR_INC : std_logic;
	
	signal PC_DEC  : std_logic;
	signal CNT_DEC : std_logic;
	signal PTR_DEC : std_logic;
	
	--multiplexor signals initialization
	signal multiplexor_in  : std_logic_vector (1 downto 0) := "00";
	signal multiplexor_out : std_logic_vector (7 downto 0) := "00000000";
	
	--exisiting states of FSM initialization
	type states is (start_st, fetch_st, decode_st, s_val_inc, s_val_inc2, s_val_dec, s_val_dec2, loopL_st,
						 loopL_st2, loopL_st3, loopL_st4, loopR_st, loopR_st2, loopR_st3, loopR_st4, loopR_st5,
						 tilda_st, tilda_st2, tilda_st3, tilda_st4, get_st, get_again_st, print_st, print_again_st, end_st);

	--initialization of present and next state of FSM
	signal presentSt : states := start_st;
	signal nextSt    : states;

begin

	--PC box process
	--for keeping track of symbols readed
	programCounter: process (RESET, CLK, PC_INC, PC_DEC, PC_AM)
	begin
		if (RESET = '1') then
			PC_AM <= "000000000000";
		elsif rising_edge(CLK) then
			if (PC_DEC = '1') then
				PC_AM <= PC_AM - 1;
			elsif (PC_INC = '1') then
				PC_AM <= PC_AM + 1;
			end if;
		end if;
	end process programCounter;
	CODE_ADDR <= PC_AM;

	--PTR box process
	--for keeping track of place in the memory
	memoryPointer: process (RESET, CLK, PTR_INC, PTR_DEC, PTR_AM)
	begin
		if (RESET = '1') then
			PTR_AM <= "0000000000";
		elsif rising_edge(CLK) then
			if (PTR_DEC = '1') then
				PTR_AM <= PTR_AM - 1;
			elsif (PTR_INC = '1') then
				PTR_AM <= PTR_AM + 1;
			end if;
		end if;
	end process memoryPointer;
	DATA_ADDR <= PTR_AM;
	
	--CNT box process
	--for keeping track on the while loops
	whileCounter: process (RESET, CLK, CNT_INC, CNT_DEC, CNT_AM)
	begin
		if (RESET = '1') then
			CNT_AM <= "000000000000";
		elsif rising_edge(CLK) then
			if (CNT_DEC = '1') then
				CNT_AM <= CNT_AM - 1;
			elsif (CNT_INC = '1') then
				CNT_AM <= CNT_AM + 1;
			end if;
		end if;
	end process whileCounter;
	OUT_DATA <= DATA_RDATA;
	
	--MX
	--for writing values to the memory
	multiplexor: process (RESET, CLK, multiplexor_in)
	begin
		if (RESET = '1') then
			multiplexor_out <= "00000000";
		elsif rising_edge(CLK) then
			case multiplexor_in is
				when "00" => multiplexor_out <= IN_DATA;
				when "10" => multiplexor_out <= DATA_RDATA - 1;
				when "01" => multiplexor_out <= DATA_RDATA + 1;
				when others => multiplexor_out <= "00000000";
			end case;
		end if;
	end process multiplexor;
	DATA_WDATA <= multiplexor_out;
	
	statesController: process (CLK, RESET, EN)
	begin
		if (RESET = '1') then
			presentSt <= start_st;
		elsif rising_edge(CLK) then
			if EN = '1' then
				presentSt <= nextSt;
			end if;
		end if;
	end process statesController;
	
	--Finite State Machine
	--Main Process
	mainFSM : process (presentSt, OUT_BUSY, IN_VLD, CODE_DATA, DATA_RDATA, CNT_AM)
	begin
		--set all values on zeros
		--change them later if needed
		PC_INC <= '0';
		PC_DEC <= '0';
		PTR_INC <= '0';
		PTR_DEC <= '0';
		CNT_INC <= '0';
		CNT_DEC <= '0';
		
		CODE_EN <= '0';
		DATA_EN <= '0';
		DATA_WREN <= '0';
		IN_REQ <= '0';
		OUT_WREN <= '0';
		
		multiplexor_in <= "00";
		
		case presentSt is
			when start_st =>
				nextSt <= fetch_st;
			when fetch_st =>
				CODE_EN <= '1';
				nextSt <= decode_st;
			when decode_st =>
				case CODE_DATA is
					--"+"
					when "00101011" =>
						PC_INC <= '1';
						DATA_WREN <= '0';
						DATA_EN <= '1';
						nextSt <= s_val_inc;
					--"-"
					when "00101101" =>
						PC_INC <= '1';
						DATA_WREN <= '0';
						DATA_EN <= '1';
						nextSt <= s_val_dec;
					--">"
					when "00111110" => 
						PC_INC <= '1';
						PTR_INC <= '1';
						nextSt <= fetch_st;
					--"<"
					when "00111100" =>
						PC_INC <= '1';
						PTR_DEC <= '1';
						nextSt <= fetch_st;
					--"["
					when "01011011" =>
						PC_INC <= '1';
						DATA_WREN <= '0';
						DATA_EN <= '1';
						nextSt <= loopL_st;
					--"]"
					when "01011101" =>
						DATA_WREN <= '0';
						DATA_EN <= '1';
						nextSt <= loopR_st;
					--"."
					when "00101110" =>
						PC_INC <= '1';
						DATA_WREN <= '0';
						DATA_EN <= '1';
						nextSt <= print_st;
					--","
					when "00101100" =>
						PC_INC <= '1';
						IN_REQ <= '1';
						multiplexor_in <= "00";
						nextSt <= get_st;
					--"~"
					when "01111110" =>
						PC_INC <= '1';
						CNT_INC <= '1';
						nextSt <= tilda_st;
					--"null"
					when "00000000" => nextSt <= end_st;
					when others =>
						PC_INC <= '1';
						nextSt <= fetch_st;
				end case;				
--"+"
			when s_val_inc =>
				multiplexor_in <= "01";
				nextSt <= s_val_inc2;
			when s_val_inc2 =>
				DATA_WREN <= '1';
				DATA_EN <= '1';
				nextSt <= fetch_st;
--"-"
			when s_val_dec =>
				multiplexor_in <= "10";
				nextSt <= s_val_dec2;
			when s_val_dec2 =>
				DATA_WREN <= '1';
				DATA_EN <= '1';
				nextSt <= fetch_st;
--"print"
			when print_st =>
				if (OUT_BUSY /= '1') then
					OUT_WREN <= '1';
					nextSt <= fetch_st;
				else
					nextSt <= print_again_st;
				end if;
			when print_again_st =>
				DATA_EN <= '1';
				DATA_WREN <= '0';
				nextSt <= print_st;
--"get"
			when get_st =>
				if (IN_VLD /= '0') then
					DATA_EN <= '1';
					DATA_WREN <= '1';
					nextSt <= fetch_st;
				else
					nextSt <= get_again_st;
				end if;
			when get_again_st =>
				IN_REQ <= '1';
				multiplexor_in <= "00";
				nextSt <= get_st;
--"["				
			when loopL_st =>
				if (DATA_RDATA /= (DATA_RDATA'range => '0')) then
					nextSt <= fetch_st;
				else
					CNT_INC <= '1';
					CODE_EN <= '1';
					nextSt <= loopL_st2;
				end if;
			when loopL_st2 =>
				if (CNT_AM = (CNT_AM'range => '0')) then
					nextSt <= fetch_st;
				else
					if (CODE_DATA = "01011011") then
						CNT_INC <= '1';
						nextSt <= loopL_st4;
					else
						CODE_EN <= '1';
						nextSt <= loopL_st3;
					end if;
				end if;
			when loopL_st3 =>
				if (CODE_DATA = "01011101") then
					CNT_DEC <= '1';
				end if;
				nextSt <= loopL_st4;
			when loopL_st4 =>
				PC_INC <= '1';
				CODE_EN <= '1';
				nextSt <= loopL_st2;
--"]"
			when loopR_st =>
				if (DATA_RDATA = (DATA_RDATA'range => '0')) then
					PC_INC <= '1';
					nextSt <= fetch_st;
				else
					CNT_INC <= '1';
					PC_DEC <= '1';
					nextSt <= loopR_st2;
				end if;
			when loopR_st2 =>
				CODE_EN <= '1';
				nextSt <= loopR_st3;
			when loopR_st3 =>
				if (CNT_AM = (CNT_AM'range => '0')) then
					nextSt <= fetch_st;
				else
					if (CODE_DATA = "01011101") then
						CNT_INC <= '1';
						nextSt <= loopR_st5;
					else
						CODE_EN <= '1';
						nextSt <= loopR_st4;
					end if;
				end if;
			when loopR_st4 =>
					if (CODE_DATA = "01011011") then
						CNT_DEC <= '1';
					end if;
					nextSt <= loopR_st5;
			when loopR_st5 =>
				if (CNT_AM = (CNT_AM'range => '0')) then
					PC_INC <= '1';
					nextSt <= loopR_st2;
				else
					PC_DEC <= '1';
					nextSt <= loopR_st2;
				end if;
			
--"break"
			when tilda_st =>
				CODE_EN <= '1';
				nextSt <= tilda_st2;
			when tilda_st2 =>
				if (CNT_AM = (CNT_AM'range => '0')) then
					nextSt <= fetch_st;
				else
					CODE_EN <= '1';
					nextSt <= tilda_st3;
				end if;
			when tilda_st3 =>
				if (CODE_DATA = "01011011") then
					PC_INC <= '1';
					CNT_INC <= '1';
					nextSt <= tilda_st;
				else
					CODE_EN <= '1';
					nextSt <= tilda_st4;
				end if;
			when tilda_st4 =>
				PC_INC <= '1';
				if (CODE_DATA = "01011101") then
					CNT_DEC <= '1';
				end if;
				nextSt <= tilda_st;
--"null"
			when end_st => nextSt <= end_st;
			when others =>
		end case;
	
	end process mainFSM;

end behavioral;
