
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;

entity eseopll is
  port(
    clk21m  : in std_logic;
    reset   : in std_logic;
    clkena  : in std_logic;
    enawait : in std_logic;
    req     : in std_logic;
    ack     : out std_logic;
    wrt     : in std_logic;
    adr     : in std_logic_vector(15 downto 0);
    dbo     : in std_logic_vector(7 downto 0);
    wav     : out std_logic_vector(15 downto 0)
 );
end eseopll;

architecture RTL of eseopll is

  component jt2413 is
    Port ( rst   : in STD_LOGIC;
           clk   : in STD_LOGIC;
           cen   : in STD_LOGIC;  -- Optional clock enable, if not needed, leave as '1'
           din   : in STD_LOGIC_VECTOR(7 downto 0);
           addr  : in STD_LOGIC;
           cs_n  : in STD_LOGIC;
           wr_n  : in STD_LOGIC;
           -- Combined output
           snd   : out std_logic_vector(15 downto 0);
           sample: out STD_LOGIC);
  end component;
  signal XIN  : std_logic;
  signal D    : std_logic_vector(7 downto 0);
  signal A    : std_logic;
  signal CS_n : std_logic;
  signal WE_n : std_logic;
  signal IC_n : std_logic;
  signal MO   : std_logic_vector(9 downto 0);
  signal RO   : std_logic_vector(9 downto 0);

  signal counter : integer range 0 to 72*6;

  signal A_buf    : std_logic;
  signal dbo_buf  : std_logic_vector(7 downto 0);
  signal CS_n_buf : std_logic;
  signal WE_n_buf : std_logic;

begin

  IC_n <= not reset;

  process (clk21m, reset)

    variable mix : std_logic_vector(10 downto 0);

  begin

    if reset = '1' then

      counter <= 0;

    elsif rising_edge (clk21m) then

      if counter /= 0 then
        counter <= counter - 1;
        ack <= '0';
      else
        if req = '1' then
          if enawait = '1' then
            if adr(0) = '0' then
              counter <= 4*6;
            else
              counter <= 72*6;
            end if;
          end if;
          A_buf <= adr(0);
          dbo_buf <= dbo;
          CS_n_buf <= not req;
          WE_n_buf <= not wrt;
        end if;
        ack <= req;
      end if;

      if (clkena = '1') then

        A    <= A_buf;
        D    <= dbo_buf;
        CS_n <= CS_n_buf;
        WE_n <= WE_n_buf;
        --mix := ('0'&MO) + ('0'&RO) - "01000000000";
        --wav <= mix(wav'range);
        CS_n_buf <= '1';

      end if;

    end if;

  end process;

  U1 : jt2413 port map (reset,
								clk21m,
								clkena,
								dbo,
								adr(0),
								CS_n,
								WE_n,
								wav,
								open);

end RTL;
