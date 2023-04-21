LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
use ieee.math_real.all;
use work.alu_ops_pkg.all;


ENTITY ALU IS
    GENERIC(
        WIDTH : NATURAL := 32);
    PORT(
        a_i    : in STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0); --prvi operand
        b_i    : in STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0); --drugi operand
        op_i   : in STD_LOGIC_VECTOR(4 DOWNTO 0); --selekcija operacije
        res_o  : out STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0); --rezultat
        zero_o : out STD_LOGIC; --signalni bit jednakosti nuli
        of_o   : out STD_LOGIC; --signalni bit prekoracenja opsega
        clk    : in std_logic;
        reset  : in std_logic;
        ready  : out std_logic;
        muldiv_stall : out std_logic);
END ALU;

ARCHITECTURE behavioral OF ALU IS
    attribute use_dsp : string;
	attribute use_dsp of behavioral : architecture is "yes";
    constant  l2WIDTH : natural := integer(ceil(log2(real(WIDTH))));
    type state_t is (idle, d1, d2, d3);
    signal state_reg, state_next : state_t;
    signal rem_res, q_res, add_res, sub_res, or_res, and_res,res_s, eq_res, sll_res, srl_res, slt_res, xor_res, sra_res, sltu_res :  STD_LOGIC_VECTOR(WIDTH-1 DOWNTO 0);
    signal start     : std_logic;
    signal signed_in : std_logic;
    signal ma_reg, mb_reg : STD_LOGIC_VECTOR(WIDTH - 1 downto 0);
    signal mm_reg, mp_reg : STD_LOGIC_VECTOR(2 * WIDTH - 1 downto 0);
    signal mua_reg, mub_reg : STD_LOGIC_VECTOR(WIDTH - 1 downto 0);
    signal mum_reg, mup_reg : STD_LOGIC_VECTOR(2 * WIDTH - 1 downto 0);    
    signal msua_reg, msub_reg : STD_LOGIC_VECTOR(WIDTH - 1 downto 0);
    signal msum_reg, msup_reg : STD_LOGIC_VECTOR(2 * WIDTH + 1 downto 0);    
    signal mstall, dstall : STD_LOGIC;
    signal mready, dready : STD_LOGIC;
BEGIN

    with op_i select
        start <= '1' when divs_op,          
                 '1' when divu_op,
                 '1' when rems_op,
                 '1' when remu_op,
                 '0' when others;
    with op_i select
        signed_in <= '1' when divs_op,          
                     '1' when rems_op,
                     '0' when others;            
    div: entity work.divider(Beh)
        generic map (WIDTH => 32)
        port map (
            clk       => clk      ,
            reset     => reset    ,
            start     => start    ,
            ready     => dready    ,
            signed_in => signed_in,
            rs1       => a_i      ,
            rs2       => b_i      ,
            stall_out => dstall,
            divw0_out => open,
            quotient  => q_res ,
            reminder  => rem_res );
            
    mul: process (clk) is begin 
	   if rising_edge(clk) then
	       ma_reg <= a_i;
	       mb_reg <= b_i;
	       mm_reg <= std_logic_vector(signed(ma_reg) * signed(mb_reg));
    	   mp_reg <= mm_reg;
    	   
    	   mua_reg <= a_i;
	       mub_reg <= b_i;
	       mum_reg <= std_logic_vector(unsigned(mua_reg) * unsigned(mub_reg));
    	   mup_reg <= mum_reg;
    	   
    	   msua_reg <= a_i;
	       msub_reg <= b_i;
	       msum_reg <= std_logic_vector(signed(msua_reg(WIDTH - 1) & msua_reg) * signed('0' & msub_reg));
    	   msup_reg <= msum_reg;
        end if;
	end process;
	
	process(clk) is begin
	   if (clk'event and clk = '1') then
	       state_reg <= state_next;
	   end if;
	end process;
	
	mul_stall: process (state_reg, state_next, op_i) is begin
        mstall <= '1';
        state_next <= idle;
        mready <= '0';
        case state_reg is 
            when idle =>
                mready <= '1';
                case op_i is
                    when mul_op =>
                        state_next <= d1;
                    when mulh_op =>
                        state_next <= d1;
                    when mulhsu_op =>
                        state_next <= d1;
                    when mulhu_op =>
                        state_next <= d1;
                    when others =>
                        mstall <= '0';
                end case;
            when d1 =>
                state_next <= d2;
            when d2 =>
                state_next <= d3;
            when d3 =>
                mstall <= '0';
            when others =>
        end case;
    end process;
	
    -- sabiranje
    add_res <= std_logic_vector(signed(a_i) + signed(b_i));
    -- oduzimanje
    sub_res <= std_logic_vector(signed(a_i) + signed(b_i));
    -- i kolo
    and_res <= a_i and b_i;
    -- ili kolo
    or_res <= a_i or b_i;
    -- jednakost
    eq_res <= std_logic_vector(to_unsigned(1,WIDTH)) when (signed(a_i) = signed(b_i)) else
    std_logic_vector(to_unsigned(0,WIDTH));
                 
    --sll
    sll_res <= std_logic_vector(shift_left(signed(a_i), to_integer(signed(b_i))));--potencijalno ograniciti broj bita b_i koji se koriste
    --srl
    srl_res <= std_logic_vector(shift_right(unsigned(a_i), to_integer(unsigned(b_i))));
    -- xor
    xor_res <= a_i xor b_i;
    -- slt
    slt_res <= std_logic_vector(to_unsigned(1,WIDTH)) when (signed(a_i) < signed(b_i)) else
    std_logic_vector(to_unsigned(0,WIDTH)); -- komplement 2?
    --sra
    sra_res <= std_logic_vector(shift_right(signed(a_i), to_integer(unsigned(b_i))));
    -- ltu
    sltu_res <= std_logic_vector(to_unsigned(1,WIDTH)) when (unsigned(a_i) < unsigned(b_i)) else
    std_logic_vector(to_unsigned(0,WIDTH));
    -- prosledi jedan od rezultata na izlaz u odnosu na  operaciju
    res_o <= res_s;
    with op_i select
        res_s <= and_res when and_op,
                 or_res  when or_op,
                 add_res when add_op,
                 sub_res when sub_op,
                 eq_res  when eq_op,
                 sll_res when sll_op,
                 srl_res when srl_op,
                 xor_res when xor_op,
                 slt_res when lts_op,
                 sra_res when sra_op,
                 sltu_res when ltu_op,
                 q_res when divs_op,          
                 q_res when divu_op,
                 rem_res when rems_op,
                 rem_res when remu_op,
                 mp_reg(WIDTH - 1 downto 0) when mul_op,
		         mp_reg(2 * WIDTH - 1 downto WIDTH) when mulh_op,
		         mup_reg(2 * WIDTH - 1 downto WIDTH) when mulhsu_op,
		         msup_reg(2 * WIDTH - 1 downto WIDTH) when mulhu_op,
                 (others => '1') when others;

    ready <= mready and dready;
    
    muldiv_stall <= mstall or dstall;
    -- signalni izlazi
    -- postavi singnalni bit jednakosti nuli
    zero_o <= '1' when res_s = std_logic_vector(to_unsigned(0,WIDTH)) else
              '0';
    -- postavi signalni bit prekoracenja
    of_o <= '1' when ((op_i="00011" and (a_i(WIDTH-1)=b_i(WIDTH-1)) and ((a_i(WIDTH-1) xor res_s(WIDTH-1))='1')) or (op_i="10011" and (a_i(WIDTH-1)=res_s(WIDTH-1)) and ((a_i(WIDTH-1) xor b_i(WIDTH-1))='1'))) else
            '0';


END behavioral;