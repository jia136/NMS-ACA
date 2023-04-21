library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity divider is
    Generic(WIDTH : INTEGER:= 32)                              ;
    Port ( clk       : in  STD_LOGIC                           ;
           reset     : in  STD_LOGIC                           ;
           start     : in  STD_LOGIC                           ;
           ready     : out STD_LOGIC                           ;
           signed_in : in  STD_LOGIC                           ;
           rs1       : in  STD_LOGIC_VECTOR (WIDTH-1 downto 0) ;
           rs2       : in  STD_LOGIC_VECTOR (WIDTH-1 downto 0) ;
           stall_out : out STD_LOGIC                           ;
           divw0_out : out STD_LOGIC                           ;
           quotient  : out STD_LOGIC_VECTOR (WIDTH-1 downto 0) ;
           reminder  : out STD_LOGIC_VECTOR (WIDTH-1 downto 0));
end divider;

architecture Beh of divider is
    --attribute use_dsp : string                                      ;
    --attribute use_dsp of Beh : architecture is "yes"                ;
    type state_type is (idle, l1, l2)                               ;
    signal state_reg     , state_next   : state_type                ;
    
    signal rem_next      , rem_reg      : unsigned(WIDTH downto 0)  ;
    signal q_next        , q_reg        : unsigned(WIDTH-1 downto 0);
    signal divisor_next  , divisor_reg  : unsigned(WIDTH-1 downto 0);
     
    signal rem_sign_next , rem_sign_reg : std_logic                 ;
    signal q_sign_next   , q_sign_reg   : std_logic                 ;
    
    signal i_next        , i_reg        : unsigned(WIDTH-1 downto 0);
    signal tmp                          : unsigned(WIDTH   downto 0);
    
    signal q_sign_sel1   , q_sign_sel2  : std_logic                 ;
    signal rem_sign_sel  , div_sign_sel : std_logic                 ;
begin
                   
    process (clk, reset)
    begin
        if reset = '0' then        
            state_reg    <= idle             ;
            divisor_reg  <= (others => '0')  ;
            rem_reg      <= (others => '0')  ;
            q_reg        <= (others => '0')  ;
            i_reg        <= (others => '0')  ;
            rem_sign_reg <= '0'              ;
            q_sign_reg   <= '0'              ;
            
        elsif (clk'event and clk = '1') then
            state_reg    <= state_next       ;
            divisor_reg  <= divisor_next     ;
            rem_reg      <= rem_next         ;
            q_reg        <= q_next           ;
            i_reg        <= i_next           ;
            rem_sign_reg <= rem_sign_next    ;
            q_sign_reg   <= q_sign_next      ;
        end if;
    end process;
    
    process(state_reg, start, signed_in, rs1, rs2, divisor_reg, divisor_next, rem_reg, rem_reg,
            q_reg, q_next, i_reg, i_next, q_sign_reg, q_sign_next, rem_sign_reg, rem_sign_next)
    begin
        divisor_next  <= divisor_reg     ;
        rem_next      <= rem_reg         ;
        q_next        <= q_reg           ;
        i_next        <= i_reg           ;
        q_sign_next   <= q_sign_reg      ;
        rem_sign_next <= rem_sign_reg    ;
        stall_out     <= '0'             ;
        ready         <= '0'             ;
        divw0_out     <= '0'             ;
        tmp           <= (others => '0') ;
        case state_reg is
            when idle => 
                ready <= '1';
                if(unsigned(rs2) = to_unsigned(0, WIDTH)) then
                    divw0_out  <= '1'  ;
                    state_next <= idle ;                
                elsif start = '1' then
                    state_next <= l1;
                    i_next <= (others => '0');
                    stall_out <= '1';
                    
                    if (div_sign_sel = '1') then
                        divisor_next <= unsigned(not rs2) + 1;
                    else
                        divisor_next <= unsigned(rs2);
                    end if;
                    
                    if (rem_sign_sel = '1') then
                        q_next(WIDTH - 1 downto 0) <= unsigned(not rs1) + 1;
                        rem_sign_next <= '1';
                    else
                        q_next(WIDTH - 1 downto 0) <= unsigned(rs1);
                        rem_sign_next <= '0';
                    end if;
                    
                    if (q_sign_sel1 = '1') then
                        q_sign_next <= '1';
                    elsif(q_sign_sel2 = '1')then
                        q_sign_next <= '1';
                    else
                        q_sign_next <= '0';
                    end if;
                    
                else
                    state_next <= idle;
                end if;
            when l1 =>
                stall_out <= '1';
                if ( i_reg < (to_unsigned(WIDTH,WIDTH)+1)) then
                    tmp <= rem_reg - divisor_reg ;
                    if(tmp(WIDTH) = '1' or tmp(WIDTH - 1) = '1') then
                        rem_next <= rem_reg(WIDTH-1 downto 0) & q_reg(WIDTH-1);
                        q_next <= q_reg(WIDTH-2 downto 0) & '0';
                    else
                        rem_next <= tmp(WIDTH-1 downto 0) & q_reg(WIDTH-1);
                        q_next <= q_reg(WIDTH-2 downto 0) & '1';
                    end if;
                    i_next <= i_reg +1;
                    state_next <= l1;
                else
                    state_next <= idle;
                    stall_out     <= '0';
                    rem_next <= (others => '0') ;
                end if;
            when l2 =>
                stall_out  <= '0';
                state_next <= idle;
            when others =>
        end case;
    end process;
    
    quotient     <= std_logic_vector(q_reg) when q_sign_reg = '0' else
                    std_logic_vector(not (q_reg) + 1); 
                           
    reminder     <= std_logic_vector(rem_reg(WIDTH downto 1)) when rem_sign_reg = '0' else
                    std_logic_vector(not (rem_reg(WIDTH downto 1)) + 1); 
                           
    q_sign_sel1  <= (signed_in and rs1(WIDTH-1)) and not(rs2(WIDTH-1)) ;
    q_sign_sel2  <= (signed_in and rs2(WIDTH-1)) and not(rs1(WIDTH-1)) ;
    rem_sign_sel <=  signed_in and rs1(WIDTH-1)                        ;
    div_sign_sel <=  signed_in and rs2(WIDTH-1)                        ;
end Beh;
