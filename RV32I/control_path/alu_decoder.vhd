library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.alu_ops_pkg.all;

entity alu_decoder is
   port ( 
      -- from data_path
      alu_2bit_op_i  : in std_logic_vector(1 downto 0);
      funct3_i       : in std_logic_vector (2 downto 0);
      funct7_i       : in std_logic_vector (6 downto 0);
      -- to data_path
      alu_op_o       : out std_logic_vector(4 downto 0); 
      ready:in std_logic;
      clk:in std_logic); 
end entity;

architecture behavioral of alu_decoder is
    signal alu_op_s:std_logic_vector(4 downto 0);
    signal prev_op_s:std_logic_vector(4 downto 0);
begin 
   -- pronalazi odgovarajucu operaciju ALU jedinice u odnosu na:
   --    dvobitni signal alu_2bit_op koji dolazi iz control decodera
   --    funct3 i funct7 polja u instrukciji
   alu_op_o <= alu_op_s;
   alu_dec:process(alu_2bit_op_i,funct3_i,funct7_i,ready,prev_op_s)is
   begin
      -- podrazumevane vrednosti
      alu_op_s <= "00000"; 
      if prev_op_s = divs_op then
        prev_op_s <= divs_op;
      elsif prev_op_s = divu_op then
        prev_op_s <= divu_op;
      elsif prev_op_s = rems_op then
        prev_op_s <= rems_op;
      elsif prev_op_s = remu_op then
        prev_op_s <= remu_op;
      elsif prev_op_s = mul_op then
        prev_op_s <= mul_op;
      elsif prev_op_s = mulh_op then
        prev_op_s <= mulh_op;
      elsif prev_op_s = mulhsu_op then
        prev_op_s <= mulhsu_op;
      elsif prev_op_s = mulhu_op then
        prev_op_s <= mulhu_op;
      else
        prev_op_s <= "00000";
      end if;     
      if ready = '0' then
          alu_op_s <= prev_op_s;
      else
          case alu_2bit_op_i is
             when "00" => 
                alu_op_s <= add_op;
             when "01" =>
                alu_op_s <= eq_op;
             when others =>
                if(funct7_i = "0000001")then
                    case funct3_i is
                        when "000" =>
                            alu_op_s <= mul_op;
                            prev_op_s <= mul_op;
                        when "001" =>
                            alu_op_s <= mulh_op;
                            prev_op_s <= mulh_op;
                        when "010" =>
                            alu_op_s <= mulhsu_op;
                            prev_op_s <= mulhsu_op;
                        when "011" =>
                            alu_op_s <= mulhu_op;
                            prev_op_s <= mulhu_op;       
                        when "100" =>
                            alu_op_s <= divs_op;
                            prev_op_s <= divs_op;
                        when "101" =>
                            alu_op_s <= divu_op;
                            prev_op_s <= divu_op;
                        when "110" =>
                            alu_op_s <= rems_op;
                            prev_op_s <= rems_op;
                        when others =>
                            alu_op_s <= remu_op;
                            prev_op_s <= remu_op;
                    end case;
                else
                    case funct3_i is
                       when "000" =>
                          alu_op_s <= add_op;
                          if(funct7_i(5)='1')then 
                             alu_op_s <= sub_op;
                          end if;
                       when "110" =>
                          alu_op_s <= or_op;
                       --adding
                       when "100" => 
                           alu_op_s <= xor_op;
                       when "010" =>
                            alu_op_s <= lts_op;
                       when "001" =>
                            alu_op_s <= sll_op;
                       when "101" =>
                            if(funct7_i(5) = '0')then
                                alu_op_s <= srl_op;
                            else
                                alu_op_s <= sra_op; -- add later
                            end if; 
                       when "011" =>
                          alu_op_s <= ltu_op;      
                       -- end
                       when others =>
                          alu_op_s <= and_op;
                    end case;
                end if;
          end case;
      end if;
      
   end process;

end architecture;
