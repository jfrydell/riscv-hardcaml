open Hardcaml
open Signal

module Op = struct
  let intR = of_bit_string "0110011"
  let intI = of_bit_string "0010011"
  let load = of_bit_string "0000011"
  let store = of_bit_string "0100011"
  let branch = of_bit_string "1100011"
  let jal = of_bit_string "1101111"
  let jalr = of_bit_string "1100111"
  let lui = of_bit_string "0110111"
  let luiPc = of_bit_string "0010111"
  let env = of_bit_string "1110011"
end

module Funct3 = struct

  let beq = of_bit_string "000"
  let bne = of_bit_string "001"
  let blt = of_bit_string "100"
  let bge = of_bit_string "101"
  let bltu = of_bit_string "110"
  let bgeu = of_bit_string "111"

end
