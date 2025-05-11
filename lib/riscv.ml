open Hardcaml
open Signal

let opIntR = of_bit_string "0110011"
let opIntI = of_bit_string "0010011"
let opLoad = of_bit_string "0000011"
let opStore = of_bit_string "0100011"
let opBranch = of_bit_string "1100011"
let opJal = of_bit_string "1101111"
let opJalr = of_bit_string "1100111"
let opLui = of_bit_string "0110111"
let opLuiPc = of_bit_string "0010111"
let opEnv = of_bit_string "1110011"

module Funct3 = struct

  let beq = of_bit_string "000"
  let bne = of_bit_string "001"
  let blt = of_bit_string "100"
  let bge = of_bit_string "101"
  let bltu = of_bit_string "110"
  let bgeu = of_bit_string "111"

end
