
module S = String
module M = Message
module A = Address 
module CS = Core.Core_string

(* think we have to use z, noting that there's a big ugly division in there... 

  https://en.bitcoin.it/wiki/Difficulty
*)


(* TODO this is useful enough that may want in M *)
let get s i = S.get s i |> int_of_char 


let decode_difficulty s =
  let exp = get s 0 in 
  let base = (get s 3) + (get s 2 lsl 8) + (get s 1 lsl 16) in 
  Z.mul (Z.of_int base) (Z.pow (Z.of_int 2) (8 * (exp - 3))) 


let bdiff v = 
  let target_1_difficulty = 
    "00000000FFFF0000000000000000000000000000000000000000000000000000" 
    |> M.string_of_hex |> A.z_of_string 
  in
  (* TODO implement fractional component *)
  let bdiff = Z.div target_1_difficulty v in
  Z.to_float bdiff 




