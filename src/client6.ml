
module S = String
module M = Message
module A = Address 

(* think we have to use z, noting that there's a big ugly division in there... 

Z.of_int: int -> t
0x0404cb * 2**(8*(0x1b - 3)) = 0x00000000000404CB000000000000000000000000000000000000000000000000
*)


(* TODO this is useful enough that may want in M *)
let get s i = S.get s i |> int_of_char 

let string_of_z z = z |> Z.to_bits |> Core.Core_string.rev 

let z_of_string s = s |> Core.Core_string.rev |> Z.of_bits



let difficulty_target s =
  let exp = get s 0 in 
  let base = (get s 3) + (get s 2 lsl 8) + (get s 1 lsl 16) in 
  Z.mul (Z.of_int base) (Z.pow ( (Z.of_int 2)) (8 * (exp - 3))) 


let s = M.string_of_hex "1b0404cb" in 
let v = difficulty_target s in


let () = print_endline (v |> string_of_z |> M.hex_of_string) in
let () = print_endline (M.hex_of_string ( v |> string_of_z |> z_of_string  |> string_of_z )) in
()



