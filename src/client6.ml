
module S = String
module M = Message
module A = Address 

(* think we have to use z, noting that there's a big ugly division in there... 

Z.of_int: int -> t
0x0404cb * 2**(8*(0x1b - 3)) = 0x00000000000404CB000000000000000000000000000000000000000000000000
*)

(* TODO this is useful enough that may want in M *)
let get s i = S.get s 0 |> int_of_char 

let s = M.string_of_hex "1b0404cb" in 

let exp = get s 0 in 
let base = get s 3 + (0xff * get s 2) + (0xffff * get s 1) in 
let v =  Z.mul (Z.of_int 0x0404cb )   (Z.pow ( (Z.of_int 2)) (8 * (exp - 3))) in

let yy = Core.Core_string.rev (Z.to_bits v) in


let () = print_endline (string_of_int exp ) in
let () = print_endline (string_of_int base  ) in
let () = print_endline (M.hex_of_string yy) in
()


(* do we have left shift *)

