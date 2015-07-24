
module M = Message
module D = Difficulty
module A = Address

open OUnit2

(* and tx sig checking *)

let test1 ctx =
  let s = M.string_of_hex "1b0404cb" in 
  let d = D.decode_difficulty s in
  let ds =  d |> A.string_of_z in
  assert_equal ds (M.string_of_hex "0404cb000000000000000000000000000000000000000000000000" ) 


let test2 ctx =
  let s = M.string_of_hex "1b0404cb" in 
  let d = D.decode_difficulty s in
  let bdiff = D.bdiff d in
  assert_equal bdiff 16307. 



let tests =
   "difficulty">::: [ "test1">:: test1; "test2">:: test2;  ] 


