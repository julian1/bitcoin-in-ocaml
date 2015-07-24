
module M = Message
module A = Address

open OUnit2

(* client5 wif 
  and tx sig checking 
*)

let test1 ctx =
  let h = M.string_of_hex "000000000000000000000000248bc90202c849ea" in
  let a = "1111111111111h111111111WdQPXP" in
  let a2 = A.btc_address_of_hash160 h in
  assert_equal a a2


let test2 ctx =
  let a = "1111111111111h111111111WdQPXP" in
  let h = M.string_of_hex "000000000000000000000000248bc90202c849ea" in
  let h2 = A.hash160_of_btc_address a in
  assert_equal h h2

(* returns type test *)
let tests =
   "address">::: [ "test1">:: test1; "test2">:: test2; ] 


