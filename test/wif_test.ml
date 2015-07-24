
module M = Message

open OUnit2

let hex = M.string_of_hex "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"

let test1 ctx =
  let wif = "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" in
  let s = Wif.wif_of_string hex false in
  assert_equal s wif


let test2 ctx =
  let wif = "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617" in
  let s =  Wif.wif_of_string hex true in
  assert_equal s wif




let tests =
   "wif">::: [ "test1">:: test1; "test2">:: test2; ]


