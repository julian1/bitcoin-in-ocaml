
module S = String
module M = Message
module A = Address 

(* https://en.bitcoin.it/wiki/Wallet_import_format *)

(*let s =  (char_of_int 0x80) ^ s  in *)

let wif_encode s =
  let s = M.string_of_hex "80" ^ s in  
  let checksum = M.sha256d s |> (fun s -> S.sub s 0 4) in
  let s = s ^ checksum in
  let s = A.base58_of_string s in 
  s 

let s =  wif_encode (M.string_of_hex "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d") in
print_endline  s 




