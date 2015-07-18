
module S = String
module M = Message
module A = Address 

(* https://en.bitcoin.it/wiki/Wallet_import_format *)

(*let s =  (char_of_int 0x80) ^ s  in *)

(*
  probably want a decode as well, so we can work with them directly
  - ughh we need an base58 unencode function
  
  TODO maybe change name to wif_of_string ?
*)

let wif_encode s compressed =
  let s = M.string_of_hex "80" ^ s in  
  let s = match compressed with
    | true -> s ^ M.string_of_hex "01"
    | false -> s
  in
  let checksum = M.sha256d s |> (fun s -> S.sub s 0 4) in
  let s = s ^ checksum in
  A.base58_of_string s


let s =  wif_encode (M.string_of_hex "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d") false in
print_endline  s 




