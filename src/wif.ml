
module S = String
module M = Message
module A = Address

(* https://en.bitcoin.it/wiki/Wallet_import_format *)


(*
  probably want a decode as well, so we can work with them directly
*)

let wif_of_string s compressed =
  match S.length s with
    | 32 ->
      (* TODO check str length *)
      let s = M.string_of_hex "80" ^ s in
      let s = match compressed with
        | true -> s ^ M.string_of_hex "01"
        | false -> s
      in
      let checksum = s |> M.sha256d |> (fun s -> S.sub s 0 4) in
      let s = s ^ checksum in
      A.base58_of_string s
    | _ -> raise (Failure "bad length" )


