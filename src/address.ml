
module M = Message  (* shouldn't be here. move M.hex_of_string out of message *)

module S = String
module CS = Core.Core_string

(* corebuild  -package cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte *)

let z_of_string s =
  s |> CS.rev
    |> Z.of_bits


let string_of_z z =
  z |> Z.to_bits
    |> CS.rev
    |> CS.lstrip ~drop:((=)(char_of_int 0))


let fill n ch = S.init n (fun _ -> ch)

let split_at_pos s i =
  S.sub s 0 i, S.sub s i (S.length s - i)



let code58_of_int i =
  S.get "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" i


let int_of_code58 c =
  let v = int_of_char c in
  match c with
    | '1' .. '9' -> v - int_of_char '1'
    | 'A' .. 'H' -> v - int_of_char 'A' + 9
    | 'J' .. 'N' -> v - int_of_char 'J' + 17
    | 'P' .. 'Z' -> v - int_of_char 'P' + 22
    | 'a' .. 'k' -> v - int_of_char 'a' + 33
    | 'm' .. 'z' -> v - int_of_char 'm' + 44


let count_prefix_chars s ch =
  let rec f i =
    if i < S.length s && S.get s i = ch then
      f (succ i)
    else
      i
  in f 0


let base58_of_string (s : string) =
  let rec f acc z =
    if Z.gt z Z.zero then
      let div, rem = Z.div_rem z (Z.of_int 58) in
      f (code58_of_int (Z.to_int rem)::acc) div
    else
      acc
  in
  let ret = s
    |> z_of_string
    |> f []
    |> CS.of_char_list  in
  let n = count_prefix_chars s (char_of_int 0) in
  fill n (code58_of_int 0) ^ ret



let string_of_base58 (s : string) =
  let len = S.length s in
  let rget i = S.get s (len - i - 1) in
  let rec f i z =
    if i < len then
      let c = rget i in
      let value = int_of_code58 c in
      let b = Z.pow (Z.of_int 58) i in
      f (succ i) (Z.add z (Z.mul (Z.of_int value) b))
    else
      z
  in
  let ret =
    f 0 Z.zero
    |> string_of_z in
  let n = count_prefix_chars s (code58_of_int 0) in
  fill n (char_of_int 0) ^ ret



let s = string_of_base58 "1JeqjYhy7GzCMkbKZ7N9Um6usLNuhsjji1" in
let () = print_endline (M.hex_of_string s) in
let s = base58_of_string s in
let () = print_endline s in
()



let btc_address_of_hash160 (s: string) =
  let s = "\x00" ^ s in
  let checksum = M.checksum2 s in
  base58_of_string (s ^ checksum)



let hash160_of_btc_address(s: string) =
  let s = string_of_base58 s in
  let s, checksum = split_at_pos s (S.length s - 4) in
  if M.checksum2 s = checksum then
    s
  else
    raise (Failure "bad checksum")


let s = M.string_of_hex "000000000000000000000000248bc90202c849ea" in
let a = btc_address_of_hash160 s in
let () = print_endline a in
let s = hash160_of_btc_address a in
print_endline (M.hex_of_string s )


(*
  be nice to have some utils. eg.
  private key (c or u)
    wif encode
    pubkey
    hash160
    btc address

  and back again? from wif private key?
*)

(* this should be held in a test *)
(*
let test1 () =

	let s = M.string_of_hex "c1a235aafbb6fa1e954a68b872d19611da0c7dc9" in
	let () = Printf.printf "encoded %s\n" @@ btc_address_of_hash160 s in

	let s = "030b7b19a00036b102336b53d473ab2c5e516bb5e7e668ceed799a711a3095fd97"
	  |> M.string_of_hex
	  |> Message.sha256
	  |> Message.ripemd160
	  |> btc_address_of_hash160 in

  Printf.printf "encoded %s %d\n" s (Message.strlen s)

*)




