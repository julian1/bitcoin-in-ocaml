
open OUnit2;;

open Message 


let mytest2 () =
  (* lets try to manually pack the payload *)
  let payload = {
      protocol = 70002;
      nlocalServices = 1L;
      nTime = 1424343350L;
      from = { address = 203,57,212,102; port = 34185 };
      to_ = { address = 50,68,44,128; port = 8333 };
      nonce = -4035119509127777989L ;
      agent = "/Satoshi:0.9.4/";
      height = 344194;
      relay = 1;
  } in
  let u = encodeVersion payload in
  let header = {
    magic = 0xf9beb4d9;
    command = "version";
    length = strlen u ;
    checksum = checksum u;
  } in
  let eheader = encodeHeader header in
  let _, dheader = decodeHeader eheader 0 in
  let () = Printf.printf "**** here\n" in
  let () = Printf.printf "%s" @@ formatHeader dheader in
  ()


let mytest1 () =
	(* checks decoding a version message and reencoding gives the same checksum *)
  let h =
  if true
  then
    "\xF9\xBE\xB4\xD9version\x00\x00\x00\x00\x00j\x00\x00\x00\xE8\xFF\x94\xD0q\x11\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00(#\xE0T\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x7F\x00\x00\x01 \x8D\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x12\xBDzI \x8D\xE9\xD4n\x1F0!\xF8\x11\x14/bitcoin-ruby:0.0.6/\xD1\xF3\x01\x00\xFF" else
    let in_channel = open_in "response.bin" in
    let h = Core.In_channel.input_all in_channel in
    let () = close_in in_channel in
    h
in
  let _, header = decodeHeader h 0 in
  let payload = strsub h 24 header.length in
  let _, version = decodeVersion payload 0 in
  let () = Printf.printf "%s" @@ formatHeader header in
  let () = Printf.printf "\n" in
  let () = Printf.printf "%s" @@ formatVersion version in
  let () = Printf.printf "checksum is %x \n" ( checksum payload ) in
  let () = Printf.printf "----------\n" in
  let u = encodeVersion version in
(*  let () = printf "%s" @@ formatVersion ( decodeVersion u 0 ) in *)
  let () = Printf.printf "checksum is %x\n" ( checksum u ) in
  let () = Printf.printf "----------\n" in
	checksum payload


let test1 test_ctxt = assert_equal (mytest1 ()) 0xd094ffe8  

let tests =
(*"test9">::: *)
 ["test9">:: test1; (* "test2">:: test2 *) ]
;;


(*
let result = (mytest1 ==  0xd094ffe ) in
()
*)


