
module M = Message

open OUnit2
open M

(* lets try to manually pack the payload *)
let version = {
    protocol = 70002;
    nlocalServices = 1L;
    nTime = 1424343350L;
    from = { address = 203,57,212,102; port = 34185 };
    to_ = { address = 50,68,44,128; port = 8333 };
    nonce = -4035119509127777989L ;
    agent = "/Satoshi:0.9.4/";
    height = 344194;
    relay = 1;
} 


let test1 ctx =
  (* message header encoding and decoding *)
  let payload = encodeVersion version in
  let header = {
    magic = 0xf9beb4d9;
    command = "version";
    length = strlen payload;
    checksum = checksum payload;
  } in
  let eheader = encodeHeader header in
  let _, dheader = decodeHeader eheader 0 in
  assert_bool "true"
    (header.magic = dheader.magic
    && header.command = dheader.command
    && header.length = dheader.length
    && header.checksum = dheader.checksum
    )


let test2 ctx =
  (* check decoding a version message and reencoding gives the same checksum, indicating all fields valid *)
  let message =
    "\xF9\xBE\xB4\xD9version\x00\x00\x00\x00\x00j\x00\x00\x00\xE8\xFF\x94\xD0q\x11\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00(#\xE0T\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x7F\x00\x00\x01 \x8D\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x12\xBDzI \x8D\xE9\xD4n\x1F0!\xF8\x11\x14/bitcoin-ruby:0.0.6/\xD1\xF3\x01\x00\xFF" in
  let _, header = decodeHeader message 0 in
  let payload = strsub message 24 header.length in
  let _, version = decodeVersion payload 0 in
  let encoded = encodeVersion version in
  assert_equal (checksum encoded) header.checksum



let test3 ctx =
  (* block header decoding *)
  let in_channel = open_in "test/data/000000000000000007c5b3e47c690e6ab9e75fdf1f47bfc7a247f29176be6d9f" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  let _, header = M.decodeBlock Bitcoin s 0 in  
  assert_bool "true" (
    header.previous = M.string_of_hex "000000000000000010006fe522dd3d6251c7d7ba217d294bcb4f99dcc11b1d24"
    && header.merkle = M.string_of_hex "e658aef520b5fa1687f1c33a3bfc0336722fab49fe87ef0c96d46693f68d914b"
    && header.bits = 404196666
    && header.nTime = 1425091936
    && header.nonce = 2316010512
    ) 


let tests =
   "message">::: [ "test1">:: test1;  "test2">:: test2; "test3">::test3; ] 

