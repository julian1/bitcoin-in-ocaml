
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
    let h =
      "\xF9\xBE\xB4\xD9version\x00\x00\x00\x00\x00j\x00\x00\x00\xE8\xFF\x94\xD0q\x11\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00(#\xE0T\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x7F\x00\x00\x01 \x8D\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x12\xBDzI \x8D\xE9\xD4n\x1F0!\xF8\x11\x14/bitcoin-ruby:0.0.6/\xD1\xF3\x01\x00\xFF" in
    let _, header = decodeHeader h 0 in
    let payload = strsub h 24 header.length in
    let _, version = decodeVersion payload 0 in
    let encoded = encodeVersion version in
    assert_equal (checksum encoded) header.checksum


let tests =
   "message">::: [ "test1">:: test1;  "test2">:: test2; ]


