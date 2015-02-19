
(* read in IPv4 and IPv6 ping packets and display them.
 * $Id$

	So the actual compiler will assemble the right string for us

 *)

open Printf

let h = "\xF9\xBE\xB4\xD9version\x00\x00\x00\x00\x00j\x00\x00\x00\xE8\xFF\x94\xD0q\x11\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00(#\xE0T\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x7F\x00\x00\x01 \x8D\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x12\xBDzI \x8D\xE9\xD4n\x1F0!\xF8\x11\x14/bitcoin-ruby:0.0.6/\xD1\xF3\x01\x00\xFF"

in 

Printf.printf "here %x\n" @@ int_of_char h.[0]  ;

(* Printf.printf "%d " (  Scanf.sscanf "\xF9" "%x" (fun x -> x) ); *)
 
Printf.printf "%d\n" ( int_of_string "0xF9" );

let u = 0xff in 
Printf.printf "u is %d\n" u ; 

let s = "hello" in
Printf.printf "value %d\n" (int_of_char @@ String.get s 0 )

let dec1 s k = int_of_char @@ String.get s k
let dec2 s k = dec1 s k * 256 + dec1 s (k + 1) in 


Printf.printf "value %x\n" (dec2 h  0  )

let buf = Buffer.create 100 in 
Buffer.add_char buf 'a' ; 


Printf.printf "buf %s\n" (Buffer.contents buf); 



