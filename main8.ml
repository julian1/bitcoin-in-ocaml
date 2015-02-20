
(* 
  corebuild -package sha main8.byte
   *)

(* open Core  *)
(*open Sha256 *)



let dec1 s pos = 
  int_of_char @@ String.get s pos 
in
(* decode string s, at pos, reading l bytes as a 64 bit Int *)
(*
	little-endian ?
let dec s pos bytes =
let rec dec_ s pos bytes acc =
	let value = (acc lsl 8) + (dec1 s pos) in
    if pos >= bytes then value
    else dec_ s (pos+1) bytes value in
	dec_ s pos (pos+bytes-1) 0
in
*)
(* decode integer value of string s at position using n bytes *)
let dec s pos bytes =
  let rec dec_ s start pos acc =
    let value = (acc lsl 8) + (dec1 s pos) in
      if pos == start then value
      else dec_ s start (pos-1) value in
    dec_ s pos (pos+bytes-1) 0
in
(* and return new position in string *)
let dec_ s pos bytes =
  bytes+pos, dec s pos bytes
in

(* decode string value strings *)
let decs s pos bytes = 
  String.sub s pos bytes 
in
(* and return new position *)
let decs_ s pos bytes =
  bytes+pos, decs s pos bytes
in
(* dump the string *)
let rec dump s a b =
	Printf.printf "magic %d - '%c' %d %d %d\n" a s.[a] (int_of_char s.[a]) (dec s a 4) (dec s a 8);
  if a > b then ()
  else dump s (a+1) b
in


let decAddress s pos =
  (* 26 bytes *)
  let pos, unknown2 = dec_ s pos 20 in
  let pos, a = dec_ s pos 1 in
  let pos, b = dec_ s pos 1 in
  let pos, c = dec_ s pos 1 in
  let pos, d = dec_ s pos 1 in
  let pos, e = dec_ s pos 1 in
  let pos, f = dec_ s pos 1 in
  let port = (e * 256 + f) in
  pos, (a, b, c, d, port)
in


let h = 
  if true
  then
    "\xF9\xBE\xB4\xD9version\x00\x00\x00\x00\x00j\x00\x00\x00\xE8\xFF\x94\xD0q\x11\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00(#\xE0T\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x7F\x00\x00\x01 \x8D\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x12\xBDzI \x8D\xE9\xD4n\x1F0!\xF8\x11\x14/bitcoin-ruby:0.0.6/\xD1\xF3\x01\x00\xFF" else
    let in_channel = open_in "response.bin" in
    let h = Core.In_channel.input_all in_channel in
    let () = close_in in_channel in
    h
in

  let pos = 0 in
  let pos, magic = decs_ h pos 4 in
  let pos, command = decs_ h pos 12 in  (* maybe should just be 20 bytes *)
  let pos, length = dec_ h pos 4 in
  (* checksum is double sha256 *)
  let pos, checksum = decs_ h pos 4 in

  let pos, protocol = dec_ h pos 4 in
  let pos, nlocalServices = dec_ h pos 8 in
  let pos, nTime = dec_ h pos 8 in
  let pos, (a,b,c,d, port) = decAddress h pos in
  let pos, (a1,b1,c1,d1, port1) = decAddress h pos in
  let pos, nonce = dec_ h pos 8 in
  let pos, size = dec_ h pos 1 in
  let pos, agent = decs_ h pos size in
  let pos, height = dec_ h pos 4 in
  let pos, relay = dec_ h pos 1 in

  let () = Printf.printf "pos %d, length %d \n" pos ( String.length h ) in
  let () = Printf.printf "magic %s\n"   magic in
  let () = Printf.printf "command %s\n" command in
  let () = Printf.printf "length %d %d\n" length (length + 24) in
  let () = Printf.printf "checksum %s\n" checksum in

  let () = Printf.printf "protocol_version %d\n" protocol in
  let () = Printf.printf "nLocalServices %d\n" nlocalServices in
  let () = Printf.printf "nTime %d\n" nTime  in
  let () = Printf.printf "a %d.%d.%d.%d:%d\n" a b c d port in
  let () = Printf.printf "a %d.%d.%d.%d:%d\n" a1 b1 c1 d1 port1  in
  let () = Printf.printf "nonce %x\n" nonce in
  let () = Printf.printf "agent %s\n" agent in
  let () = Printf.printf "height %d\n" height in
  let () = Printf.printf "relay %d\n" relay in


let sha256d_ s = 
  Sha256.string s |> Sha256.to_bin |> Sha256.string |> Sha256.to_bin
in

  let () = Printf.printf "u is %s\n" ( String.sub h 24 106 |> sha256d_ |> (fun x -> String.sub x 0 4 )) in
  

(*
let reverse = Core_string.rev 
in

  let () = Printf.printf "y is %s\n" @@ reverse "hello"   in

let m = Hex.of_string ~pretty:true "Hello world!" in 
let () = Printf.printf "m is %s" m 
in
*)

(* 
  Ok, we need a hex version of the value...
  which means we need a hexkb

  why are the digits reversed ? 

  actually we don't need to convert to hex at all!!!
*)

(*
  let () = Printf.printf "u is %s\n" (sha256d "hello") in
  let sha2 = Sha256.to_hex ( Sha1.string sha1bin ) in
  let () = Printf.printf "u is %s\n" sha2 in
*)




  (* let () = dump h 0 (String.length h - 9)  in *)
Printf.printf "finished\n"




(*
283     static member Parse(reader: BinaryReader) =
284         let version = reader.ReadInt32()
285         let services = reader.ReadInt64()
286         let timestamp = reader.ReadInt64()
287         let recv = reader.ReadBytes(26)
288         let from = reader.ReadBytes(26)
289         let nonce = reader.ReadInt64()
290         let userAgent = reader.ReadVarString()
291         let height = reader.ReadInt32()
292         let relay = reader.ReadByte()
293         new Version(version, services, timestamp, recv, from, nonce, userAgent, height, relay)

279     static member Create(timestamp: Instant, recv: IPEndPoint, from: IPEndPoint, nonce: int64, userAgent: string, height: int32, relay:     by

PrddrYou.ToString(), addr.ToString());
 551     PushMessage("version", PROTOCOL_VERSION, nLocalServices, nTime, addrYou, addrMe,
 552                 nLocalHostNonce, FormatSubVersion(CLIENT_NAME, CLIENT_VERSION, std::vector<string>()), nBestHeight, true);
 553 }
 554
intf.printf "payload %d\n" (dec h 16 4);
Printf.printf "version %d\n" (dec h 20 4);
*)


