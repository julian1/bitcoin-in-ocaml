
(*
  corebuild -package sha main8.byte
   *)

(* open Core  *)
(*open Sha256 *)

type header =
{
  magic : int;
  command : string;
  length : int;
  checksum : int;
}

type ip_address =
{
  (* use a tuple or list ?, or 32 byte integer ? or 4 8 byte integers*)
  a : int;
  b : int;
  c : int;
  d : int;
  port : int;
}

(* change name version_pkt? or version msg *)
type version =
{
  protocol : int ;
  nlocalServices : Int64.t;
  nTime : Int64.t;
  from : ip_address;
  to_ : ip_address;
  nonce : Int64.t;
  agent : string;
  height : int;
  relay : int;
}

let printf = Printf.printf

let hex_of_char c =
  let hexa = "0123456789abcdef" in
  let x = Char.code c in
  hexa.[x lsr 4], hexa.[x land 0xf]

let hex_of_string s =
  (* functional *)
  let n = String.length s in
  let buf = Buffer.create (n*2) in
  for i = 0 to n-1 do
    let x, y = hex_of_char s.[i] in
    Buffer.add_char buf x;
    Buffer.add_char buf y;
    (*Buffer.add_char buf ' ';
    Buffer.add_char buf s.[i];
    Buffer.add_char buf '\n';
    *)
  done;
  Buffer.contents buf



(* decode byte in s at pos *)
let dec1 s pos = int_of_char @@ String.get s pos

(* other endiness form
let dec s pos bytes =
let rec dec_ s pos bytes acc =
	let value = (acc lsl 8) + (dec1 s pos) in
    if pos >= bytes then value
    else dec_ s (pos+1) bytes value in
	dec_ s pos (pos+bytes-1) 0
*)

(* decode integer value of string s at position using n bytes *)
let dec s start bytes =
  let rec dec_ pos acc =
    let value = (acc lsl 8) + (dec1 s pos) in
      if pos == start then value
      else dec_ (pos-1) value in
    dec_ (start+bytes-1) 0

(* with new position in string - change name decodeInteger *)
let dec_ s pos n = n+pos, dec s pos n
let decodeInteger8 s pos = dec_ s pos 1
let decodeInteger32 s pos = dec_ s pos 4

(* decode integer value of string s at position using n bytes *)
let dec64_ s start bytes =
  let rec dec_ pos acc =
    let value = Int64.add (Int64.shift_left acc 8) (Int64.of_int (dec1 s pos)) in
      if pos == start then value
      else dec_ (pos-1) value in
    dec_ (start+bytes-1) 0L

let decodeInteger64 s pos = 8+pos, dec64_ s pos 8

(* string manipulation *)
let strsub = String.sub
let strlen = String.length
let strrev = Core.Core_string.rev

(* with new position *)
let decs_ s pos n = n+pos, strsub s pos n

let sha256 s = Sha256.string s |> Sha256.to_bin
let sha256d s = sha256 s |> sha256
(*let checksum s = sha256d s |> (fun x -> strsub x 0 4) |> (fun x -> decodeInteger32 x 0 ) *)

let checksum s = let (x: string ) = sha256d s in
  dec x 0 4

let decodeString s pos =
  let pos, len = decodeInteger8 s pos in
  let pos, s = decs_ s pos len in
  pos, s

let decodeAddress s pos =
  (* let () = printf "Addr %s\n" @@ hex_of_string (strsub s pos 26 ) in *)
  let pos, _ = dec_ s pos 20 in
  let pos, a = decodeInteger8 s pos in
  let pos, b = decodeInteger8 s pos in
  let pos, c = decodeInteger8 s pos in
  let pos, d = decodeInteger8 s pos in
  let pos, e = decodeInteger8 s pos in
  let pos, f = decodeInteger8 s pos in
  let port = (e lsl 8 + f) in
  pos, { a = a; b = b; c = c; d = d; port = port }

let decodeHeader s pos =
  let pos, magic = decodeInteger32 s pos in
  let pos, command = decs_ s pos 12 in
  let pos, length = decodeInteger32 s pos in
  let _, checksum = decodeInteger32 s pos in
  { magic = magic; command = command; length = length; checksum = checksum; }

let decodeVersion s pos =
  let pos, protocol = decodeInteger32 s pos in
  let pos, nlocalServices = decodeInteger64 s pos in
  let pos, nTime = decodeInteger64 s pos in
  let pos, from = decodeAddress s pos in
  let pos, to_ = decodeAddress s pos in
  let pos, nonce = decodeInteger64 s pos in
  let pos, agent = decodeString s pos in
  let pos, height = decodeInteger32 s pos in
  let _, relay = decodeInteger8 s pos in
  { protocol = protocol; nlocalServices = nlocalServices; nTime = nTime;
  from = from; to_ = to_; nonce  = nonce; agent = agent; height = height;
  relay = relay;
  }

let enc bytes value =
  String.init bytes (fun i ->
    let h = 0xff land (value lsr (i * 8)) in
    char_of_int h
  )

let encodeInteger8 value = enc 1 value
let encodeInteger16 value = enc 2 value
let encodeInteger32 value = enc 4 value

let enc64 bytes value =
  String.init bytes (fun i ->
    let h = Int64.logand 0xffL (Int64.shift_right value (i * 8)) in
    char_of_int (Int64.to_int h)
  )

let encodeInteger64 value = enc64 8 value

let encodeString (h : string) = enc 1 (strlen h) ^ h

(* should use a concat function, for string building *)
let encodeAddress (h : ip_address ) =
  let zeros n = String.init n (fun _ -> char_of_int 0) in
  encodeInteger8 0x1
  ^ zeros 17
  ^ encodeInteger16 0xffff
  ^ encodeInteger8 h.a
  ^ encodeInteger8 h.b
  ^ encodeInteger8 h.c
  ^ encodeInteger8 h.d
  ^ (encodeInteger8 (h.port lsr 8))
  ^ (encodeInteger8 h.port )

let encodeVersion (h : version) =
  encodeInteger32 h.protocol
  ^ encodeInteger64 h.nlocalServices
  ^ encodeInteger64 h.nTime
  ^ encodeAddress h.from
  ^ encodeAddress h.to_
  ^ encodeInteger64 h.nonce
  ^ encodeString h.agent
  ^ encodeInteger32 h.height
  ^ encodeInteger8 h.relay


let encodeHeader (h : header ) =
  let zeros n = String.init n (fun _ -> char_of_int 0) in
  (* we have a string with hex values that we have to change to encoded vals
    really not sure if the magic shouldn't be decoded as int? 0xffwwaaa
    etc.
  *)

  encodeInteger32 h.magic
  ^ h.command 
  ^ zeros (12 - strlen h.command)
  ^ encodeInteger32 h.length
  ^ encodeInteger32 h.checksum



(* dump the string - not pure *)
let rec printRaw s a b =
	let () = printf "magic %d - '%c' %d %d %d\n" a s.[a] (int_of_char s.[a]) (dec s a 4) (dec s a 8) in
  if a > b then ()
  else printRaw s (a+1) b

let printHeader (h : header) =
  (* let () = printf "magic %s\n" h.magic in *)
  let () = printf "magic %x\n"  h.magic in
  let () = printf "command %s\n" h.command in
  let () = printf "length %d %d\n" h.length (h.length + 24) in
  printf "checksum %x\n" h.checksum

let printAddress (h : ip_address ) =
  printf "%d.%d.%d.%d:%d\n" h.a h.b h.c h.d h.port

let printVersion (h : version ) =
  let () = printf "protocol_version %d\n" h.protocol in
  let () = printf "nLocalServices %s\n" @@ Int64.to_string h.nlocalServices in
  let () = printf "nTime %s\n" @@ Int64.to_string h.nTime  in
  let () = printAddress h.from in
  let () = printAddress h.to_ in
  let () = printf "nonce %s\n" @@ Int64.to_string h.nonce in
  let () = printf "agent %s\n" h.agent in
  let () = printf "height %d\n" h.height in
  printf "relay %d\n" h.relay


let h =
  if false
  then
    "\xF9\xBE\xB4\xD9version\x00\x00\x00\x00\x00j\x00\x00\x00\xE8\xFF\x94\xD0q\x11\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00(#\xE0T\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x7F\x00\x00\x01 \x8D\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xFF\xFF\x12\xBDzI \x8D\xE9\xD4n\x1F0!\xF8\x11\x14/bitcoin-ruby:0.0.6/\xD1\xF3\x01\x00\xFF" else
    let in_channel = open_in "response.bin" in
    let h = Core.In_channel.input_all in_channel in
    let () = close_in in_channel in
    h
in
  let header = decodeHeader h 0 in
  let payload = strsub h 24 header.length in
  let version = decodeVersion payload 0 in
  let () = printHeader header in
  let () = printf "\n" in
(*  let () = printf "%s\n" @@ hex_of_string payload in *)
  let () = printVersion version in
(*  let () = printf "payload checksum is %s\n" (payload |> checksum |> hex_of_string) in *)
  let () = printf "payload checksum is %x \n" ( checksum payload ) in

  let () = printf "----------\n" in

  let u = encodeVersion version in
  let () = printVersion ( decodeVersion u 0 ) in
  let () = printf "checksum is %x\n" ( checksum u ) in

  let () = printf "----------\n" in
(*  let () = printf "%s\n" @@ hex_of_string u in *)


let pack =
  let header = {
    magic = 0xf9beb4d9;
    command = "version";
    length = strlen u ;
    checksum = checksum u;
  } in
  let eheader = encodeHeader header in

  let header = decodeHeader eheader 0 in
  let () = printHeader header in



  () in
()


(*
  TODO
  - done - fix the address encoder

  - need to hex functions - to easily compare the data .
    can only use printf %x with integers
*)


let () = printf "finished\n"




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
printf "version %d\n" (dec h 20 4);
*)


