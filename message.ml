(*
  TODO

  This is really not the right file...

-	get rid of caml case for function. eg int_of_string not intOfString.
  - need to hex functions - to easily compare the data .
   -  can only use printf %x with integers
*)

type header =
{
  magic : int;
  command : string;
  length : int;
  checksum : int;
}

type ip_address =
{
  address : int * int * int * int;
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


type script_token =
  | Bytes of string
  | Unknown of int
  | OP_DUP
  | OP_EQUAL
  | OP_EQUALVERIFY
  | OP_HASH160
  | OP_CHECKSIG
;;


type tx_in=
{
  previous : string ;
  index : int ; 
  (* should be a variant either a string ... or a decoded list of tokens? *)
  script: script_token list ; 
  (* script : string;  *)
  sequence : int ; 
}

(* need to sort out naming convention for types *)
type tx_out =
{
  value : Int64.t ;	
  script : string;
}

type tx = 
{ 
  (* hash: string; - hash is calculated not embedded *)
  version: int; 
  inputs: tx_in list ; 
  outputs: tx_out list; 
  lockTime : int  
}

let hex_of_char c =
  let hexa = "0123456789abcdef" in
  let x = Char.code c in
  hexa.[x lsr 4], hexa.[x land 0xf]

(* TODO change name hex_of_binary *)
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

(* TODO horrible *)
let hex_of_int =
  Printf.sprintf "%x"

(* string manipulation *)
let strsub = String.sub
let strlen = String.length
let strrev = Core.Core_string.rev
let zeros n = String.init n (fun _ -> char_of_int 0)

(* decode byte in s at pos *)
let dec1 s pos = int_of_char @@ String.get s pos

(* for big-endian 
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
let decodeInteger16 s pos = dec_ s pos 2
let decodeInteger32 s pos = dec_ s pos 4



(*dec_ s pos 4 let pos, hash = decs_ s pos 32 in *)

(* decode integer value of string s at position using n bytes *)
let dec64_ s start bytes =
  let rec dec_ pos acc =
    let value = Int64.add (Int64.shift_left acc 8) (Int64.of_int (dec1 s pos)) in
      if pos == start then value
      else dec_ (pos-1) value in
    dec_ (start+bytes-1) 0L

let decodeInteger64 s pos = 8+pos, dec64_ s pos 8

(* for strings returning position - should obsolete *)
let decs_ s pos n = n+pos, strsub s pos n

(* 256 bit hash - should make general purpose *)
let decodeHash32 s pos = 
  let (a,b) = decs_ s pos 32 in
  a, strrev b

(* hashing *)
(* let sha256 s = s |> Sha256.string |> Sha256.to_bin *)
let sha256 (s:string) = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s
let ripemd160 (s:string) = Cryptokit.hash_string (Cryptokit.Hash.ripemd160()) s

let sha256d s = s |> sha256 |> sha256
let checksum s = s |> sha256d |> fun x -> dec x 0 4

(* hmmmn we don't always want to decode to integer *)
let checksum2 s = s |> sha256d |> (fun x -> String.sub x 0 4 ) 


(* decode items - this should be generalized decodeItems 
  - don't pass f through the recursion and shield the rec function
- can do it with a fold? 
*)

(* f is decode function, at pos, count items *)
let decodeNItems s pos f count =
  let rec fff pos acc count =
    if count == 0 then pos, (List.rev acc)
    else let pos, x = f s pos in
      fff pos (x::acc) (count-1) 
  in fff pos [] count 


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
  pos, { address = a, b, c, d; port = port }

let decodeHeader s pos =
  let pos, magic = decodeInteger32 s pos in
  let pos, command = decs_ s pos 12 in
  let pos, length = decodeInteger32 s pos in
  let _, checksum = decodeInteger32 s pos in
  let x = match ( Core.Std.String.index_from command 0 '\x00' ) with
    | Some n -> strsub command 0 n 
    | None -> command
  in
  pos, { magic = magic; command = x; length = length; checksum = checksum; }

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
  pos, { protocol = protocol; nlocalServices = nlocalServices; nTime = nTime;
    from = from; to_ = to_; nonce  = nonce; agent = agent; height = height;
    relay = relay;
  } 

let decodeInvItem s pos =
  let pos, inv_type = decodeInteger32 s pos in
  let pos, hash = decodeHash32 s pos in
  pos, (inv_type, hash)


let decodeVarInt s pos = 
  let pos, first = decodeInteger8 s pos in
  match first with
    | 0xfd -> decodeInteger16 s pos
    | 0xfe -> decodeInteger32 s pos 
    | 0xff -> (pos, first) (* TODO uggh... this will need a 64 bit int return type *)
    | _ -> (pos, first)
    

let decodeInv s pos =
  (* TODO this is a varInt 
    returns a list, should wrap in a record ? 
  *)
  let pos, count = decodeVarInt s pos in
  decodeNItems s pos decodeInvItem count




let decode_script s =
  let rec f pos acc =
    if pos < strlen s then
      let pos, c = decodeInteger8 s pos in
      (* let () = Printf.printf "whoot pos %d\n" pos in *)

      if ( c >= 1 && c <= 78) then
        let pos, len =
          match c with
            | 76 -> decodeInteger8 s pos
            | 77 -> decodeInteger16 s pos
            | 78 -> decodeInteger32 s pos
            | _ -> pos, c
          in
        let pos, bytes = decs_ s pos len in
        f pos (Bytes bytes::acc)

      else
        let op = match c with
        | 118 -> OP_DUP
        | 135 -> OP_EQUAL
        | 136 -> OP_EQUALVERIFY
        | 169 -> OP_HASH160
        | 172 -> OP_CHECKSIG
        | _ -> Unknown c
        in f pos (op::acc)
    else pos, acc
  in let _, result = f 0 []
  in List.rev result

let format_token x =
  match x with
  | OP_DUP -> "OP_DUP"
  | OP_EQUAL -> "OP_EQUAL"
  | OP_HASH160 -> "OP_HASH160"
  | OP_EQUALVERIFY-> "OP_EQUALVERIFY"
  | OP_CHECKSIG -> "OP_CHECKSIG"
  | Bytes c -> "Bytes " ^ hex_of_string c
  | Unknown c -> "Unknown " ^ string_of_int c


let format_script tokens =
  String.concat " " @@ List.map format_token tokens



let decodeTx s pos =
	(* we can't do the hash here cause we don't know the tx length, when
    it's embedded in a block
   *)
(*  let hash = sha256d s |> strrev in
  let pos = 0 in *)
  let pos, version = decodeInteger32 s pos in 

  let decodeInput s pos = 
    let pos, previous = decodeHash32 s pos in
    let pos, index = decodeInteger32 s pos in
    let pos, scriptLen = decodeVarInt s pos in
    let pos, script = decs_ s pos scriptLen in
    (* should we decode the script here as well? *)

    let pos, sequence = decodeInteger32 s pos in
    pos, { previous = previous; index = index; 
      script = decode_script script ; sequence = sequence; }
  in
  let decodeInputs s pos n = decodeNItems s pos decodeInput n in
  (* should we be reversing the list, when running decodeInput ?  *)
  let pos, inputsCount = decodeVarInt s pos in
  let pos, inputs = decodeInputs s pos inputsCount in

  let decodeOutput s pos =
    let pos, value = decodeInteger64 s pos in
    let pos, scriptLen = decodeVarInt s pos in
    let pos, script = decs_ s pos scriptLen in
    pos, { value = value; script = script; }  
  in
  let decodeOutputs s pos n = decodeNItems s pos decodeOutput n in
  let pos, outputsCount = decodeVarInt s pos in
  let pos, outputs = decodeOutputs s pos outputsCount in

  let pos, lockTime = decodeInteger32 s pos in
  pos, { version = version; inputs = inputs; outputs = outputs; lockTime }



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
let encodeAddress (h : ip_address) =
  (* replace with concat, better algorithmaclly *)
  let a,b,c,d = h.address in
  encodeInteger8 0x1
  ^ zeros 17
  ^ encodeInteger16 0xffff
  ^ encodeInteger8 a
  ^ encodeInteger8 b
  ^ encodeInteger8 c
  ^ encodeInteger8 d
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

let encodeHeader (h : header) =
  encodeInteger32 h.magic
  ^ h.command
  ^ zeros (12 - strlen h.command)
  ^ encodeInteger32 h.length
  ^ encodeInteger32 h.checksum


(* dump the string - not pure *)
(*
let rec printRaw s a b =
	let () = printf "magic %d - '%c' %d %d %d\n" a s.[a] (int_of_char s.[a]) (dec s a 4) (dec s a 8) in
  if a > b then ()
  else printRaw s (a+1) b
*)


let formatHeader (h : header) =
  String.concat "" [
    "magic:    "; hex_of_int h.magic;
    "\ncommand:  "; h.command;
    "\nlength:   "; string_of_int h.length;
    "\nchecksum: "; hex_of_int h.checksum
  ]

let formatAddress (h : ip_address ) =
  let soi = string_of_int in
  let a,b,c,d = h.address  in
  String.concat "" [
    soi a; "." ; soi b; "."; soi c; "."; soi d; ":"; soi h.port
  ]

let formatVersion (h : version) =
  (* we can easily write a concat that will space fields, insert separator etc, pass as tuple pairs instead*)
  String.concat "" [
    "protocol_version: "; string_of_int h.protocol;
    "\nnLocalServices:   "; Int64.to_string h.nlocalServices;
    "\nnTime:            "; Int64.to_string h.nTime;
    "\nfrom:             "; formatAddress h.from;
    "\nto:               "; formatAddress h.to_;
    "\nnonce:            "; Int64.to_string h.nonce;
    "\nagent:            "; h.agent;
    "\nrelay:            "; string_of_int h.relay
  ]

let formatInv h = 
  String.concat "" @@ List.map (
    fun (inv_type, hash ) -> 
      "\n inv_type " ^ string_of_int inv_type 
      ^ ", hash " ^ hex_of_string hash 
    )h  

(* not sure if we want to enclose this scope, in the format tx action *)
let formatInput input = String.concat "" [
  "  previous: " ^ hex_of_string input.previous 
  ^ "\n  index: " ^ string_of_int input.index 
  ^ "\n  script: " ^ format_script input.script 
  ^ "\n  sequence: " ^ string_of_int input.sequence
] 

let formatInputs inputs = 
  String.concat "\n" @@ List.map formatInput inputs

let formatOutput output = String.concat "" [
  "  value: " ^ Int64.to_string output.value
  ^ "\n  script: " ^ hex_of_string output.script
] 

let formatOutputs outputs = 
  String.concat "\n" @@ List.map formatOutput outputs

let formatTx tx = 
  (* " hash " ^ hex_of_string tx.hash  *)
  "\n version " ^ string_of_int tx.version 
  ^ "\n inputsCount " ^(string_of_int @@ List.length tx.inputs)
  ^ "\n" ^ formatInputs tx.inputs
  ^ "\n outputsCount " ^ (string_of_int @@ List.length tx.outputs )
  ^ "\n" ^ formatOutputs tx.outputs
  ^ "\n lockTime " ^ string_of_int tx.lockTime


