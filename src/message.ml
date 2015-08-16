(*
  TODO

  This is really not the right file...

-	get rid of caml case for function. eg int_of_string not intOfS.
  - need to hex functions - to easily compare the data .
   -  can only use printf %x with integers
*)

module S = String
module CS = Core.Core_string
module L = List
module B = Buffer




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


type block =
{
  version : int;
  previous : string;
  merkle : string;
  nTime : int;
  bits : int;
  nonce : int;
  (* tx_count : int; *)
}


type script_token =
  | BYTES of string   (* change to upper case *)
  | UNKNOWN of int
  | BAD

  | OP_1 (* OP_TRUE   81*)
  | OP_2
  | OP_3

  | OP_RETURN
  | OP_DUP
  | OP_EQUAL
  | OP_EQUALVERIFY
  | OP_HASH160
  | OP_CHECKSIG
  | OP_CHECKMULTISIG


(* change name tx_input *)
type tx_in =
{
  previous : string; (* 32 byte tx hash *)
  index : int;
  (* should be a variant either a string ... or a decoded list of tokens? *)
  (* script: script_token list; *)
  script : string;
  sequence : int;

  pos : int;
  length : int;
}

(* need to sort out naming convention for types *)
type tx_out =
{
  value : Int64.t;
  (* script: script_token list; *)
  script : string;

  pos : int;
  length : int;
}

type tx =
{
  (* hash: string; - hash is calculated not embedded *)
  version: int;
  inputs: tx_in list ;
  outputs: tx_out list;
  lockTime : int;

  (* calculated on parse *)
  pos : int;
  length : int;
}


let hex_of_char c =
  let hexa = "0123456789abcdef" in
  let x = Char.code c in
  hexa.[x lsr 4], hexa.[x land 0xf]


(*
  move to Misc.ml ?

*)
let hex_of_string s =
  (* functional *)
  let n = S.length s in
  let buf = B.create (n*2) in
  for i = 0 to n-1 do
    let x, y = hex_of_char s.[i] in
    B.add_char buf x;
    B.add_char buf y;
    (*B.add_char buf ' ';
    B.add_char buf s.[i];
    B.add_char buf '\n';
    *)
  done;
  B.contents buf



let int_of_hex (c : char) =
  (* change name int_of_hex_char ? *)
  let c1 = int_of_char c in
  match c with 
    | '0' .. '9' -> c1 - int_of_char '0'
    | 'a' .. 'f' -> 10 + c1 - int_of_char 'a'
    | 'A' .. 'F' -> 10 + c1 - int_of_char 'A'
    | _ -> raise (Failure "invalid hex value")


let string_of_hex (s: string) =
  (* TODO perhaps rename binary_of_hex *)
  let n = S.length s in
  let buf = B.create (n/2) in
  for i = 0 to n/2-1 do
    let i2 = i * 2 in
    let x = int_of_hex s.[i2] in
    let y = int_of_hex s.[i2+1] in
    B.add_char buf @@ char_of_int (x lsl 4 + y)
  done;
  B.contents buf



(* TODO horrible *)
let hex_of_int =
  Printf.sprintf "%x"

(* string manipulation *)
(* TODO remove and use module prefixes *)
let strsub = S.sub
let strlen = S.length
let strrev = CS.rev
let zeros n = S.init n (fun _ -> char_of_int 0)

(* decode byte in s at pos *)
let dec1 s pos = int_of_char @@ S.get s pos

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

(* TODO all hashes etc should probably be removed from message decoding *)
(* hashing *)
(* let sha256 s = s |> Sha256.string |> Sha256.to_bin *)
let sha256 (s:string) = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s
let ripemd160 (s:string) = Cryptokit.hash_string (Cryptokit.Hash.ripemd160()) s

let sha256d s = s |> sha256 |> sha256
let checksum s = s |> sha256d |> fun x -> dec x 0 4 (* TODO only used for message checksum - should move ? *)

(* hmmmn we don't always want to decode to integer *)
let checksum2 s = s |> sha256d |> (fun x -> S.sub x 0 4 )


(* decode items - this should be generalized decodeItems
  - don't pass f through the recursion and shield the rec function
- can do it with a fold?
*)

(* f is decode function, at pos, count items 
  TODO factor the count, from the action  
  - also do the L.rev at the end. not in the recursion
*)
let decodeNItems s pos f count =
  let rec fff pos acc count =
    if count == 0 then
	    pos, (L.rev acc)  (* move this! *)
    else let pos, x = f s pos in
      fff pos (x::acc) (count-1)
  in fff pos [] count


let decodeS s pos =
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
  (* TODO change Core.Std.String to Core.String  CS  *)
  let x = match ( CS.index_from command 0 '\x00' ) with
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
  let pos, agent = decodeS s pos in
  let pos, height = decodeInteger32 s pos in
  let _, relay = decodeInteger8 s pos in
  pos, { protocol = protocol; nlocalServices = nlocalServices; nTime = nTime;
    from = from; to_ = to_; nonce  = nonce; agent = agent; height = height;
    relay = relay;
  }


let decodeVersionLtc s pos =
  let pos, protocol = decodeInteger32 s pos in
  let pos, nlocalServices = decodeInteger64 s pos in
  let pos, nTime = decodeInteger64 s pos in
  let pos, from = decodeAddress s pos in
  let pos, to_ = decodeAddress s pos in
  let pos, nonce = decodeInteger64 s pos in
  let pos, agent = decodeS s pos in
  let pos, height = decodeInteger32 s pos in
(*  let _, relay = decodeInteger8 s pos in *)
  pos, { protocol = protocol; nlocalServices = nlocalServices; nTime = nTime;
    from = from; to_ = to_;
	nonce  = nonce; agent = agent; height = height; relay = 0;
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




let decode_script' s =
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
        f pos (BYTES bytes::acc)
      else
        let op = match c with
          | 81 -> OP_1
          | 82 -> OP_2
          | 83 -> OP_3
          | 106 -> OP_RETURN
          | 118 -> OP_DUP
          | 135 -> OP_EQUAL
          | 136 -> OP_EQUALVERIFY
          | 169 -> OP_HASH160
          | 172 -> OP_CHECKSIG
          | 174 -> OP_CHECKMULTISIG
          | _ -> UNKNOWN c
        in f pos (op::acc)
    else pos, acc
  in let _, result = f 0 []
  in L.rev result


let decode_script s =
  try  decode_script' s
  with _ -> [BAD]




let format_token x =
  match x with
    | OP_1 -> "OP_1"
    | OP_2 -> "OP_2"
    | OP_3 -> "OP_3"
    | OP_RETURN	 -> "OP_RETURN"
    | OP_DUP -> "OP_DUP"
    | OP_EQUAL -> "OP_EQUAL"
    | OP_HASH160 -> "OP_HASH160"
    | OP_EQUALVERIFY-> "OP_EQUALVERIFY"
    | OP_CHECKSIG -> "OP_CHECKSIG"
    | OP_CHECKMULTISIG -> "OP_CHECKMULTISIG"

    | BYTES c -> "BYTES " ^ hex_of_string c
    | UNKNOWN c -> "UNKNOWN " ^ string_of_int c
    | BAD -> "BAD"

let format_script tokens =
  S.concat " " @@ L.map format_token tokens


(*
  pos and len must be with respect to tx_start 
*)

let decodeTxOutput s pos =
  let first = pos in
  let pos, value = decodeInteger64 s pos in
  let pos, scriptLen = decodeVarInt s pos in
  let pos, script = decs_ s pos scriptLen in
  pos, { 
    value = value; 
    script = script;
    pos = first;
    length = pos - first;
  }

let decodeTxInput s pos =
  let first = pos in
  let pos, previous = decodeHash32 s pos in
  let pos, index = decodeInteger32 s pos in
  let pos, scriptLen = decodeVarInt s pos in
  let pos, script = decs_ s pos scriptLen in
  let pos, sequence = decodeInteger32 s pos in
  pos, { 
    previous = previous; 
    index = index;
    script = script ; 
    sequence = sequence; 
    pos = first;
    length = pos - first;
  }



let decodeTx s pos =
	(* we can't do the hash here cause we don't know the tx length, when embedded in a block.
     actually we can, but avoid mixing up parsing and crypto actions
  *)
  let first = pos in
  let pos, version = decodeInteger32 s pos in

  let decodeTxInputs s pos n = decodeNItems s pos decodeTxInput n in
  let pos, inputsCount = decodeVarInt s pos in
  let pos, inputs = decodeTxInputs s pos inputsCount in

  let decodeTxOutputs s pos n = decodeNItems s pos decodeTxOutput n in
  let pos, outputsCount = decodeVarInt s pos in
  let pos, outputs = decodeTxOutputs s pos outputsCount in

  let pos, lockTime = decodeInteger32 s pos in
  pos, {  
    pos = first; 
    length = pos - first; 
    version = version; 
    (* patch offset with respect to tx *)
    inputs = L.map (fun (input:tx_in) -> { input with pos = input.pos - first; }) inputs;
    outputs = L.map (fun (output:tx_out) -> { output with pos = output.pos - first; }) outputs;
    lockTime = lockTime; 
  }



type network =
  | Bitcoin
  | Litecoin 
  | Dogecoin

(* https://en.bitcoin.it/wiki/Merged_mining_specification

  - we have to be careful, that we pick out auxpow header correctly or we risk
  inserting incorrect txs/outputs in the db. 
*)
let decode_aux_pow network version payload pos =

  let _BLOCK_VERSION_AUXPOW = 1 lsl 8 in

  match network with 
    (* | Dogecoin when version = 6422530 -> 
      pos, None
      6422786  land (1 lsl 8) <> 0
    *)
    | Dogecoin when (version land _BLOCK_VERSION_AUXPOW) <> 0 -> 
      (* parent coinbase tx in parent *)
      let pos, aux_tx = decodeTx payload 80 in 

      let pos, aux_block_hash = decodeHash32 payload pos in

      let pos, branch_length1 = decodeVarInt payload pos in
      let pos = pos + (branch_length1 * 32) + 4 in

      let pos, branch_length2 = decodeVarInt payload pos in
      let pos = pos + (branch_length2 * 32) + 4 in

      let pos = pos + 80 in 
      pos, None

    | _ -> pos, None


(* change name decode_block_header *)
let decodeBlock network (s:string) pos  =
  let pos, version = decodeInteger32 s pos in
  let pos, previous = decodeHash32 s pos in
  let pos, merkle = decodeHash32 s pos in
  let pos, nTime = decodeInteger32 s pos in
  let pos, bits = decodeInteger32 s pos in
  let pos, nonce = decodeInteger32 s pos in

  let pos, _ = decode_aux_pow network version s pos in 
 
	pos, ({ version = version; previous = previous; merkle = merkle;
		nTime = nTime; bits = bits; nonce = nonce; 
    } : block)

(*
let decode_block_txs payload =
    (* TODO pass the 80 offset, and maybe return the pos as well *)
    let pos = 80 in
    let pos, tx_count = decodeVarInt payload pos in
    let _, txs = decodeNItems payload pos decodeTx tx_count in
    txs
*)

let decode_block_txs payload pos =
    (* TODO pass the 80 offset, and maybe return the pos as well *)
     (* let pos = 80 in  *)
     let pos, tx_count = decodeVarInt payload pos in 
    (* let tx_count = 1 in *)
    let _, txs = decodeNItems payload pos decodeTx tx_count in
    txs





let decode_block_hash payload =
  strsub payload 0 80 |> sha256d |> strrev


let decodeInv s pos =
  (* TODO this is a varInt
    returns a list, should wrap in a record ?
  *)
  let pos, count = decodeVarInt s pos in
  decodeNItems s pos decodeInvItem count




let enc bytes value =
  S.init bytes (fun i ->
    let h = 0xff land (value lsr (i * 8)) in
    char_of_int h
  )


(* all right our encoding functions don't return a position
  which is odd. options are,
  - use a list... and concat ... not as efficient as writing
  a buffer. with pos in a monad.

  we will have to evaluate substrings - in order to know size to encode them...
  this makes it more complicated,

  this also means we can't just encode in a completely linear sequence with
  a byte buffer.
*)


let encodeInteger8 value = enc 1 value
let encodeInteger16 value = enc 2 value
let encodeInteger32 value = enc 4 value

let enc64 bytes value =
  S.init bytes (fun i ->
    let h = Int64.logand 0xffL (Int64.shift_right value (i * 8)) in
    char_of_int (Int64.to_int h)
  )

let encodeInteger64 value = enc64 8 value

(* should do string reverse if necessary *)
let encodeS (h : string) = enc 1 (strlen h) ^ h

(* change name to encode hash32 and do a sanity check on the length ? *)
let encodeHash32 (h : string) =
  if S.length h <> 32 then
    raise (Failure "hash length ")
  else
  strrev h


(* TODO FIXME!!
pos in
293   match first with
hash
    | 0xfd -> decodeInteger16 s pos
    | 0xfe -> decodeInteger32 s pos
    | 0xff -> (pos, first) (* TODO uggh... this will need a 64 bit int return type *)
    | _ -> (pos, first)


*)
let encodeVarInt x =
	if x < 0xfd then
		encodeInteger8 x
	else if x < 0xffff then
		encodeInteger8 0xfd ^ encodeInteger16 x
	else (*if x < 0xffffffff then *)
		encodeInteger8 0xfe ^ encodeInteger32 x
(*	else
		encodeInteger8 0xff ^ encodeInteger64 x
*)

(*
type tx_in =
{
  previous : string; (* 32 byte tx hash *)
  index : int;
  (* should be a variant either a string ... or a decoded list of tokens? *)
  script: script_token list;
  (* script : string;  *)
  sequence : int;
    let pos, previous = decodeHash32 s pos in
    let pos, index = decodeInteger32 s pos in
    let pos, scriptLen = decodeVarInt s pos in
    let pos, script = decs_ s pos scriptLen in

}
*)

let encode_script tokens =
  let f op = match op with
    | OP_1 -> encodeInteger8 81
    | OP_2 -> encodeInteger8 82
    | OP_3 -> encodeInteger8 83
    | OP_RETURN -> encodeInteger8 106
    | OP_DUP -> encodeInteger8 118
    | OP_EQUAL -> encodeInteger8 135
    | OP_EQUALVERIFY -> encodeInteger8 136
    | OP_HASH160 -> encodeInteger8 169
    | OP_CHECKSIG -> encodeInteger8 172
    | OP_CHECKMULTISIG -> encodeInteger8 174
                (* FIXME length *)
    | BYTES s -> (encodeInteger8 (strlen s)) ^ s

  in L.map f tokens |> S.concat ""


let encodeInput (input : tx_in) =
  [ encodeHash32 input.previous;
    encodeInteger32 input.index ;
    (let s = (* encode_script *) input.script in
    let len = strlen s in
    (encodeInteger8 len) ^ s );  (* FIXME *)  (* and get rid of the ^ *)
    encodeInteger32 input.sequence
  ] |> S.concat ""


let encodeOutput (output : tx_out) =
  [
	encodeInteger64 output.value ;

    (let s = (* encode_script *) output.script in
    let len = strlen s in
    (encodeInteger8 len) ^ s );  (* FIXME *)  (* and get rid of the ^ *)

	(* encodeHash32 output.previous;
    encodeInteger32 output.index ;
    (let s = encode_script output.script in
    let len = strlen s in
    (encodeInteger8 len) ^ s );  (* FIXME *)
    encodeInteger32 output.sequence
*)  ] |> S.concat ""




let encodeTx (tx : tx) =
  S.concat ""
  [ encodeInteger32 tx.version;
    encodeVarInt @@ L.length tx.inputs;
    L.map encodeInput tx.inputs |> S.concat "";
	encodeVarInt @@ L.length tx.outputs;
    L.map encodeOutput tx.outputs |> S.concat "";
	encodeInteger32 tx.lockTime
  ]





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
  ^ encodeS  h.agent
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

(*

*)
(*
  32 and 33 byte r and s values,
  http://bitcoin.stackexchange.com/questions/12554/why-the-signature-is-always-65-13232-bytes-long
  
  https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki

  https://bitcointalk.org/index.php?topic=653313.0

  TODO We have to pad with zero, if < 32 bits... 


    let trim_front s =
      S.sub s 1 (S.length s - 1)  
    in
*)

let decode_der_signature s =
  try 
    let fixup s =
      (* perhaps should check if 33 bytes, since can't be more *)
      S.sub s (S.length s - 32) 32 
    in
    let decode_elt s pos =
      let pos, _0x02 = decodeInteger8 s pos in
      let pos, length = decodeInteger8 s pos in
      (* let () = print_endline @@ "@@@@ len " ^ string_of_int length in *)
      let pos, value = decs_ s pos length  in
      pos, value, _0x02 
    in
    let pos = 0 in
    let pos, header = decodeInteger8 s pos in
    let pos, length = decodeInteger8 s pos in

    let pos, r, rheader = decode_elt s pos in
    (* let () = print_endline @@ "@@@  r " ^ hex_of_string r in *)
    let r = fixup r in

    let pos, s_, sheader= decode_elt s pos in
    (* let () = print_endline @@ "@@@ s_ " ^ hex_of_string s_ in *)
    let s_ = fixup s_ in

    let pos, sigType = decodeInteger8 s pos in

    match header = 0x30 && rheader = 0x02 && sheader = 0x02 with 
      | true -> Some (r, s_, sigType)
      | false -> None (* could remove and let case throw *)
  with _  
    -> None




let formatHeader (h : header) =
  S.concat "" [
    "magic:    "; hex_of_int h.magic;
    "\ncommand:  "; h.command;
    "\nlength:   "; string_of_int h.length;
    "\nchecksum: "; hex_of_int h.checksum
  ]

let formatAddress (h : ip_address ) =
  let soi = string_of_int in
  let a,b,c,d = h.address  in
  S.concat "." [
    soi a; soi b; soi c; soi d
  ] ^ ":" ^ soi h.port

let formatVersion (h : version) =
  (* we can easily write a concat that will space fields, insert separator etc, pass as tuple pairs instead*)
  S.concat "" [
    "protocol_version: "; string_of_int h.protocol;
    "\nnLocalServices:   "; Int64.to_string h.nlocalServices;
    "\nnTime:            "; Int64.to_string h.nTime;
    "\nfrom:             "; formatAddress h.from;
    "\nto:               "; formatAddress h.to_;
    "\nnonce:            "; Int64.to_string h.nonce;
    "\nagent:            "; h.agent;
    "\nheight:            "; string_of_int h.height;
    "\nrelay:            "; string_of_int h.relay
  ]

let formatInv h =
  S.concat "" @@ L.map (
    fun (inv_type, hash ) ->
      "\n inv_type " ^ string_of_int inv_type
      ^ ", hash " ^ hex_of_string hash
    )h



let formatBlock (h : block) =
  S.concat "" [
    "version:    "; string_of_int h.version;
    "\nprevious: "; hex_of_string h.previous;
    "\nmerkle:   "; hex_of_string h.merkle;
    "\nnTime:    "; string_of_int h.nTime;
    "\nbits:    "; string_of_int h.bits;
    "\nnonce:    "; string_of_int h.nonce;
    (*"\ntx_count:    "; string_of_int h.tx_count;  *)
  ]




(* not sure if we want to enclose this scope, in the format tx action *)
let formatTxInput input = S.concat "" [
  "  previous: " ^ hex_of_string input.previous
  ^ "\n  index: " ^ string_of_int input.index
  ^ "\n  script: " ^ (* format_script*) hex_of_string input.script
  ^ "\n  sequence: " ^ string_of_int input.sequence
]

let formatTxInputs inputs =
  S.concat "\n" @@ L.map formatTxInput inputs

let formatTxOutput output = S.concat "" [
  "  value: " ^ Int64.to_string output.value
  ^ "\n  script: " ^ (* format_script *) hex_of_string output.script
]

let formatTxOutputs outputs =
  S.concat "\n" @@ L.map formatTxOutput outputs

let formatTx tx =
  (* " hash " ^ hex_of_string tx.hash  *)
  "\n version " ^ string_of_int tx.version
  ^ "\n inputsCount " ^(string_of_int @@ L.length tx.inputs)
  ^ "\n" ^ formatTxInputs tx.inputs
  ^ "\n outputsCount " ^ (string_of_int @@ L.length tx.outputs )
  ^ "\n" ^ formatTxOutputs tx.outputs
  ^ "\n lockTime " ^ string_of_int tx.lockTime



(*  bitcoin magic_head: "\xF9\xBE\xB4\xD9",
  testnet magic_head: "\xFA\xBF\xB5\xDA",
  litecoin magic_head: "\xfb\xc0\xb6\xdb",

(* let m = 0xdbb6c0fb   litecoin *)
*)

(*
  - Do we want an option type for the different networks that control
    generation of magic number ...
  - there might be other things that need to change as well ....
*)


(* perhaps move out of here? *) 
let get_magic = function 
  | Bitcoin -> 0xd9b4bef9 
  | Litecoin -> 0xdbb6c0fb 
  | Dogecoin -> 0xc0c0c0c0 


let encodeMessage network command payload = 
  let header = encodeHeader {
    magic = get_magic network ;
    command = command ;
    length = strlen payload;
    checksum = checksum payload;
  } in
  header ^ payload


let encodeSimpleMessage network command = 
  encodeHeader {
    magic = get_magic network ;
    command = command;
    length = 0;
    (* clients seem to use e2e0f65d - hash of first part of header? *)
    checksum = 0;
  } 
 

