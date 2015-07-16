(*
  corebuild -I src  -package pgocaml,microecc,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  src/client3.byte
 *)


module M = Message
module U = Util
module L = List
module S = String
module CL = Core.Core_list

module PG = U.PG  (* TODO PG shouldn't depend on U, move PG out of Util *)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

open M

let fold_m f acc lst =
  let adapt f acc e = acc >>= fun acc -> f acc e in
  L.fold_left (adapt f) (return acc) lst


(*
let read_from_file filename =
  let in_channel = open_in filename in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  s

let tx_s = read_from_file "test_data/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb"
let txprev_s = read_from_file "test_data/d1ae76b9e9275fc88e3163dfba0a6bf5b3c8fe6a259a45a29e96a7c710777905"
*)

(*
  maybe it accumulates ... eg previous value is left in there ... 

  zero padding of the compressed signature???
*)

(* substitute input script, and clear others *)
let substitute tx i output_script =
	let f index (input: M.tx_in ) =
		if index == i then { input with script = M.encode_script output_script; }
		else { input with script = M.encode_script [] }
	in
	{ tx with inputs = L.mapi f tx.inputs }


let encode_and_hash tx =
  (* ok, now we need a re-encode tx function and we need to append 1 byte to end before hasing *)
  let s = M.encodeTx tx ^ M.encodeInteger32 1 in
  let hash = M.sha256d s in
  hash

(*
  0 padding of der or pubkey 

  71 length pubkey works
  72 doesn't
*)

(* change name to checksig *)
let check_scripts tx lst = 
  let combine_script i output = 

    let input = L.nth tx.inputs i in
    let input_script = M.decode_script input.script in
    let output_script = M.decode_script output.script in

    let signature,pubkey = match input_script with
      | M.BYTES s :: M.BYTES p :: [] -> s, p in

    (* let () = print_endline @@ 
      "input " ^ M.format_script input_script 
      ^ " output " ^ M.format_script output_script 
    in *)

    let pubkey = 
      match S.get pubkey 0 |> int_of_char  with 
        | 0x04 -> S.sub pubkey 1 (S.length pubkey - 1)  
        | 0x02|0x03 -> Microecc.decompress pubkey
      in
 
    (* let pubkey = trim pubkey in  *)

    (*
    let () = print_endline @@ "uncompressed pubkey len " ^ (S.length pubkey |> string_of_int ) in
    let () = print_endline @@ "sig len " ^ (S.length signature |> string_of_int ) in
    *)

    (* so we substitute the prev output script into current tx with its outputs *)
    let tx = substitute tx i output_script in
    let hash = encode_and_hash tx in

    let Some (r,s,_) = M.decode_der_signature signature in

    let () = print_endline @@ "r " ^ M.hex_of_string r  in
    let () = print_endline @@ "s " ^ M.hex_of_string s  in
 


    let decoded_sig = r ^ s in
    Microecc.verify pubkey hash decoded_sig
  in
  (*let zipped = CL.zip_exn tx.inputs lst in *)
  L.mapi combine_script lst 



let log s = U.write_stdout s

(* '\x0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb' *)

let get_tx_from_db db hash =
  (*log @@ "getting tx " ^ M.hex_of_string hash
  >>*)
  let query =
  "select substr(d.data,tx.pos+1,tx.len) as data
    from tx
    join block b on b.id = tx.block_id
    join blockdata d on d.id =
    b.blockdata_id
    where tx.hash = $1;
  " in
    PG.prepare db ~query:query ()
    >> PG.execute db ~params:[
      Some (PG.string_of_bytea hash);
    ] ()
  >>= function
    (Some field ::_ )::_ -> return @@ PG.bytea_of_string field
    | _ -> raise (Failure "tx not found")


(* needs to be a list of (hash, index) *)
let get_tx_outputs_from_db db lst =
  let f x (hash,index) = 
    get_tx_from_db db hash 
    >>= fun tx_s ->
      let _,tx = M.decodeTx tx_s 0 in
      let output = L.nth tx.outputs index in
      (* log @@ "get output " ^ M.hex_of_string hash ^ " " ^ string_of_int index ^ " " ^ Int64.to_string output.value 
      >> *)
      return (output::x) 
  in fold_m f [] lst 
  >>= fun r -> return (L.rev r)  (* fmap *)

(*
  has something else got reversed, - eg the outputs whereby,,, it doesn't work
*)

let () = Lwt_main.run U.(M.(

  log "connecting to db"
  >> PG.connect ~host:"127.0.0.1" ~database: "prod" ~user:"meteo" ~password:"meteo" ()
  >>= fun db ->
 (*   let hash = M.string_of_hex "0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" in *) 
 (*   let hash  = M.string_of_hex "90bbbbf21ecd7017b341c44bb1a0860a3adcbee04b716f1798861a368931f667" in  *)
    let hash = M.string_of_hex "9ec4bc49e828d924af1d1029cacf709431abbde46d59554b62bc270e3b29c4b1" in  
    get_tx_from_db db hash
  >>= fun result ->
    let hash = M.sha256d result |> M.strrev in
    log @@ M.hex_of_string hash
  >>
    log @@ M.hex_of_string result
  >>
    let _,tx = M.decodeTx result 0 in
    let lst = L.map (fun input -> (input.previous, input.index)) tx.inputs in
    log @@ "inputs " ^ string_of_int (L.length lst ) 

(*    >> let (first,_) = (L.nth 0 lst) in
    log @@ "first " ^ M.hex_of_string first 
*)
    >> get_tx_outputs_from_db db lst 
    >>= fun outputs -> 

      log @@ "lst " ^ string_of_int (L.length outputs ) 

      >> let s = check_scripts tx outputs 
        |> L.map string_of_bool
        |> S.concat " "
      in
      log @@ "check_scripts result - " ^ s
 
))


(*    let input = List.nth tx.inputs 0 in
    log @@ M.hex_of_string input.previous ^ " " ^ string_of_int input.index
    >> 
*)
(*
  - For standard Bitcoind UTXOs are going to include the full output - including the script.
    so verifying tx's can be done entirely in memory to be looked up on disk .

  - we really want to index outputs... then we can get at them quickly, and even return
  all of them in one request.

  - if we are verifying txs we are going to have to manage the chainstate as if the block
    is included. eg. the latest block back to genesis only.  

*)
