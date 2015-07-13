(*
  corebuild -I src  -package pgocaml,microecc,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  src/client3.byte
 *)


module M = Message
module U = Util
module PG = U.PG  (* TODO PG shouldn't depend on U, move PG out of Util *)

let (>>=) = Lwt.(>>=)
let return = Lwt.return




open M


let read_from_file filename =
  let in_channel = open_in filename in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  s

let tx_s = read_from_file "test_data/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb"
let txprev_s = read_from_file "test_data/d1ae76b9e9275fc88e3163dfba0a6bf5b3c8fe6a259a45a29e96a7c710777905"


(* substitute input script, and clear others *)
let substitute tx i output_script =
	let f index (input: M.tx_in ) =
		if index == i then { input with script = M.encode_script output_script; }
		else { input with script = M.encode_script [] }
	in
	{ tx with inputs = List.mapi f tx.inputs }


let encode_and_hash tx =
  (* ok, now we need a re-encode tx function and we need to append 1 byte to end before hasing *)
  let s = M.encodeTx tx ^ M.encodeInteger32 1 in
  let hash = M.sha256d s in
  hash


(* so we'd loop through the inputs *)

let _,tx = M.decodeTx tx_s 0
let input = List.nth tx.inputs 0
(*let () = Printf.printf "%s\n" @@ M.hex_of_string input .previous  *)
let input_script = M.decode_script input.script

let _, txprev = M.decodeTx txprev_s 0
let output = List.nth txprev.outputs 0
let output_script = M.decode_script output.script


let signature,pubkey = match input_script with
  | M.BYTES s :: M.BYTES p :: [] -> s, p

(* how do we know whether to decompress the pubkey? *)
let pubkey = Microecc.decompress pubkey

(* so we substitute the prev output script into current tx with its outputs *)
let tx = substitute tx 0 output_script
let hash = encode_and_hash tx

(*
let () = Printf.printf "%s\n" @@ M.formatTx (tx )
*)


let Some (r,s) = M.decode_der_signature signature in
let decoded_sig = r ^ s in
let x = Microecc.verify pubkey hash decoded_sig in
let () = Printf.printf "sig result %b\n" x in
()



let log s = U.write_stdout s

(* '\x0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb' *)

let get_db_tx db hash =
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



let () = Lwt_main.run U.(

  log "connecting to db"
  >> PG.connect ~host:"127.0.0.1" ~database: "prod" ~user:"meteo" ~password:"meteo" ()
  >>= fun db ->
    let hash = M.string_of_hex "0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" in
    get_db_tx db hash
  >>= fun result ->
    let hash = M.sha256d result |> M.strrev in
    log @@ M.hex_of_string hash
  >>
    log @@ M.hex_of_string result
  >>
    (* foldm over inputs - to build a structure? *)
    let _,tx = M.decodeTx result 0 in
    let input = List.nth tx.inputs 0 in
    log @@ M.hex_of_string input.previous ^ " " ^ string_of_int input.index
  >>
    log "whoot"
)

(*
  - For standard Bitcoind UTXOs are going to include the full output - including the script.
    so verifying tx's can be done entirely in memory to be looked up on disk .

  - we really want to index outputs... then we can get at them quickly, and even return
  all of them in one request.

  - if we are verifying txs we are going to have to manage the chainstate as if the block
    is included. eg. the latest block back to genesis only.  

*)
