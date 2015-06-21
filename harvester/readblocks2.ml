
(* scan blocks and compute tx indexes
  native.

  corebuild -I src -package pgocaml,microecc,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax harvester/readblocks2.byte

  Need to get rid of leveldb ref, coming from misc.ml 126

*)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)  (* like bind, but second arg return type is non-monadic *)
let return = Lwt.return

module L = List
module CL = Core.Core_list
module S = String

module M = Message
module Sc = Scanner

(* open M *)


module Lwt_thread = struct
    include Lwt
    include Lwt_chan
end

module PG = PGOCaml_generic.Make (Lwt_thread)


module Utxos = Map.Make(struct type t = string * int let compare = compare end)
module RValues = Map.Make(struct type t = string let compare = compare end)


type mytype =
{
  tx_count : int;
  db : int PG.t ; (* TODO what is this *)
}

(*
  prepare once, then use...
*)

(*
let simple_query db query =
  (* think this is a complete transaction *)
  PG.prepare db ~query (* ~name*) ()
  >> PG.execute db (* ~name*) ~params:[] ()

  IMARY KEY,
    product_no integer REFERENCES products (product_no),
*)

let create_db db =
  PG.(
    PG.begin_work db
    >> inject db "drop table if exists output"
    >> inject db "drop table if exists tx"
    >> inject db "create table tx(id serial primary key, hash bytea)"
    >> inject db @@ "create table output(id serial primary key, tx_id integer references tx(id), "
                  ^ "index int, amount bigint)"


    >> PG.commit db
  )



let log = Lwt_io.write_line Lwt_io.stdout

let coinbase = M.zeros 32


let format_tx hash i value script =
  " i " ^ string_of_int i
  ^ " value " ^ string_of_float ((Int64.to_float value ) /. 100000000.)
  ^ " tx " ^ M.hex_of_string hash
  ^ " script " ^ M.format_script script


type my_script =
  | Some of string
  | None
  | Strange


let fold_m f acc lst =
  let adapt f acc e = acc >>= fun acc -> f acc e in
  L.fold_left (adapt f) (return acc) lst

(*
let map_m f lst =
  let adapt f acc e = acc >> f e in
  L.fold_left (adapt f) (return ()) lst
*)

(*
  we have
    - a tx table having tx hash, block id
    - an output table with tx_id, output_index, address, amount
    - an input table with tx_id, and referencing output table id.

  - then we can join everything. for a tx or address
  - it's append only
  - inputs refer directly to outputs using primary_id of output.
  - no aggregate indexes
  - easy to remove txs.

*)

let process_output x (i,output,hash,tx_id) =
    let script =
    let open M in
    M.decode_script output.script in
    let u = match script with
      (* pay to pubkey *)
      | BYTES s :: OP_CHECKSIG :: [] -> Some (s |> M.sha256 |> M.ripemd160)
      (* pay to pubkey hash*)
      | OP_DUP :: OP_HASH160 :: BYTES s :: OP_EQUALVERIFY :: OP_CHECKSIG :: [] -> Some s
      (* pay to script - 3 *)
      | OP_HASH160 :: BYTES s :: OP_EQUAL :: [] -> Some s
      (* null data *)
      | OP_RETURN :: BYTES _ :: [] -> None
      (* common for embedding raw data, prior to op_return  *)
      | BYTES _ :: [] -> None
      (* N K1 K2 K3 M CHECKMULTISIGVERIFY, addresses? *)
      | (OP_1|OP_2|OP_3) :: _ when List.rev script |> List.hd = OP_CHECKMULTISIG -> None

      | _ -> Strange
    in (
    match u with
      | Some hash160 ->
        begin
          (* we should add the hash160..., and pubkey etc if available... *)
          return ()
        end
      | Strange ->
          log @@ "strange " ^ format_tx hash i output.value script
      | None ->
        return ()
    )
    (* we need to rearrange to encode the hash160 as PG option type *)
    >>
    PG.prepare x.db ~query:"insert into output(tx_id,index,amount) values ($1,$2,$3)" ()
    >> PG.execute x.db  ~params:[
        Some (PG.string_of_int tx_id);
        Some (PG.string_of_int i);
        Some (PG.string_of_int64 output.value )
      ] ()
    >> return x



let process_input x (i, (input : M.tx_in ), hash,tx_id) =
    (* extract der signature and r,s keys *)
    let script = M.decode_script input.script in

    (* why can't we pattern match on string here ? eg. function *)

    (* so we have to look up the tx hash, which means we need an index on it *)
    if input.previous = coinbase then
      return x
    else

(* i think the outputs are not getting recorded??? 
 
  or we are overwriting them?
  or we're counting them incorrectly, with coinbase 

  this tx has two outputs but it doesn't find second
    and in fact it doesn't seem to exist 
  https://blockchain.info/tx/f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16

ok we have them, so there
meteo=> select * from output where tx_id = 171
meteo-> ;
 id  | tx_id | index |   amount   
-----+-------+-------+------------
 171 |   171 |     0 | 1000000000
 172 |   171 |     1 | 4000000000
ok, it appears correct, so why do our joins not work...  

   id  |                                hash                                
-----+--------------------------------------------------------------------
 171 | \xf4184fc596403

*)

        (*PG.prepare x.db ~query:"select output.id from output join tx on tx.id = output.id where tx.hash = $1 and output.index = $2" () *)
        PG.prepare x.db ~query:"select output.id from output join tx on tx.id = output.tx_id where tx.hash = $1 and output.index = $2" ()
      >> PG.execute x.db  ~params:[
        Some (PG.string_of_bytea input.previous);
        Some (PG.string_of_int input.index); ] ()
      >>= fun rows ->
        let tx_id = match rows with
          (Some field ::_ )::_ -> PG.int_of_string field 
          | _ -> raise (Failure "previous tx not found") 
        in
        log @@ "found " ^ string_of_int tx_id ^ " " ^ M.hex_of_string input.previous ^ " " ^ string_of_int input.index
       
    >>
      return x
(*
      let key = (input.previous,input.index) in
      match Utxos.mem key x.unspent with
        | true -> return x
        | false -> raise ( Failure "ughh here" )
*)

let process_tx x (hash,tx) =
  begin
    match x.tx_count mod 10000 with
      | 0 -> log @@ S.concat "" [
        " tx_count "; string_of_int x.tx_count;
      ] >>
          log "comitting"
        >> PG.commit x.db
        >> log "done comitting, starting new transction"
        >> PG.begin_work x.db
      | _ -> return ()
  end
  >>
    let x = { x with tx_count = succ x.tx_count } in

    PG.prepare x.db ~query:"insert into tx(hash) values ($1) returning id" ()
  >> PG.execute x.db  ~params:[ Some (PG.string_of_bytea hash); ] ()
  >>= fun rows ->
    let tx_id = match rows with
      (Some field ::_ )::_ -> PG.int_of_string field
      | _ -> raise (Failure "uggh")
    in

    (* can get rid of the hash *)
    let group index a = (index,a,hash,tx_id) in

    let open M in

    let inputs = L.mapi group tx.inputs in
    fold_m process_input x inputs
  >>= fun x ->
    let outputs = L.mapi group tx.outputs in
    fold_m process_output x outputs





let process_file () =

    log "connecting and create db"

    >>
    PG.connect ~host:"127.0.0.1" ~database: "meteo" ~user:"meteo" ~password:"meteo" ()
    >>= fun db ->
        create_db db
    >>
      Lwt_unix.openfile "blocks.dat.orig" [O_RDONLY] 0
    >>= fun fd ->
      log "scanning blocks..."
    >> Sc.scan_blocks fd
    >>= fun headers ->
      log "done scanning blocks - getting leaves"
    >>
      let leaves = Sc.get_leaves headers in
      log @@ "leaves " ^ (leaves |> L.length |> string_of_int)
    >>
      let longest = Sc.get_longest_path leaves headers in
      log @@ "longest " ^ M.hex_of_string longest
    >>
      log "computed leaves work "
    >>
      let seq = Sc.get_sequence longest headers in
      let seq = CL.drop seq 1 in (* we are missng the first block *)
      let seq = CL.take seq 200000 in
      (* let seq = [ M.string_of_hex "00000000000004ff6bc3ce1c1cb66a363760bb40889636d2c82eba201f058d79" ] in *)


(*          let query = "insert into tx(hash,index,amount) values ($1,$2,$3)" in
          PG.prepare db ~name:"myinsert" ~query  ()
      >>
*)
          PG.begin_work db
      >>

      let x = {
        tx_count = 0;
        db = db;
      } in
      Sc.replay_tx fd seq headers process_tx x
    >>
      log "finished "


(*
  insert into tx(hash,index,amount) values (E'\\x0000',0,200);
*)
let () = Lwt_main.run (
  Lwt.catch (
(*
    fun () ->
    PG.connect ~host:"127.0.0.1" ~database: "meteo" ~user:"meteo" ~password:"meteo" ()
    >>= fun db -> create_db db
    >> return ()
*)

    process_file

  )
  (fun exn ->
    (* must close *)
    let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
    log ("finishing - exception " ^ s )
    >> return ()
  )
)



