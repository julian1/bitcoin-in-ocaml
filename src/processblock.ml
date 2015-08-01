(*
  - it should be easy to stop. and resume this stuff as well, if we want.
  - should test whether have block already and skip...
------
    - very important the whole chain/heads structure could be put in db.
    - the issue of blocks coming in fast out-of-order sequence, can be handled
    easily by just always pushing them in the sequential processing queue.
    - when we have block.previous_id then it should be very simple to work
    out the heads.
*)
(* scan blocks and store to db

corebuild -I src -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax harvester/readblocks2.native

  Need to get rid of leveldb ref, coming from misc.ml 126

*)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)  (* like bind, but second arg return type is non-monadic *)
let return = Lwt.return

module L = List
module CL = Core.Core_list
module S = String
module U = Util
module PG = U.PG

(* module Sc = Scanner *)
module M = Message

(*  - as well as fold_m should have takeWhile ...
*)
let fold_m f acc lst =
  let adapt f acc e = acc >>= fun acc -> f acc e in
  L.fold_left (adapt f) (return acc) lst

(*
let map_m f lst =
  let adapt f acc e = acc >> f e in
  L.fold_left (adapt f) (return ()) lst
*)


(* TODO get rid of this and just pass the db ref only 
    else only use this in this module ...
*)
type mytype =
{
  block_count : int;
  db : int PG.t ; (* TODO what is this *)
}


let log = Lwt_io.write_line Lwt_io.stdout

let decode_id rows =
  (* >>= fun rows -> *)
  (* TODO just use function *)
  match rows with
    (Some field ::_ )::_ -> PG.int_of_string field
    | _ -> raise (Failure "previous tx not found")


let coinbase = M.zeros 32


let create_prepared_stmts db =
    (* note we're already doing a lot more than with leveldb

        - address hashes are not normalized here. doesn't really matter
        - likewise for der values
        - pubkey - and der.  after der, then we can start writing views.
        - and some views to make joining stuff easier.
        - we could normalize address
        - and a flag on block if it's valid chain sequence
    *)
    (*
      advantages,
      - transactional around blocks.
      - we can join everything. for a tx or address, sum etc.
      - it's append only
      - inputs refer directly to outputs using primary_id of output.
      - no aggregate indexes
      - easy to remove txs.
    *)
    (*
        - we ought to be able to do chainstate rearrangement really simply... just
        with a flag against the block. or another table, to say whether it's mainchain
        then adjust the views accordingly.
        - actually could even do it, as a single tip value... but probably easier to
        mark.
        - we need to get the db transactions organized around block. rather than tx_count
    *)
    (*
      - if we stored the blocks in db. then could use substr
        - get blocks by hash
        - get tx by hash
    *)
    (*
      - ok, do we pull the db creation out??? probably should - to support migrations.
      - we're going to have a bunch of views etc...
    *)
  PG.(
    begin_work db
    >> fold_m (fun db query -> inject db query >> return db ) db [
    ]

    >>= fun db ->  fold_m (fun db (name,query) -> prepare db ~name ~query () >> return db ) db [

      (* $1 hash, $2 previous hash, $3 time *)
      ("insert_block", "
          insert into block(hash,time, height)
          select $1, to_timestamp($3) at time zone 'UTC', (select height+1 from block b where b.hash = $2) 
          returning id
      ");
 
      ("insert_previous", "
          insert into previous(block_id, block_previous_id)
          select $1, block.id from block where hash = $2
          returning id
      ");

      ("insert_block_data", "
          insert into block_data(block_id,data)
          values ($1, $2)
          returning id
      ");

      (* TODO maybe remove this *)
      ("insert_genesis_block", "insert into block(hash) select $1" );

      ("select_tx_id", "select tx.id from tx where tx.hash = $1"  );

      ("insert_tx", "insert into tx( hash) values ($1) returning id");

      ("insert_tx_block", "insert into tx_block( tx_id, block_id, pos, length ) values ($1, $2, $3, $4) returning id");


      ("select_output_id", "select output.id from output join tx on tx.id = output.tx_id where tx.hash = $1 and output.index = $2"  );

      ("insert_output", "insert into output(tx_id,index,amount, pos, length) values ($1,$2,$3,$4,$5) returning id" );

      ("insert_input", "insert into input(tx_id,output_id, pos, length) values ($1,$2,$3,$4) returning id" );

      (* TODO this is too complicated - do it client side *)
      ("insert_address", "
          with s as (
              select id, hash, script
              from address
              where hash = $1
              and script = $2
          ), i as (
              insert into address (hash, script)
              select $1, $2
              where not exists (select 1 from s)
              returning id, hash, script
          )
          select id, hash, script
          from i
          union all
          select id, hash, script
          from s
        "  );
      ("insert_output_address", "insert into output_address(output_id,address_id) values ($1,$2)"  );
      ("insert_coinbase", "insert into coinbase(tx_id) values ($1)"  );
      ("insert_signature", "insert into signature(input_id,r,s, sig_type) values ($1,$2,$3,$4)"  );

      (* TODO change name to extends_chain? *)
      ("can_insert_block", "
          -- hash, previous hash
          select not exists(
            select * from block where hash = $1
          )
          and exists(
            select * from block where hash = $2
          )
      ");
    ]
    >> commit db
  )


let format_tx hash i value script =
  " i " ^ string_of_int i
  ^ " value " ^ string_of_float ((Int64.to_float value ) /. 100000000.)
  ^ " tx " ^ M.hex_of_string hash
  ^ " script " ^ M.format_script script


type my_script =
  | None
  | Strange
  | P2PK of string
  | P2PKH of string
  | P2SH of string
  (* | P2MS p2ms MULTI of string *)



let process_output x (index,output,tx_hash,tx_id) =
  (* TODO should get rid of tx_hash argument used for loging strange *)
  let open M in
  PG.( execute x.db ~name:"insert_output" ~params:[
      Some (string_of_int tx_id);
      Some (string_of_int index);
      Some (string_of_int64 output.value);
      Some (PG.string_of_int output.pos);
      Some (PG.string_of_int output.length);
    ] () )
  >>= fun rows ->
    let output_id = decode_id rows in
    let script = M.decode_script output.script in
    let decoded_script = match script with
      | BYTES s :: OP_CHECKSIG :: [] -> P2PK s  (* could do the hashing later *)
      | OP_DUP :: OP_HASH160 :: BYTES s :: OP_EQUALVERIFY :: OP_CHECKSIG :: [] -> P2PKH s
      | OP_HASH160 :: BYTES s :: OP_EQUAL :: [] -> P2SH s
      (* null data *)
      | OP_RETURN :: BYTES _ :: [] -> None
      (* seems common for embedding raw data, prior to op_return *)
      | BYTES _ :: [] -> None
      (* N K1 K2 K3 M CHECKMULTISIGVERIFY, addresses? TODO should record all these make generic  *)
      | (OP_1|OP_2|OP_3) :: _ when List.rev script |> List.hd = OP_CHECKMULTISIG -> None
      | _ -> Strange
    in
    let insert hash160 script =
      PG.( 
        execute x.db ~name:"insert_address" ~params:[
          Some (string_of_bytea hash160);
          Some script
        ] ()
      >>= fun rows ->
        let address_id = decode_id rows in
        execute x.db ~name:"insert_output_address" ~params:[
          Some (string_of_int output_id);
          Some (string_of_int address_id);
        ] ()
      )
    in
    match decoded_script with
      | P2PK pk -> insert (pk |> M.sha256 |> M.ripemd160) "p2pk" >> return x
      | P2PKH hash160 -> insert hash160 "p2pkh" >> return x
      | P2SH hash160 -> insert hash160 "p2sh" >> return x
      | Strange ->
        (* TODO we should record strange... but in another table *)
        log @@ "strange " ^ format_tx tx_hash index output.value script
          >> return x
      | None ->
        return x

(*
  important - this is recording the redeeming signature for (non-script) multisig. 
    it's only the hash160 output we are not recording. 

  - we should be careful to do a left-join on address however to avoid excluding
    rows with sigs, but no address 
*)

let process_input_script x (input_id, input) =
  let (input : M.tx_in) = input in
  (* maybe change name to process_signature *)
  let script = M.decode_script input.script in
  (* extract der signature and r,s keys *)
  let ders = L.fold_left (fun acc elt ->
    match elt with
      | M.BYTES s -> (
        match M.decode_der_signature s with
          Some der -> der :: acc
          | None -> acc
      )
      | _ -> acc
  ) [] script in

  let process_der x der =
    (* ok, all we have to do is insert the der ...  *)
    (* TODO we should be storing and indexing the sigType 

      TODO actually should we be just storing the pos,length, and then creating an index on it?
    *)
    let r,s,sig_type = der in
    PG.execute x.db ~name:"insert_signature" ~params:[
      Some (PG.string_of_int input_id);
      Some (PG.string_of_bytea r);
      Some (PG.string_of_bytea s);
      Some (PG.string_of_int sig_type);
    ] ()
    >>
    return x
  in
  fold_m process_der x ders


let process_input x (index, input, hash, tx_id) =

  let (input : M.tx_in) = input in

    log @@ "input " ^ M.hex_of_string input.previous ^ " " ^ string_of_int input.index 
  >>



  (* let process_input x (i, input, hash,tx_id) = *)
  (* why can't we pattern match on string here ? eg. function *)
  (* so we have to look up the tx hash, which means we need an index on it *)
  if input.previous = coinbase then
    PG.execute x.db ~name:"insert_coinbase" ~params:[
      Some (PG.string_of_int tx_id);
    ] ()
    >> return x
  else
    PG.execute x.db ~name:"select_output_id" ~params:[
        Some (PG.string_of_bytea input.previous);
        Some (PG.string_of_int input.index);
      ] ()
    >>= fun rows ->
      let output_id = decode_id rows in
      PG.execute x.db ~name:"insert_input" ~params:[
        Some (PG.string_of_int tx_id);
        Some (PG.string_of_int output_id);
        Some (PG.string_of_int input.pos);
        Some (PG.string_of_int input.length);
      ] ()
    >>= fun rows ->
      let input_id = decode_id rows in
      process_input_script x (input_id,input)


let process_tx x (block_id,hash,tx) =
  let (tx : M.tx) = tx in

    log @@ "tx " ^ M.hex_of_string hash 
  >>


    (* TODO we have to lookup the tx first... rather than just insert and return the tx_id  
      the insertion of the tx, should be done completely before we insert the tx_block ? 
      and preferably outside this function to support mempool stuff...
      - we don't want to return values, since we are passing values in....

      ("select_tx_id", "select tx.id from tx where tx.hash = $1"  );
    *)

    PG.execute x.db ~name:"select_tx_id"  ~params:[
      Some (PG.string_of_bytea hash);
    ] ()

  >>= (function
    | (Some tx_id ::_ )::_ -> 
      return (PG.int_of_string tx_id ) 

    | _ -> (* it's not a null it's going to be empty??? - remember we have the unique constraint on the tx hash *) 
      PG.execute x.db ~name:"insert_tx"  ~params:[
          Some (PG.string_of_bytea hash);
        ] ()
      >>= fun rows ->
        let tx_id = decode_id rows in
      (*  log @@ "inserted tx - id " ^ string_of_int tx_id
      >> *)
        (* TODO can get rid of the hash - except used for reporting errors in later actions *)
        let group index a = (index,a,hash,tx_id) in
        let open M in
        let inputs = L.mapi group tx.inputs in
        fold_m process_input x inputs
      >>= fun x ->
        let outputs = L.mapi group tx.outputs in
        fold_m process_output x outputs
      >> return tx_id
    ) 
  (* now insert tx_block if that was the source *) 
  >>= fun tx_id ->
    PG.execute x.db ~name:"insert_tx_block" ~params:[
      Some (PG.string_of_int tx_id );
      Some (PG.string_of_int block_id );
      Some (PG.string_of_int tx.pos);
      Some (PG.string_of_int tx.length);
    ] ()
  >>= fun rows ->
    (*log "done insert_tx_block"
    >>*) return x 


(*
  ok, to check difficulty - we'll have to take db actions .... height etc.
*)

let process_block (db : int PG.t ) payload =

  let x = {
      block_count = 0;
      db = db;
    }
  in
  let _, block  = M.decodeBlock payload 0 in
    let hash = M.decode_block_hash payload in
    log @@ "begin insert_block " ^ M.hex_of_string hash

  >> PG.begin_work x.db

  >> PG.( execute x.db ~name:"can_insert_block" ~params:[
      Some (string_of_bytea hash);
      Some (string_of_bytea block.previous)
    ] ()
    )
  >>= begin
    function
    | (Some "t" ::_ )::_ ->
        begin
        (* TODO the following three stmts could be wrapped up in one prepared stmt *)
          PG.execute x.db ~name:"insert_block" ~params:[
            Some (PG.string_of_bytea hash);
            Some (PG.string_of_bytea block.previous);  (* required for height *)
            Some (PG.string_of_int block.nTime);
          ] ()
        >>= fun rows ->
          let block_id = decode_id rows in

          log @@ "done insert_block id " ^ string_of_int block_id

        >> PG.execute x.db ~name:"insert_previous" ~params:[
            Some (PG.string_of_int block_id );
            Some (PG.string_of_bytea block.previous );
          ] ()

        >>
          PG.execute x.db ~name:"insert_block_data" ~params:[
            Some (PG.string_of_int block_id );
            Some (PG.string_of_bytea payload );
          ] ()
        >>= fun rows ->
          let txs = M.decode_block_txs payload in
          let txs = L.map (fun (tx : M.tx) ->
            block_id,
            (*M.strsub payload tx.pos tx.length, *)  (* TODO should pass raw payload and do hashing in process_tx *)
            M.strsub payload tx.pos tx.length |> M.sha256d |> M.strrev,
            tx
          ) txs
          in
          (*let process_tx x (block_id,payload,hash,tx) =
              log @@ "here -> " ^ M.hex_of_string hash ^ " " ^ M.hex_of_string payload
              >> 
              return x 
          in *)
          fold_m process_tx  x txs
        end

    | (Some "f" ::_ )::_ ->
      begin
        log "cannot insert"
        >> return x
      end 
(*      | _ -> begin
        log "unknown db response"
        >> return x
      end 
*)
    end
    (* TODO exceptions and rollback..., or will calling commit here cause an exception if invalid?  *)
  >>= fun x ->
    PG.commit x.db

  (* return null here? or the db input arg? *)
  >> return ()


