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

module M = Message
(* module Sc = Scanner *)


module PG = Misc.PG

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

      ("insert_blockdata", "
          insert into blockdata(data)
          values ($1)
          returning id
      ");

      ("insert_block", "
          insert into block(hash,previous_id,time, blockdata_id)
          select
              $1,
              b.id,
              to_timestamp($3) at time zone 'UTC',
              $4
          from block b
          where hash = $2 and b.id is not null
          returning id
      ");

      (* TODO maybe remove this *)
      ("insert_genesis_block", "insert into block(hash) select $1" );

      ("insert_tx", "insert into tx(block_id,hash, pos, len) values ($1, $2, $3, $4) returning id"  );

      ("select_output_id", "select output.id from output join tx on tx.id = output.tx_id where tx.hash = $1 and output.index = $2"  );
      ("insert_output", "insert into output(tx_id,index,amount) values ($1,$2,$3) returning id" );
      ("insert_input", "insert into input(tx_id,output_id) values ($1,$2) returning id" );

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
      ("insert_signature", "insert into signature(input_id,r,s) values ($1,$2,$3)"  );

      ("can_insert_block", "
          -- hash, previous hash
          select not exists(
            select * from block where hash = $1
          )
          and exists(
           select * from block where hash = $2
          )
      "  );
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
        Some (string_of_int64 output.value)
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
        (* why don't we record strange... because it's not an address should be in another table *)
        log @@ "strange " ^ format_tx tx_hash index output.value script
          >> return x
      | None ->
        return x



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
    let r,s = der in
    PG.execute x.db ~name:"insert_signature" ~params:[
      Some (PG.string_of_int input_id);
      Some (PG.string_of_bytea r);
      Some (PG.string_of_bytea s);
    ] ()
    >>
    return x
  in
  fold_m process_der x ders


let process_input x (index, input, hash, tx_id) =

  let (input : M.tx_in) = input in
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
      ] ()
    >>= fun rows ->
      let input_id = decode_id rows in
      process_input_script x (input_id,input)


let process_tx x (block_id,hash,tx) =
  let (tx : M.tx) = tx in
  PG.execute x.db ~name:"insert_tx"  ~params:[
      Some (PG.string_of_int block_id);
      Some (PG.string_of_bytea hash);
      Some (PG.string_of_int tx.pos);
      Some (PG.string_of_int tx.length);
    ] ()
  >>= fun rows ->
    let tx_id = decode_id rows in
    (* can get rid of the hash *)
    let group index a = (index,a,hash,tx_id) in
    let open M in
    let inputs = L.mapi group tx.inputs in
    fold_m process_input x inputs
  >>= fun x ->
    let outputs = L.mapi group tx.outputs in
    fold_m process_output x outputs

(*
  - IMPORTANT should move the transaction isolation begin and commit outside the process block
    so that if want to do other actions - like check if block has been inserted in the same
    tx we can.
*)

(*
  either locking. it can't seem to begin work ? because nested too deep. or not closing some other tx? 

  begin writing db
  insert_block 000000001fb56bef722327ef1b56442c6ee5f1989d0a8467b981a03ed6d4938e
  23.229.45.32:8333   *** got inventory blocks 1 - on request 1
  request addr 50.199.113.193
  blocks on request 1
  fds

*)
let process_block x payload =
(*
  let x = { x with block_count = succ x.block_count } in
    begin
      (* todo move commits to co-incide with blocks *)
      match x.block_count mod 10000 with
        | 0 -> log @@ " block_count " ^ string_of_int x.block_count;
        | _ -> return ()
    end
  >>
u
*)  



  let _, block  = M.decodeBlock payload 0 in
    let hash = M.decode_block_hash payload in
    log @@ "insert_block " ^ M.hex_of_string hash
  >> PG.begin_work x.db

  >>  log  "after begin work " 

  >> PG.( execute x.db ~name:"can_insert_block" ~params:[
      Some (string_of_bytea hash);
      Some (string_of_bytea block.previous)
    ] ()
    )
    >>= begin
      function
      | (Some "t" ::_ )::_ ->
        begin
          log "can insert " 

          >> PG.execute x.db ~name:"insert_blockdata" ~params:[
            Some (PG.string_of_bytea payload );
          ] ()
          >>= fun rows ->
            let blockdata_id = decode_id rows in

          PG.execute x.db ~name:"insert_block" ~params:[
            Some (PG.string_of_bytea hash );
            Some (PG.string_of_bytea block.previous );
            Some (PG.string_of_int block.nTime );
            Some (PG.string_of_int blockdata_id );
          ] ()
          >>= fun rows ->
            log "done insert_block"
          >>
            let block_id = decode_id rows in
            let txs = M.decode_block_txs payload in
            let txs = L.map (fun (tx : M.tx) ->
              block_id,
              M.strsub payload tx.pos tx.length |> M.sha256d |> M.strrev,
              tx
            ) txs
            in
            fold_m process_tx x txs
          end

      | (Some "f" ::_ )::_ ->
        begin
          log "cannot insert"
          >> return x
        end 
      | _ -> begin
          log "unknown"
          >> return x
        end 
    end
  >>= fun x ->
    PG.commit x.db
  >> return x


