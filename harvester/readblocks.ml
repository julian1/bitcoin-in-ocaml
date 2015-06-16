
(* scan blocks and compute tx indexes
  native.
  corebuild -I src -package leveldb,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax  harvester/readblocks.native
*)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)  (* like bind, but second arg return type is non-monadic *)
let return = Lwt.return

module M = Message
module L = List
module CL = Core.Core_list
module S = String

open M 
(*
type tx_out = M.tx_out
type tx = M.tx
*)



(*
  - timings 200,000 blocks
    4m 10 for hash,i  set
    4m 41 for i,hash  set
    4m 13 for hash,i  map

    1m 12 without set or map!!

    4m 21 recording r_values

  height 200000 tx_count 7316307 output_count 16629435 unspent 2318465

  - native goes through in 12 minutes - including output script decoding.

  - we need to factor into a module, analysis and the action scanning
  - outputting value, so we can get a sense of how much value is
    being sent, in what period of time, and if it's worthwhile trying to compete...

  - lets output the value of the previous...
    by looking up the matching output tx details script ...
      - but will have to store all tx's with their values.
      - actually we'd be more interested in the amounuggh.

  -----
    - need to index the tx, maybe output
    - index the rvalue.

    - OK, now hang on if we want to use db to do this stuff...
    - we can't store the tx_out output script ... or can we? 
  
    -- hmmmm reading the full tx, to get the output is going to be slow????
  
    -- it's getting more and more complicated. we really want to factor this...  

    -- scanner

*)


module Utxos = Map.Make(struct type t = string * int let compare = compare end)
(* module Utxos = Set.Make(struct type t = string * int let compare = compare end) *)

 
module RValues = Map.Make(struct type t = string let compare = compare end)


type mytype =
{
  unspent : tx_out Utxos.t;
  tx_count : int;
  output_count : int;

  r_values : string RValues.t;

  db :  Db.t ;  (* db of hashes, maybe change name *)
}


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


let process_output x (i,output,hash) =
  x  >>= fun x ->
(**)
    let script = M.decode_script output.script in
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

      (* N K1 K2 K3 M CHECKMULTISIGVERIFY *)
      | (OP_1|OP_2|OP_3) :: _ when List.rev script |> List.hd = OP_CHECKMULTISIG -> None

      | _ -> Strange
    in (
    match u with
      | Some hash160 ->
        begin
(*
          Db.get x.db hash160
          >>= function
            | Some found ->
              log @@ "found hash160 " ^ M.hex_of_string hash160 ^ " " ^ found
                ^ " " ^ format_tx hash i output.value script
            | _ ->
              (*log @@ "not found "
              >>*) return ()
*)
          return ()
        end
      | Strange ->
          log @@ "strange " ^ format_tx hash i output.value script
      | None ->
        return ()
    )
    >>
    return { x with
        output_count = succ x.output_count ;
        unspent =
          let key = (hash,i) in
          Utxos.add key output x.unspent
    }


(* ok, if we scan and index the blocks, then we can select a block for testing
  rather than indexing txoutpus . might be interesting to index 160 addresses
  address -> [ txs ] that reference it.
  can then get the value very easily
  -
  i think we do want the map for utxos.
    can record the actual output script etc, or hash160, etc.
*)

(*
  so we record r values...
    
  - all the tx's will be spent.
    what we want is to find any other uses of the pubkey/ address that are unspent..

    ... hmmmn
  ok, we normally have
    SIG PUBKEY

  which means we have the pubkey. without referencing the previous tx. in fact i 

  - so it's easy to reference with what's been spent.
  - remember what we want is indication how often... so whether we run a process
  - it's only if the address is reused.

  - want to find out when and if recent.
 
  - all founds are repitions.  
  - it means we can front run if fast enough. 
*)

let process_input x (i, (input : M.tx_in ), hash) =
  x >>= fun x -> 
    (* extract der signature and r,s keys *)
    let script = M.decode_script input.script in
    let ders = L.fold_left (fun acc elt ->
        match elt with
          | BYTES s -> ( 
              match M.decode_der_signature s with  
                Some der -> der :: acc
                | None -> acc
          )
          | _ -> acc
    ) [] script
    (* lookup *) 
    in
      let f x der = 
        x >>= fun x -> 
        let r,s = der in
        match RValues.mem r x.r_values with 
          true -> begin 
            log @@ "found !!! " ^ 
              " tx " ^ M.hex_of_string hash ^ 
              " previous " ^ M.hex_of_string input.previous ^ 
              " index " ^ string_of_int i ^ 
              " r_value " ^ M.hex_of_string r 
            >> 
            (* ok, if we want the output value, then we have to store it *)
            let key = (input.previous,input.index) in
            match Utxos.mem key x.unspent with
              | true -> 
                  let output = Utxos.find key x.unspent in 
                  let value = output.value in
                  log @@ " value " ^ string_of_float ((Int64.to_float value ) /. 100000000.)
                  >> 
                  return x
              | false -> raise ( Failure "ughh here" )
            end
          | false -> 
            return { x with r_values = RValues.add r "mytx" x.r_values }
    in
    L.fold_left f (return x) ders
    >|= fun x ->
    (* why can't we pattern match here ? eg. function *)
    if input.previous = coinbase then
      x
    else
      let key = (input.previous,input.index) in
      match Utxos.mem key x.unspent with
        | true -> { x with unspent = Utxos.remove key x.unspent }
        | false -> raise ( Failure "ughh here" )

(*
    >>= fun x -> 
    log @@ "tx " ^ M.hex_of_string hash ^ 
      " previous " ^ M.hex_of_string input.previous ^ 
      " index " ^ string_of_int input.index ^ 
      " script " ^ M.format_script script ^ 
      " ders " ^ string_of_int (L.length ders) 
*)

(* let u = match script with *)



(* process tx by processing inputs then outputs *)

let process_tx x (hash,tx) =
  begin
    match x.tx_count mod 100000 with
      | 0 -> log @@ S.concat "" [
        " tx_count "; string_of_int x.tx_count;
          " output_count "; string_of_int x.output_count;
          " unspent "; x.unspent |> Utxos.cardinal |> string_of_int;
          " rvalues "; x.r_values |> RValues.cardinal |> string_of_int
      ] 
      | _ -> return ()
  end
    >>

  let x = { x with tx_count = succ x.tx_count } in
(*log "tx" >> *)
  let group index a = (index,a,hash) in

  let inputs = L.mapi group tx.inputs in
  L.fold_left process_input (return x) inputs
>>= fun x ->
  let outputs = L.mapi group tx.outputs in 
  L.fold_left process_output (return x) outputs


module Sc = Scanner



let process_file () =
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
      (* let leaves_work = L.map (fun hash -> (hash, get_pow2 hash headers )) leaves in *)
      log "computed leaves work "
    >>
      let seq = Sc.get_sequence longest headers in
      let seq = CL.drop seq 1 in (* we are missng the first block *)
      let seq = CL.take seq 200000 in 
      (* let seq = [ M.string_of_hex "00000000000004ff6bc3ce1c1cb66a363760bb40889636d2c82eba201f058d79" ] in *)

      Db.open_db "myhashes"
    >>= fun db ->
      log "opened myhashes db"
    >>
      let x = { 
        unspent = Utxos.empty; 
        db = db; 
        tx_count = 0; 
        r_values = RValues.empty; 
        output_count = 0; 
      } in
(*
      let process_block = process_block process_tx in
      Sc.replay_blocks fd seq headers process_block x
*)

      Sc.replay_tx fd seq headers process_tx x 

    >>
      log "finished "



let () = Lwt_main.run (
  Lwt.catch (
    process_file
  )
  (fun exn ->
    (* must close *)
    let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
    log ("finishing - exception " ^ s )
    >> return ()
  )
)



(* note we could even store block hash if we wanted

  - basically nested folds
  - it's better to pass x as monadic argument since allows folds without complication.
  - x can be any structure that we want to record stuff - a record, or () if nothing.
      or a db, or combination.
  - eg. if we want to keep a block count it should be on that structure

  - this could be made more generalizable, but not sure it would make it code simpler and easier.
    just repeat for different context.
    - may even want to remove the partial application functions - and call things directly.
  --------

  so how do we do this?
    txhash / index -> s or u

  what about amounts?
    block <- output <- address

  - ok, we want to return 0w
  - hmmmmm we don't actually need to pass the mydb through the fold...
---------

  - OK, all we have to do is change the db, from address to hash160.
  - and open, and look values up
*)





(*

  - ok, we've got the utxo set being calculated. but what about
  1. extract the addresses
  2. look them up.

  can return the list of address -> tx
    use
        tx -> address - and use the existing struccture
            then only remove if not also found..

  - it doesn seem to slow more than would expect
  - think the int should be first

    PROBLEM
        tx's that are spent in same block - are ones we are
        really interested in. because they're auto harvested

  -------

    - ok there's an issue, that a fork block spends txs, then another
    block tries to do the same.
    - we have to pick a path through the blocks.


*)
(*
let process_output index output =
  log @@  string_of_int index ^ " " ^  string_of_int (Int64.to_int output.value )
  >> log @@ "\n  script: " ^ (output.script |> decode_script |> M.format_script )



let sequence f initial lst  =
  L.fold_left (fun acc x -> acc >> f x) initial lst

let sequencei f initial lst  =
  let ret,_ = L.fold_left (fun (acc,i) x -> (acc >> f i x, succ i)) (initial,0) lst in
  ret
*)
(*
  (* these are reversed, - should do inputs then outputs *)
    let x = L.fold_left process_input x tx.inputs in
    let m = L.mapi (fun i output -> (i,output,hash)) tx.outputs in
    L.fold_left process_output x m
*)
(*
  log @@ M.hex_of_string hash
  (* we should probably sequence, not parallelise this *)
  (* >>  Lwt.join ( L.mapi process_output tx.outputs ) *)
  (* >>  Lwt.join ( L.mapi process_output tx.outputs ) *)
  >> sequencei process_output (return ()) tx.outputs
*)


(* issue passing a structure through a series of functions i
  use a module?
  everything is a fold
*)

(*
let process_tx db block_pos ((hash, tx) : string * M.tx )  =
  let coinbase = M.zeros 32
  in
  let process_input (input : M.tx_in) =
    if input.previous = coinbase then
      return ()
    else
      let key = I.encodeKey { hash = input.previous; index = input.index  } in
      Db.get db key
      >>= (fun result ->
        match result with
          Some s ->
            (* we should write functions to do this *)
            let value = I.decodeValue s in
            if value.status <> "u" then
              let msg = "ooops tx is spent" in
              raise (Failure msg)
            else
              Db.put db key (I.encodeValue { value with status = "s";  } )
          | None ->
            let msg = "txo not found " ^ M.hex_of_string input.previous ^ " " ^ string_of_int input.index in
            raise (Failure msg)
      )
  in
  let process_output tx_hash index (output : M.tx_out) =
    let key = I.encodeKey { hash = tx_hash; index = index } in
    let value = I.encodeValue {
        status = "u"; block_pos = block_pos; tx_pos = tx.pos; tx_length = tx.length;
        output_pos = output.pos; output_length = output.length;
      } in
    Db.put db key value
  in
  (* process in parallel inputs, then outputs in sequence *)
  Lwt.join ( L.map process_input tx.inputs )
  >> Lwt.join ( L.mapi (process_output hash) tx.outputs )
*)

(* this thing doesn't use the db, so it should be configured ...  all this stuff is still mucky *)

(*    log @@ "output " ^ (M.format_script script)
    >> *)
    (* we don't have to decode the script type just the length - another fold
      ok, we want hash 160,,, how do we output the value....  *)
(*
    let u = L.fold_left (fun acc e ->
      match e with
        BYTES s when S.length s = 40 -> acc
        (* BYTES s -> s :: acc *)
        | _ -> acc
      ) [] script
    in
    Either Good
    No just return Some or None
*)
(* can we factor this - out of the io? - rather than calculate values, we
should have functions
  really not sure that we need height information. in can be calculated at any time,
  which makes it dynamic...
*)

(*
let get_height hash headers =
  match HM.mem hash headers with
    | true -> (HM.find hash headers).height
    | false -> 0

let get_pow hash headers =
  let rec get_pow' hash =
    match HM.mem hash headers with
      | true ->
        let previous = (HM.find hash headers).previous in
        1 + (get_pow' previous)
      | false -> 0
  in get_pow' hash
*)

(*
  ughhh - this is complicated - we basically have to go through the
  complete list
  we have to get to genesis/root before we can mark anything
  working with two sets....
  compute heights...

let get_height hash headers =
  let rec get_list hash lst =
    match HM.mem hash headers with
      | true ->
        let previous = (HM.find hash headers).previous in
        let lst = get_list previous lst in
        lst
      | false -> lst
  in
  get_list hash []
    max height 354567 longest 0000000000000000106904f8bb02831df9c16689f2208a38e1bbdf08811b0fd9
    max height 354323 longest 00000000000000000f054cbca49e12841e73d8c6709b9b5bdfe1883a9255e98f
*)

(*
let process_blocks f fd x =
	let rec process_blocks' x =
    x >>= fun x ->
      Misc.read_bytes fd 24
      >>= function
        | None -> return x
        | Some s ->
          (* Lwt_unix.lseek fd 0 SEEK_CUR
          >>= fun pos -> *)
            let _, header = M.decodeHeader s 0 in
            (* log @@ header.command ^ " " ^ string_of_int header.length >> *)
            Misc.read_bytes fd header.length
          >>= function
            | None -> return x
            | Some payload ->
              f (return x) payload
              >>= fun x -> process_blocks' (return x)
  in process_blocks' x


let process_file () =
    Lwt_unix.openfile "blocks.dat.orig" [O_RDONLY] 0
    >>= fun fd ->
      log "opened blocks..."
    >>
      Db.open_db "myhashes"
    >>= fun db ->
      log "opened myhashes db"
    >>
      let process_block = process_block process_tx in
      let x = { unspent = UtxoMap.empty; db = db; } in
      process_blocks process_block fd (return x)
    >>= fun x ->
      Lwt_unix.close fd
      >> log @@ "final " ^ (string_of_int (UtxoMap.cardinal x.unspent) )
*)
(*
  If we're going to build up a datastructure , then can test that for count
*)

