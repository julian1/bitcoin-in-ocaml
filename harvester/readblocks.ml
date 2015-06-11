
(* scan blocks and compute tx indexes *)

let (>>=) = Lwt.(>>=)
let return = Lwt.return
let (>|=) = Lwt.(>|=)  (* what does this do? *)

module M = Message
module L = List
module S = String
open M



(*
    - no just track unspent, and keys separately.
    - then we output those with keys only, and mark whether unspent.
    - ok, but it means we need to carry io return type...  3o
    - hang on maybe we write each one twice...
    - or if we pass a structure, it could contain
*)

(*
  - change db to do hash160
  - then lookup
*)

(*
    fold_lefti can be done with mapi and then feeding into fold...
    OK. we want to make a key val store
*)
module TXOMap = Map.Make(struct type t = int * string let compare = compare end)

type mytype =
{
  unspent : string TXOMap.t ;
  db :  Db.t ;  (* db of hashes, maybe change name *)

}


let log = Lwt_io.write_line Lwt_io.stdout



let coinbase = M.zeros 32


let process_output x (i,output,hash) =
  x >>= fun x ->
    let script = decode_script output.script in
    let u = match script with
      (* do we need to reverse the 160 or something? *)
      (* pay to pubkey *)
      | Bytes s :: OP_CHECKSIG :: [] -> Some (s |> M.sha256 |> M.ripemd160)
      (* pay to pubkey hash*)
      | OP_DUP :: OP_HASH160 :: Bytes s :: OP_EQUALVERIFY :: OP_CHECKSIG :: [] -> Some s
      (* pay to script - 3 *)
      | OP_HASH160 :: Bytes s :: OP_EQUAL :: [] -> Some s
      (* null data *)
      (* | OP_RETURN :: Bytes s :: [] -> None *)
      | _ -> None
    in (
    match u with
      | Some hash160 ->
        begin
          Db.get x.db hash160
          >>= function
            | Some found ->
              log @@ "found hash160 " ^ M.hex_of_string hash160 ^ " " ^ found
            | _ ->
              (*log @@ "not found "
              >>*) return ()
        end
      | None ->
        let msg =
          "tx " ^ M.hex_of_string hash
          ^ " i " ^ string_of_int i
          ^ " value " ^ string_of_float ((Int64.to_float output.value ) /. 100000000.)
          ^ " " ^ M.format_script script in
        log @@ "error " ^ msg
    )
    >>
      return { x with unspent = TXOMap.add (i,hash) "u" x.unspent }

(* ok, if we scan and index the blocks, then we can select a block for testing *)



let process_input x input =
  x >>= fun x ->
 (*   log @@ "input  " ^ M.hex_of_string input.previous
      ^ " index " ^ (string_of_int input.index )
  >>
*)
    if input.previous = coinbase then
      return x
    else
      let key = (input.index,input.previous) in
      match TXOMap.mem key x.unspent with
        | true -> return x (* (TXOMap.remove key x ) *)
        | false -> raise ( Failure "ughh here" )


let process_tx x (hash,tx) =
  x >>= fun x ->
    (*log "tx"
  >> *)
    L.fold_left process_input (return x) tx.inputs
  >>= fun x ->
    let group i output = (i,output,hash) in
    let outputs = L.mapi group tx.outputs in
    L.fold_left process_output (return x) outputs


let process_block f x payload =
  (*log "block"
  >> *)
    (* let block_hash = M.strsub payload 0 80 |> M.sha256d |> M.strrev in *)
    (* decode tx's and get tx hash *)
    let pos = 80 in
    let pos, tx_count = M.decodeVarInt payload pos in
    let _, txs = M.decodeNItems payload pos M.decodeTx tx_count in
    let txs = L.map (fun tx ->
      let hash = M.strsub payload tx.pos tx.length |> M.sha256d |> M.strrev
      in hash, tx
    ) txs
    in
    L.fold_left f (x) txs


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
      let x = { unspent = TXOMap.empty; db = db; } in
      process_blocks process_block fd (return x)
    >>= fun x ->
      Lwt_unix.close fd
      >> log @@ "final " ^ (string_of_int (TXOMap.cardinal x.unspent) )

(*
  If we're going to build up a datastructure , then can test that for count
*)


(* module SS = Map.Make(struct type t = string let compare = compare end) *)
module SS = Map.Make( String )

module SSS = Set.Make(String);;


type my_header =
{
    previous : string;
    pos : int;
    height : int;
}

(* so it's not fast
  when we used to be able to scan over it very quickly...
  - it is the datastructure not the io...
  - takes 1minute to loop the blocks...

  - no it's just the fricken cardinal/ordinal which is not efficient
  - change name to index blocks?

  - IMPORTANT - it's arguable height should be computed dynamically ...
              and only when required. that would enable out-of-order blocks
              to be calculated.
  -
*)


(* get the tree tip hashes as a list *)
let get_tips headers = 
    (* create set of all previous hashes *)
    let f key header acc = SSS.add header.previous acc in
    let previous = SS.fold f headers SSS.empty in 
    (* tips are headers that are not pointed at by any other header *)
    SS.filter (fun hash _ -> not @@ SSS.mem hash previous ) headers
    |> SS.bindings
    |> L.rev_map (fun (tip,_) -> tip)



(* trace sequence back to genesis and return hashes as a list *)
let get_sequence hash headers =
  let rec get_list hash lst =
    let lst = hash :: lst in
    match SS.mem hash headers with
      | true -> 
        let previous = (SS.find hash headers).previous in
        get_list previous lst
      | false -> lst
  in 
  get_list hash [] 


(* assuming calculate as we go 
  - probably should be able to calulate difficulty dynamically  *)
let get_height hash headers =
  match SS.mem hash headers with
    | true -> (SS.find hash headers).height
    | false -> 0



(* scan blocks for headers and record *)
let scan_blocks fd =
  let rec loop_blocks heads count =
    (
    Lwt_unix.lseek fd 0 SEEK_CUR
    >>= fun pos -> Misc.read_bytes fd (24 + 80)
    >>= function
      | Some s -> (
        let _, msg_header = M.decodeHeader s 0 in
        let block_hash = M.strsub s 24 80 |> M.sha256d |> M.strrev in
        let _, block_header = decodeBlock s 24 in
        let previous = block_header.previous in
        let height = (get_height previous heads)  + 1 in
        let heads = SS.add block_hash { previous = previous; height = height; pos = pos + 24} heads in
        ( match count mod 10000 with
          0 -> log @@ S.concat "" [
            M.hex_of_string block_hash; " "; string_of_int pos; " "; string_of_int count;
          ]
          | _ -> return ()
        )
        >> Lwt_unix.lseek fd (msg_header.length - 80) SEEK_CUR
        >> loop_blocks heads (succ count)
      )
      | _ -> return heads
    )
  in
  let heads = SS.empty
  in loop_blocks heads 0


let process_file2 () =
    Lwt_unix.openfile "blocks.dat.orig" [O_RDONLY] 0
    >>= fun fd ->
      log "scanning blocks..."
    >> scan_blocks fd
    >>= fun headers -> 
      log "computing tips"
    >> 
      let tips = get_tips headers in 
      log @@ "tips " ^ (tips |> L.length |> string_of_int) 
    >> 
      

      (* let tips_work = L.map (fun hash -> (hash, get_pow2 hash headers )) tips in *)
      log "computed tips work "
    >>
      let seq = L.map (fun hash -> get_sequence hash headers ) tips in 


(*
    - rather than create a complete new lists for heads it might make 
    sense to compute a set of work mappings. hash -> work 
    - this would effectively be height  
*)

(*
      L.fold_left (fun  acc (hash,work)-> 
        log @@ "whoot " ^ M.hex_of_string hash ^ " " ^ string_of_int work) 
      (return ()) tips_work  
    >> 
 *)     log "finished "





let () = Lwt_main.run (process_file2 ())


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
        Bytes s when S.length s = 40 -> acc
        (* Bytes s -> s :: acc *)
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
  match SS.mem hash headers with
    | true -> (SS.find hash headers).height
    | false -> 0

let get_pow hash headers =
  let rec get_pow' hash =
    match SS.mem hash headers with
      | true -> 
        let previous = (SS.find hash headers).previous in
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
    match SS.mem hash headers with
      | true -> 
        let previous = (SS.find hash headers).previous in
        let lst = get_list previous lst in
        lst
      | false -> lst
  in 
  get_list hash [] 

*)


