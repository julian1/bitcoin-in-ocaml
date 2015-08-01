(*
  scans a file with blocks and inserts them into the db

  on block inserttion - we should check
    - the block doesn't already exist
    - it's previous links into the tree 


  - IMPORTANT moving the transaction begin and commit outside the process_block
    might be useful, to make more composible.

  - should probably avoid exceptions if the point of this code, is allow fast load
    from file.
  - choices
    - avoid exceptions
    - if the block has already been in inserted return something to indiate that ...
    - or simply rely on the db...

  - should wrap the process_block up... only. other exceptions should kill the
  - the exception indicates a postgres tx exception which is good...
  - to catch exceptions outside the block...
  -----------
  OK, rather than rely on exceptions why not just test first...
*)
(*
  scan blocks and store to db

  corebuild -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax src/client2.byte
*)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
let return = Lwt.return

module M = Message

module U = Util
module PG = U.PG



let log s = U.write_stdout s

type mytype2 =
{
  block_count : int;
  db : int PG.t ; (* TODO what is this *)
}



let process_block (x : mytype2) payload =
  Lwt.catch (
    fun () -> return x (* Processblock.process_block x.db payload *)
  )
  (fun exn ->
    PG.rollback x.db
    >>
      let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
      log ("rollback " ^ s )
    >> return x
  )


(* read a block at current pos and return it - private *)
let read_block fd =
  U.read_bytes fd 24
  >>= function
    | None -> return None
    | Some s ->
      let _, header = M.decodeHeader s 0 in
      (* should check command is 'block' *)
      U.read_bytes fd header.length
      >>= function
        | None -> raise (Failure "here2")
        | Some payload -> return (Some payload)


(* scan through blocks in the given sequence
  - perhaps insted of passing in seq and headers should pass just pos list *)

let replay_blocks fd f x =
  let rec replay_blocks' x =
      read_block fd
    >>= function
      | None -> return x
      | Some payload -> f x payload
    >>= fun x ->
      replay_blocks' x
  in
  replay_blocks' x


let process_file () =
  log "connecting and create db"
  >> PG.connect ~host:"127.0.0.1" ~database: "aaa" ~user:"meteo" ~password:"meteo" ()
  >>= fun db ->
    Processblock.create_prepared_stmts db
  >> Lwt_unix.openfile "blocks.dat.orig" [O_RDONLY] 0
  >>= fun fd ->
    log "scanning blocks..."
  >>
    let x =
      {
        block_count = 0;
        db = db;
      } in
    replay_blocks fd process_block x
  >> PG.close db
  >> log "finished "


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


(* insert genesis 
  PG.begin_work db
  >> PG.execute x.db ~name:"insert_block2" ~params:[
    Some (PG.string_of_bytea (M.string_of_hex "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f") );
  ] ()
  >> PG.commit db
*)
