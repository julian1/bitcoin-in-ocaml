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


module M = Message


module PG = Misc.PG

let log s = Misc.write_stdout s


(* read a block at current pos and return it - private *)
let read_block fd =
  Misc.read_bytes fd 24
  >>= function
    | None -> return None
    | Some s ->
      let _, header = M.decodeHeader s 0 in
      (* should check command is 'block' *)
      Misc.read_bytes fd header.length
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
  >> PG.connect ~host:"127.0.0.1" ~database: "prod" ~user:"meteo" ~password:"meteo" ()
  >>= fun db ->
    Processblock.create_prepared_stmts db
  >> Lwt_unix.openfile "blocks.dat.orig" [O_RDONLY] 0
  >>= fun fd ->
    log "scanning blocks..."
(*  >>
    let x = {
      block_count = 0;
      db = db;
    }
    in
*)
(*  >> replay_blocks fd process_block x *)

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

