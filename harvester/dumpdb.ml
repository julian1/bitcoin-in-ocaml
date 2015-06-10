(*
  corebuild -I src -package leveldb,microecc,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax  harvester/createdb.byte  harvester/readdb.byte
*)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Misc


let log = Lwt_io.write_line Lwt_io.stdout

(*
  ok, so we can look stuff up. 
  - now we want to resurrect the blockchain parser. 
  - look up txs...
  - process_block
  - process_tx   leveldb type so we can do stuff...
*)


let rec loop i = 
  Db.valid i >>= function
    | true -> ( 
      Db.get_key i 
      >>= fun key -> Db.get_value i 
      >>= fun value -> log @@ M.hex_of_string key ^ " " ^ value
      >> Db.next i
      >> loop i
    )
    | _ -> return ()

let () = Lwt_main.run (
  Lwt.catch (
  fun () ->
    Db.open_db "myhashes"
    >>= fun db -> Db.make db  
    >>= fun i -> Db.seek_to_first i 
    >> loop i
	)
  (fun exn ->
    let s = Printexc.to_string exn ^ "\n" ^ (Printexc.get_backtrace ()) in
    log @@ "got exception " ^ s
  ))
