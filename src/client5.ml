
let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Util
module L = List


type whoot_t =
{
  db : int U.PG.t;
}


let log s = U.write_stdout s


let rec loop whoot =
  log "here"
  >>
  if true then
    log "finishing!!"
  else
    loop whoot


let start () =
  log "connecting to db"
  >> U.PG.connect ~host:"127.0.0.1" ~database: "dogecoin" ~user:"meteo" ~password:"meteo" ()
  >>= fun db ->
    Processblock.create_prepared_stmts db
  >>
    let state = {
      db = db;
    }
    in
      loop state


let run f =
  Lwt_main.run (
    Lwt.catch
      f
    (fun exn ->
      (* must close *)
      let s = Printexc.to_string exn ^ "\n" ^ (Printexc.get_backtrace ()) in
      log ("finished with exception " ^ s )
      >> (* just exist cleanly *)
        return ()
    )
  )

let () = run start


