
(*
	corebuild    -package leveldb,microecc,cryptokit,zarith,lwt,lwt.unix,lwt.preemptive,lwt.syntax -syntax camlp4o,lwt.syntax  src/test02.byte
*)


open Lwt
let (>>=) = Lwt.bind
(* let return = Lwt.return  *)


(*
let in_posix_thread ~on_exn f =
  Lwt_preemptive.detach (fun () ->
  (* (fun f () -> Lwt.return (f ()))  (fun () -> *)
    try `Ok (f ())
    with e -> on_exn e) ()
*)

(*
  to write a block.
    - write append only log
    - delete old head
    - add new head
*)

let detach f = 
  Lwt_preemptive.detach 
    (fun () -> f ) () 

let () = Lwt_main.run (

	Lwt_io.write_line Lwt_io.stdout "hi"

	>> detach @@ LevelDB.open_db "mydb" 

	>>= fun db -> detach @@ LevelDB.get db "myhash" 
	
	>>= fun v -> 
    Lwt_io.write_line Lwt_io.stdout 
      @@ match v with 
        | Some s -> s 
        | None -> "none" 
    
)
