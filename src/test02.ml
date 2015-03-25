
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


let () = Lwt_main.run (
	Lwt_io.write_line Lwt_io.stdout "hi"




	>> Lwt_preemptive.detach (fun () ->
		let db = LevelDB.open_db "mydb" in 
		return 123
	) ()

	>> return 123
	
	>>= fun v -> Lwt_io.write_line Lwt_io.stdout ( string_of_int v )
)
