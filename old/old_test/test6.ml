

let (>>=) = Lwt.bind

let open_db name = Lwt_preemptive.detach (fun () -> LevelDB.open_db name ) ()

let get db key = Lwt_preemptive.detach (fun () -> LevelDB.get db key ) ()


let () = Lwt_preemptive.init 5 10 (fun err -> Printf.printf "error %s" err  )  in
let _ = Lwt_main.run (
	Lwt_io.write_line Lwt_io.stdout "starting"
	>> open_db  "mydb"
	>>= fun db -> get db "aaa"
	>>= fun v -> Lwt_io.write_line Lwt_io.stdout (match v with
		| Some s -> s
		| None -> "none"
	)
) in 
()

