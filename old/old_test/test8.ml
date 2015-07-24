
(*
	ordering of package args matters,
	clear; corebuild    -package lwt.preemptive,leveldb,microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax -I src tests/test_runner.byte
*)

open OUnit2

let (>>=) = Lwt.bind

let test1 test_ctxt =

	let () = Lwt_preemptive.init 5 10 (fun err -> Printf.printf "error %s" err  )  in
	let _ = Lwt_main.run (
		Lwt_io.write_line Lwt_io.stdout "starting"
		>>  Lwt_preemptive.detach (fun () -> LevelDB.open_db "mydb" ) ()
		>>= fun db -> Lwt_preemptive.detach (fun () -> LevelDB.get db "aaa" ) ()
		>>= fun v -> Lwt_io.write_line Lwt_io.stdout (match v with
			| Some s -> s
			| None -> "none"
		)
	)
	in
	assert_equal true true

let tests = ["test8">:: test1; ]



