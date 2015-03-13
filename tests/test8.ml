
(* 
	ordering of package args matters,
	clear; corebuild    -package lwt.preemptive,leveldb,microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax -I src tests/test_runner.byte
*)

open OUnit2

open Lwt (* for >>= *)


let test1 test_ctxt = 

	let () = Lwt_preemptive .init 5 10 (fun err -> Printf.printf "error %s" err  )  in

	ignore ( Lwt_main.run (
 

		Lwt_io.write_line Lwt_io.stdout "whoot1"

		>>= fun _ -> Lwt_preemptive.detach (fun () -> LevelDB.open_db "mydb"   ) ()  

		>>= fun db -> Lwt_preemptive.detach (fun () -> LevelDB.get db "aaa"   ) ()  

		>>= fun v -> Lwt_io.write_line Lwt_io.stdout (match v with
			| Some s -> s 
			| None -> "none"
		)
 
		)
	)  



let tests =
	 ["test8">:: test1; ]



