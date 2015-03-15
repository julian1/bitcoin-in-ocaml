
open OUnit2


let test1 test_ctxt = 
	let () = Printf.printf "hi!" in

	let db = LevelDB.open_db "mydb" in
	let () = LevelDB.put db "aaa" "bbb" in  
	let () = LevelDB.close db in

	let db = LevelDB.open_db "mydb" in
	let v = LevelDB.get db "aaa" in
	let () = Printf.printf "lookup %s" ( 
		match v with
			| Some s -> s 
			| None -> "none"
		)
	in
	let () = LevelDB.close db in

	assert_equal (Some "bbb") v 


let tests =
	 ["test7">:: test1; ]



