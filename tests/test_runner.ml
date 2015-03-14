
open OUnit2;;


let () =

	(*let lst = Test10.tests @  Test9.tests @ Test7.tests in *)
	(* let lst = Test1.tests @ Test10.tests @  Test9.tests @ Test7.tests @ Test8.tests  in *)
	let lst = Test1.tests  in
	run_test_tt_main ( "all">::: lst  )
;;

