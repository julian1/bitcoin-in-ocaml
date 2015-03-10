
open OUnit2;;



(* how do we aggregate the tests together ?? *)

let () =
  (* run_test_tt_main Test02.tests  *)

	let lst = Test10.tests @  Test9.tests in
	run_test_tt_main ( "all">::: lst  )
;;

