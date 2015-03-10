
open OUnit2;;

(*
let test1 test_ctxt = assert_equal 1 1 ;; (* "x" (Foo.unity "x");; *)

let test2 test_ctxt = assert_equal 100 100 ;; (* (Foo.unity 100);; *)

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2]
;;
*)



(* how do we aggregate the tests together ?? *)

let () =
  (* run_test_tt_main Test02.tests  *)

	let lst = Test10.tests @  Test9.tests in

  run_test_tt_main ( "suite">::: lst  )
;;

