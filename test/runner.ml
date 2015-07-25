
open OUnit2

(*
  test composition functions are ok
  http://ounit.forge.ocamlcore.org/api-ounit-dev/OUnit2.html

  val (>:) : string -> test -> test  - Create a TestLabel for a test
  val (>::) : string -> test_fun -> test   - Create a TestLabel for a TestCase
  val (>:::) : string -> test list -> test  - Create a TestLabel for a TestList
  val test_list : test list -> test  - Generic function to create a test list.
*)

let () =
  run_test_tt_main (
    test_list [
      Address_test.tests;
      Message_test.tests;
      Wif_test.tests;
      Difficulty_test.tests;
      Merkle_test.tests;
    ])

