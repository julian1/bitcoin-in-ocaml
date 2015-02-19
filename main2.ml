
(* time corebuild  main2.byte

  - how do we view/ define the type?

  - if someone pledges collateral to short something into existance, then
  that collateral should be moved to a separate account (perhaps still associated
  with user).
  - ought to make it easier to handle events,

  - rather than directly manipulating the state as a data structure, can we encapsulate
  in order to handle permissions? hmmm,

  - no the abstraction we need, is use of other contracts eg to transfer... which will
  - otherwise - don't allow func to return/manipulate the state, but instead return an internal
       set of actions to do it. these then get played transactionally.
*)

open Core.Std



type accounts_type = int String.Map.t

type state_type = {
  accounts : accounts_type ;
  dummy : int;
}

let print_map map =
  let print_key_val ~key:k ~data:v = print_endline @@ k ^ "->" ^ (string_of_int v) in
  Map.iter map print_key_val


let genesis  = {
  dummy = 123;
  accounts = Map.of_alist_exn [ "alice", 0; "bob", 1 ] ~comparator:String.comparator ;
}


let tx_create_account name state =
  { state with
    accounts = state.accounts |> Map.add ~key:name  ~data: 0
  }

let tx_transfer name amount state =
  { state with
    accounts =
      (* this isn't right, it's hiding the error 
        - we return state or we return None error
      *)	
      match Map.find state.accounts name  with
        | None -> state.accounts
        | Some x -> Map.add state.accounts ~key:name ~data: (x + amount)
  }


(* can we compose a two way transfer 
  then compose with keys etc.
	- also enforce conditions - balance positive, signature, permission, time? account exists 
    - if they fail, we just rejjj
		- but we are probably going to need an option monad for all the conditions... 
	- an offer is an offer
	- a contract is two way exchange

  - a contract that needs time as a feed, just goes in a separate part of state, with
  subcontracts, specifying the observable. and then the dynamics will evaluate it. and
  it will pay for the dynamic evaluation.  
  - using the map, we can also analyze state change, by using a map difference

  - a contract should have to pay, to insert a collateral contract into the set of contracts that 
  are evaluated at each block.
*)

let ledger = [ 
  tx_create_account "john"; 
  tx_transfer "john" 10 ; 
  let amount = 5 in
  Fn.compose (tx_transfer "john" @@ -amount) (tx_transfer "alice" amount ) 
];;


let result_state = List.fold_left
  ~init:genesis ~f:(fun state f -> f state) ledger ;;



let _ = print_map result_state.accounts



(*
      match Map.find state name  with
        | None -> 0
        | Some x -> x
*)

(*let result = tx_add_clair genesis  *)
(* issue is different type
state
  val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
  val fold_left : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
*)


(*
let _ = print_endline "here2"


let string_alist = [ "zero", 0; "one", 1 ]

let digit_map = Map.of_alist_exn  string_alist ~comparator:String.comparator |>  Map.add ~key:"two" ~data:2

let _ = print_endline @@ string_of_int( Map.find_exn digit_map "one" )

let _ = print_map digit_map
*)
