

(* state -> args state 
	this is a k or a tx?

	the state is more likely to be a set of users etc.

	List.fold_left (fun acc x -> acc + x) 0 [1;2;3] ;;

	if transactions are functions, (that might refer to other functions) then	
	we should fold over them.
	tx is partially bound function (eg arguments bound in).

TODO
  - exception, or monad for failure. 
  - Note, that we don't need to wind back because it's functional. its the fold
  that decides whether to apply the change
  - bind the party into the tx
*)


type state_type = { 
	a_balance: int;
	b_balance: int;
};;


let genesis  = { 
	a_balance = 123 ; 
	b_balance = 0; 
};;


let myprint2 state = 
	print_endline @@ 
      "a_balance: " ^ (string_of_int state.a_balance )
    ^ ", b_balance " ^ (string_of_int state.b_balance );


let give state1 symbol from to_ = 123 in give "AUD" "account1" () ;;

(* simple tx *)
let tx state = { 
	a_balance = state.a_balance - 1; 
	b_balance = state.b_balance ; 
};;

(* partial application *)
let transfer_k_tx amount state = { 
	a_balance = state.a_balance - amount; 
	b_balance = state.b_balance + amount ; 
};;


(* chain/ledger *)
let ledger = [ tx; tx; transfer_k_tx 5 ];; 

myprint2 genesis;;


let result = List.fold_left 
  (fun state f -> f state ) genesis ledger;;

myprint2 result ;;




