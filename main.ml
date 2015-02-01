

(* state -> args state 
	this is a k or a tx?

	the state is more likely to be a set of users etc.

	List.fold_left (fun acc x -> acc + x) 0 [1;2;3] ;;

	if transactions are functions, (that might refer to other functions) then	
	we should fold over them.
	tx is partially bound function (eg arguments bound in).
*)


type host_info = { 
	a_balance: int;
	b_balance: int;
};;


let genesis_state  = { 
	a_balance = 123 ; 
	b_balance = 0; 
};;


let myprint2 state = 
	print_endline @@ 
      "a_balance: " ^ (string_of_int state.a_balance )
    ^ ", b_balance " ^ (string_of_int state.b_balance );


let give state1 symbol from to_ = 123 in give "AUD" "account1" () ;;


let tx state = { 
	a_balance = state.a_balance - 1; 
	b_balance = state.b_balance ; 
};;


let chain = [ tx; tx ];; 

myprint2 genesis_state;;


let result = List.fold_left 
  (fun state f -> f( state) ) genesis_state chain;;

myprint2 result ;;




