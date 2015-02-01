

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

let f x = { a_balance = x.a_balance; b_balance = 0; };;

(* let myprint state = output_string stdout (string_of_int state.a_balance );;   *)
let myprint state = output_string stdout @@ string_of_int state.a_balance ;;  

let myprint2 state = 
	print_endline @@ "a_balance: " ^ (string_of_int state.a_balance )
    ^ ", b_balance " ^ (string_of_int state.b_balance );


let give state1 symbol from to_ = 123 in give "AUD" "account1" () ;;


let _ = output_string stdout "hi\n";;


myprint2 genesis_state;

