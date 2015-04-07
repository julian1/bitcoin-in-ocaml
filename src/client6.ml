(*
  scan indexes, for spent and unspent and count them 

  - we have all these recursive folding structures over blocks and now txs that aren't abstract enough...
  - should be polymorphic 
*)
module M = Message

let (>>=) = Lwt.(>>=)
let return = Lwt.return


type accounting = {
  unspent : int ;
  spent : int ;
}


let myfunc acc (key,value) =
  let k = Index.decodeKey key in
  let v = Index.decodeValue value in
  let acc = 
    match v.status with 
      "u" -> { acc with unspent = acc.unspent + 1 }  
      | "s" -> { acc with spent = acc.spent + 1 }  
      | _ -> raise ( Failure "ughh" )
  in
  if ((acc.unspent + acc.spent) mod 100000) = 0 then
    Misc.write_stdout @@ "unspent " ^ string_of_int acc.unspent 
      ^ " " ^ " spent " ^ string_of_int acc.spent 
  else
    return ()
    ;

  (* Misc.write_stdout @@ M.hex_of_string k.hash ^ " " ^ string_of_int k.index
    ^ " " ^ v.status ^ " " ^ string_of_int v.lseek
    ^ " " ^ string_of_int v.length
  >> Misc.write_stdout @@ "unspent " ^ string_of_int acc.unspent 
    ^ " " ^ " spent " ^ string_of_int acc.spent 
  >> *)
  >> return acc 

let rec fold i f acc =
  Db.get_keyval i 
  >>= fun x -> f acc x 
  >>= fun acc -> Db.next i
  >> Db.valid i 
  >>= fun valid ->
    if valid (* && acc.unspent < 100 *) then
      fold i f acc
    else
      return acc 


let run () =
	Lwt_main.run (

	  Db.open_db "mydb"
	  >>= fun db -> Db.make db
	  >>= fun i -> Db.seek_to_first i
	  >> fold i myfunc { unspent = 0; spent = 0; } 
    >>= fun acc -> 
      Misc.write_stdout @@ "finished - unspent " ^ string_of_int acc.unspent 
      ^ " " ^ " spent " ^ string_of_int acc.spent 
  )

let () = run ()

