
module M = Message

let (>>=) = Lwt.(>>=)
let return = Lwt.return

let write_stdout = Lwt_io.write_line Lwt_io.stdout

(*
  - we have all these recursive looping structures that aren't abstract enough...
  - we need to be able to fold a function...
*)

let myfunc acc (key,value) =
  let k = Index.decodeKey key in
  let v = Index.decodeValue value in

  write_stdout @@ string_of_int acc 
  >> write_stdout @@ M.hex_of_string k.hash ^ " " ^ string_of_int k.index
    ^ " " ^ v.status ^ " " ^ string_of_int v.lseek
    ^ " " ^ string_of_int v.length
  >> return (acc + 1)


let run () =
	Lwt_main.run (

    let rec loop i f acc =
      Db.get_keyval i >>= fun x ->
        f acc x >>= fun acc -> 
(*        let k = Index.decodeKey key in
        let v = Index.decodeValue value in
        write_stdout @@ M.hex_of_string k.hash ^ " " ^ string_of_int k.index
          ^ " " ^ v.status ^ " " ^ string_of_int v.lseek
          ^ " " ^ string_of_int v.length
      >> *) Db.next i
      >> Db.valid i >>= fun valid ->
        if valid then
		    	loop i f acc
        else
          return acc 
    in
	  Db.open_db "mydb"
	  >>= fun db -> Db.make db
	  >>= fun i -> Db.seek_to_first i
	  >> loop i myfunc 0
    >> return ()
  )

let () = run ()

