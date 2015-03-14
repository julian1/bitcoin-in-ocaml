

let (>>=) = Lwt.bind
let return = Lwt.return 


let debug = ref false
let dbg fmt a b = () (* ksprintf (eprintf "Trakeva_sqlite: %s\n%!") fmt *)
 
type t = {
  handle: Sqlite3.db;
  action_mutex: Lwt_mutex.t;
}



let in_posix_thread ~on_exn f =
  Lwt_preemptive.detach (fun () ->
  (* (fun f () -> Lwt.return (f ()))  (fun () -> *)
    try `Ok (f ())
    with e -> on_exn e) ()



let with_executed_statement handle statement f =
  let prep = Sqlite3.prepare handle statement in
  (try
    let x = f prep in
    let _ = Sqlite3.finalize prep in
    x
  with e ->
    let _ = Sqlite3.finalize prep in
    raise e)



let get_row_exn prep =
  match Sqlite3.step prep with
  | Sqlite3.Rc.ROW -> Sqlite3.column prep 0, true
  | Sqlite3.Rc.DONE -> Sqlite3.column prep 0, false
  | rc -> failwith (Printf.sprintf "not a row: %s" (Sqlite3.Rc.to_string rc))

let string_option_data_exn data =
  let open Sqlite3.Data in
  begin match data with
  | NONE  -> failwith "string_option_data_exn: none"
  | NULL  -> None
  | INT _ -> failwith "string_option_data_exn: int"
  | FLOAT _ -> failwith "string_option_data_exn: float"
  | TEXT s
  | BLOB s -> Some s
  end
 

let exec_option_exn handle statement =
  with_executed_statement handle statement (fun prep ->
      let first, (_ : bool) = get_row_exn prep in
      string_option_data_exn first)



let is_ok_or_done_exn handle (rc: Sqlite3.Rc.t) =
  let open Sqlite3.Rc in
  match rc with
  | OK  -> ()
  | DONE -> ()
  | _ -> failwith (Printf.sprintf "not ok/done: %s (global error: %s)"
                     (Sqlite3.Rc.to_string rc)
                     (Sqlite3.errmsg handle))



let exec_unit_exn handle statement =
  with_executed_statement handle statement (fun prep ->
      Sqlite3.step prep |> is_ok_or_done_exn handle)


let load path =
  (* let on_exn e = `Error (`Database (`Load path, Printexc.to_string e)) in *)
  let on_exn e = `Error ( Printexc.to_string e ) in

	(*
  begin
    try if Sys.getenv "TRAKEVA_SQLITE_DEBUG" = "true" then debug := true
    with _ -> ()
  end;
	*)
  let action_mutex = Lwt_mutex.create () in
  in_posix_thread ~on_exn (fun () ->
     (* if !debug then
        dbg "openning: %S" path; *)
      (* we need the mutex `FULL: https://www.sqlite.org/threadsafe.html
         the private cache is up for debate: 
         https://www.sqlite.org/sharedcache.html *)
      let handle = Sqlite3.db_open ~mutex:`FULL ~cache:`PRIVATE path in
      (* exec_unit_exn handle (create_table default_table); *)
        (* {handle; action_mutex}   *)
		handle
    )


let close handle =
  let on_exn e = `Error ( Printexc.to_string e) in
  in_posix_thread ~on_exn begin fun () ->
    let rec loop = function
    | 0 -> failwith "failed to close (busy many times)"
    | n ->
      if Sqlite3.db_close handle then () else (
        Sqlite3.sleep 100 |> ignore;
        loop (n - 1)
      )
    in
    loop 5
  end




let _ = Lwt_main.run (

	load "jjj"
	(* >>= fun _ -> Lwt_io.write_line Lwt_io.stdout "starting" *)
(*
	>>= fun x -> match x with 
		| `Ok handle -> let _ = exec_unit_exn handle "create table mytable(one varchar(10), two smallint)" 
			in Lwt.return x 

	>>= fun x -> match x with 
		| `Ok handle -> let _ = close handle  
			in Lwt.return x 
*)
	
	
	>>= fun x -> match x with 
		| `Ok handle_ -> ( let handle = handle_ in 
			let _ = exec_unit_exn handle "insert into mytable values('hello!', 'whoot')" 
			in Lwt.return () 
	
		>>= fun _ -> Lwt_io.write_line Lwt_io.stdout "done inserting" 

(*		>>= fun _ -> let _ = exec_unit_exn handle "select * from mytable"  in return ()
*)

	>>= fun _ ->
	  let on_exn e = `Error (Printexc.to_string e) in

	(* we want a fucking fold here *)
	  let lister row headers =
          Printf.printf "    %s : '%s'\n" headers.(0) row.(0);
          Printf.printf "    %s : '%s'\n" headers.(1) row.(1)
      in

	  in_posix_thread ~on_exn (fun () ->
		  Sqlite3.exec_not_null handle ~cb:lister "select * from mytable"
		)

(*
	>>= fun _ ->
	  let on_exn e = `Error (Printexc.to_string e) in
	  in_posix_thread ~on_exn (fun () ->
		  exec_option_exn handle "select * from mytable"
		)
*)


	)
	
)	

