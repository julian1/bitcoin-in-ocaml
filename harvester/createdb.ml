
let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Misc



(*
  we should bind in the level db argument... 
*)


let addr_from_string s = 
  let privkey = M.sha256 s in
  match Microecc.compute_public_key privkey with 
    | Some pubkey ->  
      let pubkey = "\x04" ^ pubkey in
      let compressed_pubkey = Microecc.compress pubkey in 
      let addr_from_pubkey pubkey = 
          pubkey 
          |> Message.sha256 
          |> Message.ripemd160 
          |> Address.btc_address_of_hash160 
      in
      ( addr_from_pubkey pubkey )
    | None -> "none" 

in 
    
let log = Lwt_io.write_line Lwt_io.stdout
in

let process_line line = 
  let line = String.trim line in
  let addr = addr_from_string line in
  log @@ (U.pad line 10) ^ " " ^ (addr_from_string line) 
in  


let rec process_lines ic =
  Lwt_io.read_line_opt ic
  >>= function
    | Some line -> 
      process_line line
      (*  Lwt_io.write_line Lwt_io.stdout line *)
      >> process_lines ic 

    | _ -> return ()
in

Lwt_main.run
(
  Lwt.catch
	(
fun () ->

  let filename = Sys.argv.(1) in
  log @@ "starting... " ^ filename

	>> Lwt_unix.openfile filename [O_RDONLY ] 0o644

  >>= fun fd ->
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in

    process_lines ic

	)
  (fun exn ->
    let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
    log @@ "got exception " ^ s
  )
)


