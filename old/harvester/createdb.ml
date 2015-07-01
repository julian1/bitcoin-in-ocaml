(*
  corebuild -I src -package leveldb,microecc,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax  harvester/createdb.byte

  - the raw 160 hash would be a lot more efficient storage wise - than storing the string  - 20 bytes only:

  - ok, so we want to open the db ro and do stuff...

  - note 
    - using partial application to bind db into process_line, to make process_lines more general
    - using inner rec function, to avoid passing unchanging params through recursion
*)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Misc

let log = Lwt_io.write_line Lwt_io.stdout

let hash160_from_privkey privkey =
  let sha = M.sha256 privkey in
  match Microecc.compute_public_key sha with
    | Some pubkey ->
      let uncompressed_pubkey = "\x04" ^ pubkey in
      let compressed_pubkey =   Microecc.compress pubkey in
      let addr_from_pubkey key = key |> M.sha256 |> M.ripemd160 (*|> Address.btc_address_of_hash160*)
      in
      (* *)
      [ addr_from_pubkey uncompressed_pubkey ; addr_from_pubkey compressed_pubkey ] 
    | None -> []


let process_line db line =
  let line = String.trim line in
  let addrs = hash160_from_privkey line in
  List.fold_left (fun acc e -> Db.put db e line) (return ()) addrs  

(*  log @@ "line " ^ line 
  >> 
  List.fold_left (fun acc e -> log @@ M.hex_of_string e ) (return ()) addrs 
*)
  (* Db.put db addr line *)

(*  log @@ (U.pad line 10) ^ " " ^ (hash160_from_privkey line)  *)


let process_lines process_line ic =
  let rec process_lines' count =
    Lwt_io.read_line_opt ic
    >>= function
      | Some line ->
        process_line line
        >> (match count mod 1000 with
          | 0 -> log @@ "count " ^ string_of_int count
          | _ -> return ()
        )
        >> process_lines' (succ count)
      | _ -> return ()
  in
  process_lines' 0

let () = Lwt_main.run
(
  Lwt.catch
  (
  fun () ->
    let filename = Sys.argv.(1) in
    log @@ "starting... " ^ filename
	>> Lwt_unix.openfile filename [O_RDONLY ] 0o644
  >>= fun fd ->
    Db.open_db "myhashes"
  >>= fun db ->
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let process_line = process_line db in
    process_lines process_line ic
	)
  (fun exn ->
    let s = Printexc.to_string exn ^ "\n" ^ (Printexc.get_backtrace ()) in
    log @@ "got exception " ^ s
  )
)

