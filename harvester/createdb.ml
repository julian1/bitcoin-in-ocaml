(*
  corebuild -I src -package leveldb,microecc,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax  harvester/createdb.byte

  the raw 160 hash would be a lot more efficient storage wise
*)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Misc


let addr_from_string s =
  let privkey = M.sha256 s in
  match Microecc.compute_public_key privkey with
    | Some pubkey ->
      let pubkey = "\x04" ^ pubkey in
      let compressed_pubkey = Microecc.compress pubkey in
      let addr_from_pubkey pubkey =
          pubkey
          |> M.sha256
          |> M.ripemd160
          |> Address.btc_address_of_hash160
      in
      ( addr_from_pubkey pubkey )
    | None -> "none"
in

let log = Lwt_io.write_line Lwt_io.stdout
in

let process_line db line =
  let line = String.trim line in
  let addr = addr_from_string line in
(*  log @@ (U.pad line 10) ^ " " ^ (addr_from_string line)  *)
  Db.put db addr line
in

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

