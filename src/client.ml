(*
  corebuild -I src -I harvester -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax src/client.byte
*)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message


let log s = Misc.write_stdout s

let run f =

  Lwt_main.run (

    (* we'll have to think about db transactions *) 
    log "connecting and create db"
    >> Misc.PG.connect ~host:"127.0.0.1" ~database: "prod" ~user:"meteo" ~password:"meteo" ()
    >>= fun db ->

      Readblocks2.create_prepared_stmts db 
    >>

      Chain.create ()
    >>= fun blocks_fd -> 
       (
        (* we actually need to read it as well... as write it... *)
        let state =
          ({
			jobs = P2p.create();
            connections = [];
			(*heads = tree; *)

            db = db; 
			blocks_fd = blocks_fd;

			(* should be hidden ?? *)
			block_inv_pending  = None;
			blocks_on_request = Misc.SS.empty;
			last_block_received_time = [];

			seq_jobs_pending = Myqueue.empty;
			seq_job_running = false;

          } : Misc.my_app_state )
        in

        let rec loop (state : Misc.my_app_state ) =
          Lwt.catch (
          fun () -> Lwt.nchoose_split state.jobs

            >>= fun (complete, incomplete) ->
              (*Lwt_io.write_line Lwt_io.stdout  @@
                "complete " ^ (string_of_int @@ List.length complete )
                ^ ", incomplete " ^ (string_of_int @@ List.length incomplete)
                ^ ", connections " ^ (string_of_int @@ List.length state.connections )
            >>
          *)
              let state = List.fold_left f { state with jobs = incomplete } complete in
              if List.length state.jobs > 0 then
                loop state
              else
                Lwt_io.write_line Lwt_io.stdout "finishing - no more jobs to run!!"
                >> return ()
          )
            (fun exn ->
              (* must close *)
              let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
              Lwt_io.write_line Lwt_io.stdout ("finishing - exception " ^ s )
              >> (* just exist cleanly *)
                return ()
            )
        in
          loop state
        )
  )


let update state e =
  let state = P2p.update state e in
  let state = Chain.update state e in
  let state = Seq.update state e in
  state


let () = run update 



