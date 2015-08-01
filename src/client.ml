(*
  corebuild -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax src/client.byte

  we don't need the syntax, but do need the package.
  corebuild -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  src/client.byte
*)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Util
module L = List




type whoot_t =
{
  state : U.my_app_state option;

  (* jobs : U.jobs_type; *) (* it's not a job it's a job completion code, or result or event *)
  jobs :  U.my_event Lwt.t list; 

  queue : U.my_event Myqueue.t;
}


let log s = U.write_stdout s


let rec loop (whoot : whoot_t ) =
    (* select completed jobs *)            
    Lwt.nchoose_split whoot.jobs
    >>= fun (complete, incomplete) ->

      (* update incoplete jobs *)
      let whoot = { whoot with jobs = incomplete } in 

      (* process complete io jobs *) 
      let f whoot e = 
        match e with 
          (* nop *)
          | U.Nop -> whoot 
          (* a seq job finished, so take take the new state and add any new jobs*) 
          | SeqJobFinished (newstate, newjobs) -> { 
            whoot with state = Some newstate; 
            jobs =  newjobs @ whoot.jobs ;
          } 
          (* any other normal event gets added to queue for processing *)
          | _ -> { whoot with queue = Myqueue.add whoot.queue e }  
      in
      let whoot = L.fold_left f whoot complete in

      log @@ " here !! jobs " ^ (string_of_int (L.length whoot.jobs)) 
        ^ " queue " ^ (string_of_int (Myqueue.length whoot.queue )) 
        ^ " conns " ^ (match whoot.state with 
            | Some state -> (string_of_int (L.length state.connections )) 
            | None -> "unknown"  
        )
    >>   
      (* process a queue item if we can *)
      let whoot = 
        if whoot.queue <> Myqueue.empty && whoot.state <> None then 
          let e,queue = Myqueue.take whoot.queue in
          {  (* whoot with *) 
            queue = queue;
            jobs = (
              (* state is injected into the seq job, and nulled *)
              let Some state = whoot.state in
                P2p.update state e 
             >>= fun (U.SeqJobFinished (state, jobs1)) ->
                Chain.update state e 
             >>= fun (U.SeqJobFinished (state, jobs2)) ->
              return ( U.SeqJobFinished (state,jobs1 @ jobs2))

            ) :: whoot.jobs;
            state = None;
          }
        else
          whoot
      in
   
      (* more jobs to process? *) 
      if L.length whoot.jobs > 0 then
        loop whoot 
      else
        log "finishing - no more jobs to run!!"
        >> return ()


let loop' whoot =
  Lwt.catch (
    fun () -> loop whoot 
  )
    (fun exn ->
      (* must close *)
      let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
      log ("finishing - exception " ^ s )
      >> (* just exist cleanly *)
        return ()
    )


let () =
  Lwt_main.run U.(

    (* we'll have to think about db transactions *) 
    log "connecting and create db"
    >> U.PG.connect ~host:"127.0.0.1" ~database: "dogecoin" ~user:"meteo" ~password:"meteo" ()
    >>= fun db ->
      Processblock.create_prepared_stmts db 
    >>
        (* we actually need to read it as well... as write it... *)

        let whoot = {
          state = Some ({
            network = Dogecoin;
            connections = [];
            db = db; 
            (* should be hidden ?? *)
            block_inv_pending  = None;
            blocks_on_request = U.SS.empty;
            last_block_received_time = [];
          } : U.my_app_state )
          ; 
          jobs = P2p.create(); 
          queue = Myqueue.empty ;
        }  
        in
          loop' whoot 
  )


(*
let update state e =
  let state = P2p.update state e in
  let state = Chain.update state e in
  let state = Seq.update state e in
  state
*)



