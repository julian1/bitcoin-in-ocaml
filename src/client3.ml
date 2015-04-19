(*
corebuild    -package leveldb,microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax  src/client3.byte

*)


let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message


type my_app_state =
{
  jobs :  Misc.jobs_type  ;

  connections : Misc.connection list ;

  (* responsible for downloading chain *)
  chain :  Chain.t; 
}



let run f =

  Lwt_main.run (

    Chain.create () 
    >>= fun chain ->   
    (* we actually need to read it as well... as write it... *)
    let state =
      {
        jobs = P2p.create () ;
        connections = [];
        chain = chain ;
      }
    in
    let rec loop state =
      Lwt.catch (
      fun () -> Lwt.nchoose_split state.jobs

        >>= fun (complete, incomplete) ->
          (*Lwt_io.write_line Lwt_io.stdout  @@
            "complete " ^ (string_of_int @@ List.length complete )
            ^ ", incomplete " ^ (string_of_int @@ List.length incomplete)
            ^ ", connections " ^ (string_of_int @@ List.length state.connections )
        >>
      *)
          let new_state = List.fold_left f { state with jobs = incomplete } complete in 
          if List.length new_state.jobs > 0 then
            loop new_state
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


let f state e =
  let (connections, jobs1) = P2p.update state.connections e in
  let (chain, jobs2) = Chain.update state.chain connections e in 
  { state with 
    chain = chain; 
    connections = connections; 
    jobs =  state.jobs @ jobs1 @ jobs2  
  }


let () = run f



