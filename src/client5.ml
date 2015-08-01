
let (>>=) = Lwt.(>>=)
let return = Lwt.return

module M = Message
module U = Util
module L = List
module S = String


type whoot_t =
{
  db : int U.PG.t;
  block_id : int;
}


let log s = U.write_stdout s
(*
select block_id from previous where block_previous_id = 2
*)

let check_block hash data =
  return () 


let rec loop whoot =
  log "here"
  >> U.PG.prepare whoot.db ~query:"select hash,data from block left join block_data on block_data.block_id = block.id where block.id = $1" ()
  >> U.PG.execute whoot.db ~params:[ Some (U.PG.string_of_int whoot.block_id ) ] ()
  >>= function
      | (Some hash :: Some data :: _ )::_ -> 
          let hash = U.PG.bytea_of_string hash in
          let data = U.PG.bytea_of_string data in
          log @@ "hash " ^ M.hex_of_string hash ^ " len " ^ string_of_int @@ S.length data 
          >> check_block hash data
         
          (* lookup the next entry *)  
          >> U.PG.prepare whoot.db ~query:"select block_id from previous where block_previous_id = $1" ()
          >> U.PG.execute whoot.db ~params:[ Some (U.PG.string_of_int whoot.block_id ) ] ()
          >>= function
              | (Some field::_ )::_ -> 
              let block_id = U.PG.int_of_string field in
              log @@ "next block_id " ^ string_of_int block_id 
              >> loop { whoot with block_id = block_id } 

      | _ -> raise (Failure "couldn't get hash")


 
(* need to advance forwards *)
    

let start () =
  log "connecting to db"
  >> U.PG.connect ~host:"127.0.0.1" ~database: "dogecoin" ~user:"meteo" ~password:"meteo" ()
  >>= fun db ->
    Processblock.create_prepared_stmts db
  >>
    let state = {
      db = db;
      block_id = 3;
    }
    in
      loop state


let run f =
  Lwt_main.run (
    Lwt.catch
      f
    (fun exn ->
      (* must close *)
      let s = Printexc.to_string exn ^ "\n" ^ (Printexc.get_backtrace ()) in
      log ("finished with exception " ^ s )
      >> (* just exist cleanly *)
        return ()
    )
  )

let () = run start


