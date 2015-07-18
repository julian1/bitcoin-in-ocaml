
module M = Message
module U = Util
module L = List
module S = String
module CL = Core.Core_list

module PG = U.PG  (* TODO PG shouldn't depend on U, move PG out of Util *)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

open M

let log s = U.write_stdout s

let do_query db =
  let query =
    "
    select 
      substring( block_data.data, tx_block.pos + output.pos + 1, output.length ) --,
      -- output.* ,
      -- tx.hash,
      -- tx_block.pos + output.pos
    from output
    join tx on tx.id = output.tx_id 
    join tx_block on tx_block.tx_id = tx.id
    join block on block.id = tx_block.block_id
    join block_data on block_data.id = block.block_data_id
    where tx.id = $1
  " in
    PG.prepare db ~query:query ()
    >> PG.execute db ~params:[
      Some (PG.string_of_int 171 );
    ] ()
  >>= function
    (Some field ::_ )::_ ->
      let output = PG.bytea_of_string field in
      let _, doutput = decodeTxOutput output 0 in 
      let script =  M.decode_script doutput.script in

      log @@ "got something back " ^ M.hex_of_string output 
      >> log @@ "amount " ^ Int64.to_string doutput.value 
      >> log @@ "script " ^ M.format_script script
    | _ -> raise (Failure "tx not found")


let () = Lwt_main.run U.(M.(

  log "connecting to db"
  >> PG.connect ~host:"127.0.0.1" ~database: "test" ~user:"meteo" ~password:"meteo" ()
  >>= fun db ->
    log "whoot connected" 
  >>
  do_query db
))


