

let replay_tx fd seq headers process_tx x =
  let process_block = process_block process_tx in
  Sc.replay_blocks fd seq headers process_block x



    >> Sc.scan_blocks fd
    >>= fun headers ->
      log "done scanning blocks - getting leaves"
    >>
      let leaves = Sc.get_leaves headers in
      log @@ "leaves " ^ (leaves |> L.length |> string_of_int)
    >>
      let longest = Sc.get_longest_path leaves headers in
      log @@ "longest " ^ M.hex_of_string longest
    >>
      log "computed leaves work "
    >>
      let seq = Sc.get_sequence longest headers in
      let seq = CL.drop seq 1 in (* we are missng the first block *)
      (*let seq = CL.take seq 50000 in *)
      (* let seq = [ M.string_of_hex "00000000000004ff6bc3ce1c1cb66a363760bb40889636d2c82eba201f058d79" ] in *)


       let last = seq |> L.rev |> L.hd in
      log @@ "last hash " ^ M.hex_of_string last 
      

