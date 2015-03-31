 

open Lwt (* for >>= *)



Lwt_main.run (

  let read_bytes fd len =
    let block = Bytes .create len in
    Lwt_unix.read fd block 0 len >>= 
    fun ret ->
      (* Lwt_io.write_line Lwt_io.stdout @@ "read bytes - "  ^ string_of_int ret >>  *)
    return (
      if ret > 0 then Some ( Bytes.to_string block )
      else None 
      )
  in
  let advance fd len =
      Lwt_unix.lseek fd len SEEK_CUR 
      >>= fun r -> 
      (* Lwt_io.write_line Lwt_io.stdout @@ "seek result " ^ string_of_int r  *)
      return ()
  in
  (* to scan the messages stored in file 
    this thing is effectively a fold. 
    don't we want to be able to compute something...
    perhaps monadically...
  *)
  let rec loop fd =
    read_bytes fd 24
    >>= fun x -> match x with 
      | Some s -> ( 
        let _, header = Message.decodeHeader s 0 in
        (* Lwt_io.write_line Lwt_io.stdout @@ header.command ^ " " ^ string_of_int header.length >> *) 
        read_bytes fd 80 
        >>= fun u -> match u with 
          | Some payload -> 
            let hash = payload |> Message.sha256d |> Message.strrev in
            let _, block_header = Message.decodeBlock payload 0 in
              advance fd (header.length - 80 )
              >> loop fd  
          | None -> 
            return () 
        )
      | None -> 
        return () 
  in 

  Lwt_unix.openfile "blocks.dat"  [O_RDONLY] 0 
  >>= fun fd -> 
    match Lwt_unix.state fd with 
      Opened -> 
        Lwt_io.write_line Lwt_io.stdout "scanning blocks..." 
        >> loop fd 
        >> Lwt_unix.close fd
(**)
)
