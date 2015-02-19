(* OCAMlfind c -w A -linkpkg -package lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax myecho.ml -o myecho *)
(* This code refers to https://github.com/avsm/ocaml-cohttpserver/blob/master/server/http_tcp_server.ml *)
open Lwt


let addr ~host ~port = 
  lwt entry = Lwt_unix.gethostbyname host in
  if Array.length entry.Unix.h_addr_list = 0 then begin
    failwith (Printf.sprintf "no address found for host %S\n" host)
  end;
  return (Unix.ADDR_INET (entry.Unix.h_addr_list.(0) , port))

(*let x =
	Lwt_unix.connect val connect : file_descr -> sockaddr -> unit Lwt.t
*)


let _ =  Lwt_main.run (
 
	addr ~host: "193.234.225.137" ~port: 8333
	>>= fun ip -> Lwt_io.write_line Lwt_io.stdout "decoded address " 

	>>= fun ()  -> let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    (Lwt_unix.connect fd ip)

	>>= fun _ -> 
		let inchan = Lwt_io.of_fd ~mode:Lwt_io.input fd in
		Lwt_io.read ~count:10 inchan 
	

	>>= fun s -> Lwt_io.write_line Lwt_io.stdout ("got someting '" ^ s ^ "'" )


	(*  >>= (fun () -> Lwt_unix.close fd)  *)
	
)


