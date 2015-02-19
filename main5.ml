(* ocamlfind c -w A -linkpkg -package lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax myecho.ml -o myecho *)
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
 
	addr ~host: "www.google.com" ~port: 80
	>>= fun ip -> Lwt_io.write_line Lwt_io.stdout "decoded address " 

	>>= fun ()  -> let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    (Lwt_unix.connect fd ip)

	>>= fun _ -> Lwt_io.write_line Lwt_io.stdout "connected" 

	>>= fun _ -> 
		let outchan = Lwt_io.of_fd ~mode:Lwt_io.output fd in
		Lwt_io.write_line outchan  "GET / HTTP/1.1\n\n" 

	>>= fun _ -> 
		let inchan = Lwt_io.of_fd ~mode:Lwt_io.input fd in
		Lwt_io.read_line inchan 
	

	>>= fun s -> Lwt_io.write_line Lwt_io.stdout @@ "got something\n" ^ s 

	(*  >>= (fun () -> Lwt_unix.close fd)  *)
	
)


