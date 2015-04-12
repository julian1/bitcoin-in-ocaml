
(* there's a bunch of string manipulation functions in message.ml that
are not encode or decode that should probably go here *)

let (>>=) = Lwt.(>>=)
let return = Lwt.return

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* useful for debugging *)
let string_of_bytes s = 
  explode s |> List.map (fun ch -> ch |> Char.code |> string_of_int ) |> String.concat " " 



let read_bytes fd len =
  let block = Bytes .create len in
  Lwt_unix.read fd block 0 len >>= 
  fun ret ->
    (* Lwt_io.write_line Lwt_io.stdout @@ "read bytes - "  ^ string_of_int ret >>  *)
  return (
    if ret = len then Some ( Bytes.to_string block )
    else None 
    )

let write_stdout = Lwt_io.write_line Lwt_io.stdout

let pad s length =
    let n = length - String.length s + 1 in 
    if n > 0 then 
      s ^ String.make n ' '
    else
      s 


