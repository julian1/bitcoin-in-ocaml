
(* nit : int -> (int -> char) -> string


magic 36 - '(' 40 1423975208 1423975208
magic 37 - '#' 35 5562403 72057594043490339
magic 38 - '<E0>' 224 21728 281474976732384
magic 39 - 'T' 84 84 1099511627860

*)
open Core

let acc = 1423975208 in

let enc bytes value =
  String.init bytes (fun i ->
    let h = 0xff land (acc lsr (i * 8)) in
    (* Printf.printf "%d %d\n" i h ; *)
    char_of_int h
  )
in

Printf.printf "value %s\n" @@ "magic" ^ (enc 4 acc )



