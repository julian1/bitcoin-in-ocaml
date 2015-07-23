

type my_header =
{
    previous : string;
    pos : int;
    height : int;
}

module HM : module type of Map.Make(String)    


val get_leaves : my_header HM.t -> string list

val get_sequence : string -> my_header HM.t -> string list 
 
val get_height : string -> my_header HM.t -> int 

val get_longest_path : string list -> my_header HM.t -> string 

val scan_blocks : Lwt_unix.file_descr -> my_header HM.t Lwt.t 

(*
  rather than, 
    string list -> my_header HM.t  
  could just use pos
    int list 
*)

val replay_blocks : Lwt_unix.file_descr -> string list -> my_header HM.t 
    -> ('a -> string -> 'a Lwt.t ) -> 'a -> 'a Lwt.t

(*
  we should be allowing the handler to handle this message
*)
val replay_tx : Lwt_unix.file_descr -> string list -> my_header HM.t 
    -> ('a -> (string * Message.tx ) -> 'a Lwt.t ) -> 'a -> 'a Lwt.t

