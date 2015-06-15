

module type OrderedType =
sig
    type t
end

(* i don't think we can do module = struct because that's the full definition *)

module type S =
sig
    type my_header =
    {
        previous : string;
        pos : int;
        height : int;
    }

    module HM : module type of Map.Make(String)    

    type t

    val get_leaves : my_header HM.t -> string list

    val get_sequence : string -> my_header HM.t -> string list 
     
    val get_height : string -> my_header HM.t -> int 

    val get_longest_path : string list -> my_header HM.t -> string 

    val scan_blocks : Lwt_unix.file_descr -> my_header HM.t Lwt.t 

    (* val replay_blocks fd seq headers f x = *)
    val replay_blocks : Lwt_unix.file_descr -> string list -> my_header HM.t 
        -> (  t Lwt.t-> string -> t Lwt.t ) -> t -> t Lwt.t
end  

module Make (Ord : OrderedType) : S with type t = Ord.t 


(*
module Make (Ord : OrderedType) : S with type t = Ord.t
*)
 
 
(*
val get_leaves : my_header HM.t HMxxx -> string list 
*)


