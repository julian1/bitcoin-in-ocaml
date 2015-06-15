

module type MyType =
sig
  type t
end


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

  (* only really this function needs the functor *)
  (* val replay_blocks fd seq headers f x = *)
  val replay_blocks : Lwt_unix.file_descr -> string list -> my_header HM.t 
      -> (  t -> string -> t Lwt.t ) -> t -> t Lwt.t

  (* val get_block file_desc string my_header  - might be useful *) 
  
  (* VERY IMPORTANT - we may not have needed a functor - just abstract type t *)

r
  (*val replay_tx : Lwt_unix.file_descr -> string list -> my_header HM.t 
 
  *)
end  

module Make (Ord : MyType) : S with type t = Ord.t 


