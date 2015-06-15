
(*
  why do we have to repeat everything ?? 
*)

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
  (* val replay_blocks fd seq headers f x = *)
  val replay_blocks : Lwt_unix.file_descr -> string list -> my_header HM.t 
      -> (  t -> string -> t Lwt.t ) -> t -> t Lwt.t
end  





module Make(Ord: MyType) =
struct

  module L = List
  module HM = Map.Make(String)
  module HS = Set.Make(String)
  module S = String
  
  (* not nice to have here *)
  module M = Message

  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)  (* like bind, but second arg return type is non-monadic *)
  let return = Lwt.return


  type t = Ord.t

  type my_header =
  {
      previous : string;
      pos : int;
      height : int;
  }



  (* get the tree leaf hashes as a list *)
  let get_leaves headers =
      (* create set of all previous hashes *)
      let f _ header acc = HS.add header.previous acc in
      let previous = HM.fold f headers HS.empty in
      (* leaves are headers that are not pointed at by any other header *)
      HM.filter (fun hash _ -> not @@ HS.mem hash previous ) headers
      |> HM.bindings
      |> L.rev_map (fun (tip,_) -> tip)


  (* trace sequence back to genesis and return hashes as a list *)
  let get_sequence hash headers =
    let rec get_list hash lst =
      let lst = hash :: lst in
      match HM.mem hash headers with
        | true ->
          let previous = (HM.find hash headers).previous in
          get_list previous lst
        | false -> lst
    in
    get_list hash []


  (* assuming calculate as we go
    - probably should be able to calulate difficulty dynamically  *)
  let get_height hash headers =
    match HM.mem hash headers with
      | true -> (HM.find hash headers).height
      | false -> 0



  (* given list of leaves - return hash of the longest one -
      horrible - should be recursive and use just the headers
  *)
  let get_longest_path leaves headers =
    (* associate hash with height *)
    let x = L.map (fun hash -> hash, (HM.find hash headers).height) leaves in
    (* select hash with max height *)
    let longest, _ = L.fold_left (fun a b ->
      let _,ah = a in
      let _,bh = b in
      if ah > bh then a else b) ("",-1) x in
    longest


  let log = Lwt_io.write_line Lwt_io.stdout

  (* scan blocks and return a headers structure *)
  let scan_blocks fd =
    let rec loop_blocks headers count =
      (
      Lwt_unix.lseek fd 0 SEEK_CUR
      >>= fun pos -> Misc.read_bytes fd (24 + 80)
      >>= function
        | Some s -> (
          let _, msg_header = M.decodeHeader s 0 in
          let block_hash = M.strsub s 24 80 |> M.sha256d |> M.strrev in
          let _, block_header = M.decodeBlock s 24 in
          let previous = block_header.previous in
          let height = (get_height previous headers) + 1 in
          let headers = HM.add block_hash { previous = previous; height = height; pos = pos } headers in
          ( match count mod 10000 with
            0 -> log @@ S.concat "" [
              M.hex_of_string block_hash; " "; string_of_int pos; " "; string_of_int count;
            ]
            | _ -> return ()
          )
          >> Lwt_unix.lseek fd (msg_header.length - 80) SEEK_CUR
          >> loop_blocks headers (succ count)
        )
        | _ -> return headers
      )
    in
    let headers = HM.empty
    in loop_blocks headers 0


  (* read a block at current pos and return it - private *)
  let read_block fd =
    Misc.read_bytes fd 24
    >>= function
      | None -> raise (Failure "here0")
      | Some s ->
        let _, header = M.decodeHeader s 0 in
        (* should check command is 'block' *)
        Misc.read_bytes fd header.length
        >>= function
          | None -> raise (Failure "here2")
          | Some payload -> return payload


  (* scan through blocks in the given sequence
    - perhaps insted of passing in seq and headers should pass just pos list *)
  let replay_blocks fd seq headers f x =
    let rec replay_blocks' seq x =
      x >>= fun x ->
      match seq with
        | hash :: tl ->
          let header = HM.find hash headers in begin
            match header.height mod 10000 with
              | 0 -> log @@ S.concat "" [
                "height "; string_of_int header.height;
              (*  " tx_count "; string_of_int x.tx_count;
                " output_count "; string_of_int x.output_count;
                " unspent "; x.unspent |> Utxos.cardinal |> string_of_int;
                " rvalues "; x.r_values |> RValues.cardinal |> string_of_int
              *)
                ]
              | _ -> return ()
          end
          >> Lwt_unix.lseek fd header.pos SEEK_SET
          >> read_block fd
          >>= fun payload ->
             f (x) payload
          >>= fun x ->
            replay_blocks' tl (return x)
        | [] -> return x
    in
    replay_blocks' seq (return x)

end
