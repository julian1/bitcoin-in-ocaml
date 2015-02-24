
open Core
open Message 

let get_output () = 
  let in_channel = open_in "dumps/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  let _, tx = decodeTx s 0 in 
  let () = Printf.printf "* got tx!!!\n%s\n" (formatTx tx) in
  let output_1 :: _ = tx.outputs in
  output_1


let (output : tx_out )  = get_output ()

(*
let output = get_output () 
let() = Printf.printf "* length %d\n" len 
let pos, arg = decs_ s pos len in
*)
(* use the same byte decoder *)

type t =
  | Male
  | OP_DUP
  | Unknown of int
;;

(* might just about use the takeN recursive function  
  or just while pos is not at the end...
  it's not a fold ... 
*)

let ff s =
  let rec f (pos: int) acc =

    if pos < strlen s then
      let pos, c = decodeInteger8 s pos in
      let x = match c with 
        118 -> OP_DUP
        | _ -> Unknown c
      in pos, x::acc
    else acc

  in let _, result = f 0 []
  in result

(*  
  in
  let _, result = f 0 [] in
  123 

let s = output.pkScript in
in
  let format_ x = match x with 
    | OP_DUP -> "OP_DUP"
    | Unknown v -> "Unknown " ^ string_of_int v
*)

let fuck = ff output.pkScript 













