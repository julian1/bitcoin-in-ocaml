
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

let pos = 0 in
let s = output.pkScript in
let pos, c = decodeInteger8 s pos in

let x = match c with 
  118 -> OP_DUP
  | _ -> Unknown 123
in

Printf.printf "* output %d\n" c











