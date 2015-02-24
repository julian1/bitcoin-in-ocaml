
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
  | OP_HASH160
  | OP_DUP
  | OP_EQUALVERIFY
  | OP_CHECKSIG 
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
      (* let () = Printf.printf "whoot pos %d\n" pos in *)
      let x = match c with
        118 -> OP_DUP
        | 169 -> OP_HASH160
        | 136 -> OP_EQUALVERIFY
        | 172 -> OP_CHECKSIG
        | _ -> Unknown c
      in f pos (x::acc)
    else pos, acc

  in let _, result = f 0 []
  in result


let fuck = List.rev ( ff output.pkScript )

let () = Printf.printf "length %d\n" (List.length fuck)

let f x =
  let u = match x with
  | OP_DUP -> "OP_DUP"
  | OP_HASH160 -> "OP_HASH160"
  | OP_EQUALVERIFY-> "OP_EQUALVERIFY"
  | OP_CHECKSIG -> "OP_CHECKSIG"
  | Unknown c -> "Unknown %d" ^ string_of_int c
  in  
    Printf.printf " - %s\n" u



let () = List.iter f fuck








