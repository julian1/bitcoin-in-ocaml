
open Core
open Message



type t =
  | Bytes of string
  | Unknown of int
  | OP_DUP
  | OP_EQUAL
  | OP_EQUALVERIFY
  | OP_HASH160
  | OP_CHECKSIG
;;


let decode_script s =
  let rec f pos acc =
    if pos < strlen s then
      let pos, c = decodeInteger8 s pos in
      (* let () = Printf.printf "whoot pos %d\n" pos in *)

      if ( c >= 1 && c <= 78) then
        let pos, len =
          match c with
            | 76 -> decodeInteger8 s pos
            | 77 -> decodeInteger16 s pos
            | 78 -> decodeInteger32 s pos
            | _ -> pos, c
          in
        let pos, bytes = decs_ s pos len in
        f pos (Bytes bytes::acc)

      else
        let op = match c with
        | 118 -> OP_DUP
        | 135 -> OP_EQUAL 
        | 136 -> OP_EQUALVERIFY
        | 169 -> OP_HASH160
        | 172 -> OP_CHECKSIG
        | _ -> Unknown c
        in f pos (op::acc)
    else pos, acc
  in let _, result = f 0 []
  in List.rev result


let format_token x =
  match x with
  | OP_DUP -> "OP_DUP"
  | OP_EQUAL -> "OP_EQUAL"
  | OP_HASH160 -> "OP_HASH160"
  | OP_EQUALVERIFY-> "OP_EQUALVERIFY"
  | OP_CHECKSIG -> "OP_CHECKSIG"
  | Bytes c -> "Bytes " ^ hex_of_string c
  | Unknown c -> "Unknown " ^ string_of_int c


let format_script tokens = 
  String.concat " " @@ List.map format_token tokens  

(* 
  String.concat "" (fun x -> (format_token x)) tokens 
*) 


(* TODO should have a string concat to string here *)

(* should we be decoding the script, in the tx decoding entirely? *)  

(*
let tx  =
  let in_channel = open_in "dumps/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  let _, tx = decodeTx s 0 in
  tx
*)


(*
  tx.inputs, 
  tx.outputs 
*)

(*
let inputs, outputs = get_outputs ()

let f (script: string ) = 
  let tokens = decode_script script in
  Printf.printf "%s\n" (format_script tokens )

let () = List.iter (fun x ->  f (x : tx_in ).script ) inputs 
let () = List.iter (fun x ->  f (x : tx_out ).script ) outputs 
*)


(*
let formatTx2 tx = 

  let format_scripts script_inputs = 
    let formatted_inputs = List.map (fun x -> x |> decode_script |> format_script ) script_inputs in
    String.concat "\n"  formatted_inputs in

    let script_inputs = List.map (fun x -> (x:tx_in).script) tx.inputs in
    let script_outputs = List.map (fun x -> (x:tx_out).script) tx.outputs in

   let inputs = format_scripts script_inputs in 
   let outpus = format_scripts script_outputs in 

  " hash " ^ hex_of_string tx.hash 
  ^ "\n version " ^ string_of_int tx.version 
  ^ "\n inputsCount " ^(string_of_int @@ List.length tx.inputs)
  ^ "\n " ^ inputs (* formatInputs tx.inputs *)
  ^ "\n outputsCount " ^ (string_of_int @@ List.length tx.outputs )
  ^ "\n " ^ outpus (* formatOutputs tx.outputs *)
  ^ "\n lockTime " ^ string_of_int tx.lockTime

*)


(*
let () = Printf.printf "%s\n" @@ formatTx2 tx
*)

(*
let tokens = List.rev @@ decode_script output.pkScript
let () = Printf.printf "length %d\n" (List.length tokens)
let () = List.iter (fun x -> Printf.printf "%s " (format_token x)) tokens
let () = Printf.printf "\n"
*)






