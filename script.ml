
(*
  there's multiple layers
    tx -> script -> der coding.

*)

open Core
open Message





(*

let formatTx2 tx =
  let format_scripts script_inputs =
    let formatted_inputs = List.map (fun x -> x |> decode_script |> format_script ) script_inputs in
    String.concat "\n"  formatted_inputs in
	let script_inputs = List.map (fun x -> (x:tx_in).script) tx.inputs in
  let script_outputs = List.map (fun x -> (x:tx_out).script) tx.outputs in
  let inputs = format_scripts script_inputs in
  let outpus = format_scripts script_outputs in
  (* " hash " ^ hex_of_string tx.hash  *)
  "\n version " ^ string_of_int tx.version
  ^ "\n inputsCount " ^(string_of_int @@ List.length tx.inputs)
  ^ "\n " ^ inputs (* formatInputs tx.inputs *)
  ^ "\n outputsCount " ^ (string_of_int @@ List.length tx.outputs )
  ^ "\n " ^ outpus (* formatOutputs tx.outputs *)
  ^ "\n lockTime " ^ string_of_int tx.lockTime
*)

let tx  =
  let in_channel = open_in "test_data/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" in
  let s = Core.In_channel.input_all in_channel in
  let () = close_in in_channel in
  (* Decoding the tx ought to decode the script as well
    - perhaps not, should we also decode the signature der as well...
      Do we do it all at once?

      so we decode it. Now we will loop through stuff 
      - OK, if we don't decode at the time, we want a complete structure representing the entire tx
      with decoded scripts...  so we can pass it around...
      - or alternatively we just write it as a transform
      - We have to reassemble the entire tx. to do signature verification. 
      and deconstruct and re encode the thing... 
      - we could have the inputs and outputs  as a variant...
  *)
  let _, tx = decodeTx s 0 in
  tx



let () = Printf.printf "%s\n" @@ formatTx tx


(*
let tokens = List.rev @@ decode_script output.pkScript
let () = Printf.printf "length %d\n" (List.length tokens)
let () = List.iter (fun x -> Printf.printf "%s " (format_token x)) tokens
let () = Printf.printf "\n"
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
  String.concat "" (fun x -> (format_token x)) tokens 
*) 


(* TODO should have a string concat to string here *)

(* should we be decoding the script, in the tx decoding entirely? *)

