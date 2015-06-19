
(* scan blocks and compute tx indexes
  native.
  corebuild -I src -package leveldb,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax  harvester/readblocks.native
*)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)  (* like bind, but second arg return type is non-monadic *)
let return = Lwt.return

module L = List
module CL = Core.Core_list
module S = String

module M = Message
module Sc = Scanner

open M

module Utxos = Map.Make(struct type t = string * int let compare = compare end)
module RValues = Map.Make(struct type t = string let compare = compare end)


type mytype =
{
  unspent : tx_out Utxos.t;
  tx_count : int;
  output_count : int;

  db :  Db.t ;  (* db of hashes, maybe change name *)
}


let log = Lwt_io.write_line Lwt_io.stdout

let coinbase = M.zeros 32


let format_tx hash i value script =
  " i " ^ string_of_int i
  ^ " value " ^ string_of_float ((Int64.to_float value ) /. 100000000.)
  ^ " tx " ^ M.hex_of_string hash
  ^ " script " ^ M.format_script script


type my_script =
  | Some of string
  | None
  | Strange


let fold_m f acc lst =
  let adapt f acc e = acc >>= fun acc -> f acc e in
  L.fold_left (adapt f) (return acc) lst

(*
let map_m f lst =
  let adapt f acc e = acc >> f e in
  L.fold_left (adapt f) (return ()) lst
*)


let process_output x (i,output,hash) =
    let script = M.decode_script output.script in
    let u = match script with
      (* pay to pubkey *)
      | BYTES s :: OP_CHECKSIG :: [] -> Some (s |> M.sha256 |> M.ripemd160)
      (* pay to pubkey hash*)
      | OP_DUP :: OP_HASH160 :: BYTES s :: OP_EQUALVERIFY :: OP_CHECKSIG :: [] -> Some s
      (* pay to script - 3 *)
      | OP_HASH160 :: BYTES s :: OP_EQUAL :: [] -> Some s
      (* null data *)
      | OP_RETURN :: BYTES _ :: [] -> None
      (* common for embedding raw data, prior to op_return  *)
      | BYTES _ :: [] -> None
      (* N K1 K2 K3 M CHECKMULTISIGVERIFY, addresses? *)
      | (OP_1|OP_2|OP_3) :: _ when List.rev script |> List.hd = OP_CHECKMULTISIG -> None

      | _ -> Strange
    in (
    match u with
      | Some hash160 ->
        begin
(*
          Db.get x.db hash160
          >>= function
            | Some found ->
              log @@ "found hash160 " ^ M.hex_of_string hash160 ^ " " ^ found
                ^ " " ^ format_tx hash i output.value script
            | _ ->
              (*log @@ "not found "
              >>*) return ()
*)
          return ()
        end
      | Strange ->
          log @@ "strange " ^ format_tx hash i output.value script
      | None ->
        return ()
    )
    >>
    return { x with
        output_count = succ x.output_count ;
        unspent =
          let key = (hash,i) in
          Utxos.add key output x.unspent
    }


let process_input x (i, (input : M.tx_in ), hash) =
    (* extract der signature and r,s keys *)
    let script = M.decode_script input.script in
    let ders = L.fold_left (fun acc elt ->
        match elt with
          | BYTES s -> (
              match M.decode_der_signature s with
                Some der -> der :: acc
                | None -> acc
          )
          | _ -> acc
    ) [] script
    (* lookup *)
    in
      let f x der =
        let r,s = der in

        Db.get x.db r
        >>= function
          (* match RValues.mem r x.r_values with  *)
          Some value -> begin
            (* ok, if we want the output value, then we have to store it *)
            let key = (input.previous,input.index) in
            match Utxos.mem key x.unspent with
              | true ->
                let output = Utxos.find key x.unspent in
                let value = output.value in
                let value = (Int64.to_float value) /. 100000000. in
                log @@ "found !!! " ^
                  " tx " ^ M.hex_of_string hash ^
                  " previous " ^ M.hex_of_string input.previous ^
                  " index " ^ string_of_int i ^
                  " r_value " ^ M.hex_of_string r ^
                  " value " ^ string_of_float value
                >>
                  return x
              | false -> raise ( Failure "ughh here" )
            end
          | None ->
            Db.put x.db r  ""
            >>
            return x (* { x with r_values = RValues.add r "mytx" x.r_values } *)

    in
    fold_m f x ders
    (* L.fold_left (adapt f) (return x) ders *)
    >|= fun x ->
    (* why can't we pattern match on string here ? eg. function *)
    if input.previous = coinbase then
      x
    else
      let key = (input.previous,input.index) in
      match Utxos.mem key x.unspent with
        | true -> { x with unspent = Utxos.remove key x.unspent }
        | false -> raise ( Failure "ughh here" )


let process_tx x (hash,tx) =
  begin
    match x.tx_count mod 100000 with
      | 0 -> log @@ S.concat "" [
        " tx_count "; string_of_int x.tx_count;
          " output_count "; string_of_int x.output_count;
          " unspent "; x.unspent |> Utxos.cardinal |> string_of_int;
          (* " rvalues "; x.r_values |> RValues.cardinal |> string_of_int *)
      ]
      | _ -> return ()
  end
  >>
    let x = { x with tx_count = succ x.tx_count } in
  (*log "tx" >> *)
    let group index a = (index,a,hash) in

    let inputs = L.mapi group tx.inputs in
    fold_m process_input x inputs
  >>= fun x ->
    let outputs = L.mapi group tx.outputs in
    fold_m process_output x outputs





let process_file () =
    Lwt_unix.openfile "blocks.dat.orig" [O_RDONLY] 0
    >>= fun fd ->
      log "scanning blocks..."
    >> Sc.scan_blocks fd
    >>= fun headers ->
      log "done scanning blocks - getting leaves"
    >>
      let leaves = Sc.get_leaves headers in
      log @@ "leaves " ^ (leaves |> L.length |> string_of_int)
    >>
      let longest = Sc.get_longest_path leaves headers in
      log @@ "longest " ^ M.hex_of_string longest
    >>
      log "computed leaves work "
    >>
      let seq = Sc.get_sequence longest headers in
      let seq = CL.drop seq 1 in (* we are missng the first block *)
      (* let seq = CL.take seq 200000 in *)
      (* let seq = [ M.string_of_hex "00000000000004ff6bc3ce1c1cb66a363760bb40889636d2c82eba201f058d79" ] in *)

      let filename = "rvalues" in
      Db.open_db filename
    >>= fun db ->
      log @@ "opened db " ^ filename
    >>
      let x = {
        unspent = Utxos.empty;
        db = db;
        tx_count = 0;
        (* r_values = RValues.empty;  *)
        output_count = 0;
      } in
      Sc.replay_tx fd seq headers process_tx x
    >>
      log "finished "


let () = Lwt_main.run (
  Lwt.catch (
    process_file
  )
  (fun exn ->
    (* must close *)
    let s = Printexc.to_string exn  ^ "\n" ^ (Printexc.get_backtrace () ) in
    log ("finishing - exception " ^ s )
    >> return ()
  )
)



