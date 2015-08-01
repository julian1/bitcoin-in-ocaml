
module M = Message
module L = List


let (<|) f g x = f(g(x))

(*
  TODO move this
let compare a b =
  let za = A.z_of_string a in
  let zb = A.z_of_string b in
  if Z.gt za zb then 1
  else if Z.equal za zb then 0
  else -1
*)

(* TODO should be able to get rid of all the reversing by doing once at the start and finish *)

(*
  let concat_hash a b = M.sha256d a ^  b
and
  let lst = L.map M.strrev lst in
  let e = root' lst in
  M.strrev e
*)


let concat_hash a b = M.strrev a ^ M.strrev b |> M.sha256d |> M.strrev

let root lst =
  let rec root' lst =
    match lst with
      | e :: [] -> e
      | lst ->
        let lst =
          (* duplicate last element if odd number *)
          if (L.length lst) mod 2 = 1 then
            let lst  = L.rev lst in
            L.hd lst :: lst |> L.rev
          else
            lst
        in
        let aggregate (lst,previous) e =
          match previous with
            | None -> lst, Some e
            | Some e2 -> concat_hash e2 e::lst, None
        in
        let lst, None = L.fold_left aggregate ([],None) lst in
        let lst = L.rev lst in
        root' lst
  in
  root' lst


