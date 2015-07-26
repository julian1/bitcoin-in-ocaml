
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

(* TODO could get rid of the rev by doing it once at the start and finish *)

let hash = M.strrev <| M.sha256d 

let concat_hash a b = hash (M.strrev a ^ M.strrev b)


let rec root lst =
  match lst with
    | e :: [] -> e
    | lst ->
      let lst =
        if (L.length lst) mod 2 = 1 then
          (* duplicate last element *)
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
      root lst

