
(* Purely functional queue, adapted https://gist.github.com/23Skidoo/1347308 *)


type 'a t = MyQueue of 'a list * 'a list
 
let empty = MyQueue ([], []);;
 
let add q elt = match q with
| MyQueue (front, back) -> MyQueue (elt::front, back);;
 
let take q = match q with
| MyQueue ([], []) -> raise (Invalid_argument "Empty queue!")
| MyQueue (front, b::bs) -> b, (MyQueue (front, bs))
| MyQueue (front, []) -> 
  let back = List.rev front
  in (List.hd back, MyQueue ([], List.tl back));; 


let length = function 
  | MyQueue (front,back) -> List.length front +  List.length back 

