
open Core
open Message 

let in_channel = open_in "dumps/0e7b95f5640018b0255d840a7ec673d014c2cb2252641b629038244a6c703ecb" in
let s = Core.In_channel.input_all in_channel in
let () = close_in in_channel in

let _, tx = decodeTx s 0 in 
Printf.printf "* got tx!!!\n%s\n" (formatTx tx )





