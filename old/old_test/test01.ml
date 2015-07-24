

let db = LevelDB.open_db "mydb" in

let () = LevelDB.put db "myhash" "myval" in 



let v = LevelDB.get db "myhash" in 
let () = match v with 
  | Some s -> Printf.printf "got %s\n" s
  | None -> Printf.printf "none\n"
in

let () = LevelDB.close db in 
()
 

