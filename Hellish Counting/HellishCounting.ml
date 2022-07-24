#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

open Thread;;
open Event;;




(* spwan counter for task 1 2 4  *)
let spawn_counter n =
  let rec aux i =
    if i <= n then 
    let s = Printf.sprintf "Thread ID,NUM: %d %d" (id (self ())) i in
    print_endline s;
    aux (i+1)
    else ()
  in
  create aux 0

let rec init cur high acum toCount= if cur == high then List.rev acum 
else init (cur+1) high ((spawn_counter toCount)::acum) toCount
(* task 2  *)
let run_counters m n =
  let counters = init 0 m [] n in
  List.iter join counters;
  print_endline "\n"

(* task 4  *)
let run_counters3 m n =
  let rec aux cur =
  if cur >= m then ()
  else  let th = spawn_counter n in
  join th;
  aux (cur+1)
  in aux 0



(* orginezed  *)
(* task 3 *)
exception Invalid_input;;
			                  
let channelCreator n = 
	let mainChannel = new_channel ()
  in let rec counter (max,cur) = 
    let th = sync(receive mainChannel)  (* daayovne sanam ar miigeb rame mesijs  *) 
    in if cur > max then sync(send mainChannel "DONE")
    else print_string ((Printf.sprintf "Thread ID,NUM %d %d" (Thread.id(Thread.self ())) cur) ^ ";  \n" ) ; 
    sync(send mainChannel "PRINTING");
    counter (max,(cur+1))
    in let a =Thread.create counter (n,0) in 
  mainChannel 

let rec initialize cur high acum toCount= if cur == high then List.rev acum 
else initialize (cur+1) high ((channelCreator toCount)::acum) toCount

let run_counters1 m n = 
  let connectedChannel = initialize 0 m [] n 
  in let rec iter queue = match queue with [] -> () |
    head::tail -> let a = sync ( send head "START")  in
            match sync(receive head) with "PRINTING"-> iter (tail@[head])
			| "DONE" ->iter tail
            | _ -> raise Invalid_input 
  in iter connectedChannel

(* orginezed  finished*)

