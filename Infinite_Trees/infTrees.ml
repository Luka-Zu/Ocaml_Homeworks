type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let rec layer_tree n = LNode(n, (fun () -> layer_tree (n+1)), (fun () -> layer_tree (n+1)))

let rec interval_tree left right = 
    LNode((left,right), (fun () -> 
    interval_tree left ((left+.right)/.2. )), (fun () ->interval_tree ((left+.right)/.2. ) right ))

let rec rational_tree n d = 
    LNode((n,d), (fun () -> rational_tree n (d+1)), (fun () -> rational_tree (n+1) d))

let rec top n t=
    match n , t with 
    0 , _ -> Empty
    |num , LNode(cur, fun1 , fun2) -> Node(cur , (top (n-1)  (fun1()) ) , (top (n-1)  (fun2()) ) )
    
let rec map f tree = match tree with LNode(cur, fun1 ,fun2 ) -> 
    LNode ( (f cur), (fun()-> map f (fun1() ) ) , (fun()-> map f (fun2())) ) 

let find predicate tree = let rec aux pred queue = match queue with (LNode(a,f1,f2) as tr)::rest-> 
    if pred a then tr else aux pred (rest@ [(f1());(f2())]) 
    |[] -> failwith "NO Such CASE"
    in aux predicate [tree]
