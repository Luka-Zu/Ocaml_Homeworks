type graph = (int * float * int) list
let insert ls a  = if not (List.mem a ls) then a::ls else ls
let translate gr = 
    let rec aux gr1 nodes =
        match gr1 with [] -> nodes
        |(a,b,c)::t ->  aux t (insert (insert nodes a) c)
    in aux gr []
(* ///////////////////////////////////// *)
let rec verticesToList graph curgraph ver = 
  let rec forOne gr a =
    match gr with [] -> []
    |(x,z,y)::rest -> if (x==a || y == a ) then (x,z,y) ::(forOne rest a) else (forOne rest a)
  in 
  match ver with [] -> curgraph
  |l::rest ->  verticesToList graph (curgraph @ forOne graph l) rest


(* ///////////////////////////////////////////////// *)
let rec merge_sort = 
    let rec halve = function
    | [] 
    | [_] as t1 -> t1, []
    | h::t ->
        let t1, t2 = halve t in
          h::t2, t1
    in let rec merge = function
        | list, []
        | [], list -> list
        | ((a,h1,c)as k )::t1, ((b,h2,d)as j)::t2 ->
            if h1 <= h2 then
            k :: merge (t1, j::t2)
            else
            j :: merge (k::t1, t2)
    in function
    | [] ->[]
    | [_] as list -> list
    | list ->
        let l1, l2 = halve list in
          merge (merge_sort l1, merge_sort l2)


(* main *)
let rec helpMe graph currentGraph toCheck currentVertices = 
    let toCheck = merge_sort toCheck in 
    match toCheck with 
    []-> currentGraph
    |(l,v,r)::rest-> if not(List.mem l currentVertices  && List.mem r currentVertices ) then 
    let currentVertices = if not(List.mem l currentVertices ) then l::currentVertices else r::currentVertices 
    in 
    helpMe graph ((l,v,r)::currentGraph) (verticesToList graph [] currentVertices) currentVertices
    
    else 
    helpMe graph (currentGraph) (rest) currentVertices
    

let mst graph = if graph=[] then []
    else let a = List.nth graph 0 in
    helpMe graph [a] (verticesToList graph [] (translate [a]) ) (translate [a])
