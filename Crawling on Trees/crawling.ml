type tree = Empty | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop


(* print a graphical representation (dot) of a binary tree (2. argument) to a file (1. argument) *)
let print_tree filename btree = 
  let file = open_out filename in
  Printf.fprintf file "digraph Tree {\n";
  let rec print next_id = function Empty -> 
    Printf.fprintf file "\tn%d[shape=rectangle,label=\"\"];\n" next_id; next_id + 1, next_id
  | Node (x, l, r) ->
    let node_id = next_id in
    Printf.fprintf file "\tn%d[label=\"%d\"];\n" node_id x;
    let next_id, lid = print (next_id + 1) l in
    let next_id, rid = print next_id r in 
    (Printf.fprintf file "\tn%d -> n%d[label=\"L\"];\n" node_id lid);
    (Printf.fprintf file "\tn%d -> n%d[label=\"R\"];\n" node_id rid);
    next_id, node_id
  in
  ignore(print 0 btree);
  Printf.fprintf file "}";
  close_out file

let rec crawlUp startTree (stackOfTrees:tree list) (stackOfPreviousCommands:command list)=
    match stackOfTrees,stackOfPreviousCommands with 
    a::restTrees , Left::restCms ->( match a with Node(x,l,r)-> Node(x,startTree,r)|_->Empty )
    |a::restTrees , Right::restCms -> ( match a with Node(x,l,r)-> Node(x,l,startTree)|_->Empty )
    |a,b->Empty
let rec crawler (cmds:command list) (tree1:tree) (stackOfTrees:tree list) (stackOfPreviousCommands:command list) (stack:tree list):tree=
    let rec fillUpWithUps lst =match lst with []->[]|h::t-> Up::fillUpWithUps t in
    if cmds=[] && List.length stackOfPreviousCommands!=0 then crawler (fillUpWithUps stackOfPreviousCommands) tree1 stackOfTrees stackOfPreviousCommands stack else 
    match cmds with | Left::rest -> let stackOfTrees=tree1::stackOfTrees in let stackOfPreviousCommands=Left::stackOfPreviousCommands in 
              crawler rest (match tree1 with |Node (a,left,right)->left |Empty->Empty) stackOfTrees stackOfPreviousCommands stack
    | Right::rest -> let stackOfTrees=tree1::stackOfTrees in let stackOfPreviousCommands=Right::stackOfPreviousCommands in 
              crawler rest ((match tree1 with |Node (a,left,right)->right |Empty->Empty)) stackOfTrees stackOfPreviousCommands stack
    | Delete::rest -> crawler rest  Empty stackOfTrees stackOfPreviousCommands stack
    | New(x)::rest->  crawler rest  (Node(x, Empty , Empty )) stackOfTrees stackOfPreviousCommands stack
    | Push ::rest -> let stack= tree1::stack in crawler rest  tree1 stackOfTrees stackOfPreviousCommands stack
    | Pop :: rest -> let x = match stack with h::t -> h | []-> Empty in let stack = 
            match stack with a::restt ->restt | [] -> [] in crawler rest x stackOfTrees stackOfPreviousCommands stack
    | Up::rest -> let newTree = crawlUp tree1 stackOfTrees stackOfPreviousCommands 
    in let stackOfTrees=match stackOfTrees with h::t->t|[]->[] in 
    let stackOfPreviousCommands = match stackOfPreviousCommands with h::t->t|[]->[] 
    in crawler rest newTree stackOfTrees stackOfPreviousCommands stack
    | []->tree1
let crawl cmds tree = crawler cmds tree [] [] []
    
    
    
