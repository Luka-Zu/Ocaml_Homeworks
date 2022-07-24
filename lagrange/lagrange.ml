let  lagrange lst =
    let  make_g  lst =
    (fun x xj->
        let rec g x2 xj2  lst2= 
            match lst2 with 
            [] -> 1.
            |(a,b)::tail -> if xj2 <> a then ( (x2-.a)/.(xj2-.a) ) *. (g x2 xj2 tail)
            else (g x2 xj2 tail) 
        in g x xj  lst
    )
    in let g = make_g lst in 
    (fun x -> 
    let rec  aux  lst = 
        match lst with []->0.0
        |(a,b)::t->  b*.(g x a )+. (aux t)
    in aux lst) 
