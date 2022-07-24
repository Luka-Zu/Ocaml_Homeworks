let starts_with haystack hay = failwith "TODO starts_with"
module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string : t -> string   (* field element to string *)
  val add : t -> t -> t         (* addition *)
  val mul : t -> t -> t         (* multiplication *)
  val sub : t -> t -> t         (* subtraction *)
  val div : t -> t -> t         (* division *)
  val add_inv : t -> t          (* additive inverse *) 
  val mul_inv : t -> t          (* multiplicative inverse *)
end

module type RationalField =
  sig
    include Field with type t = int * int
    type t = int * int          (* rationals are represented as pairs of int *)
    exception Bad_rational of string
    val standard_form : t -> t  (* standard from of a rational number *)
    val to_float : t -> float   (* decimal expansion *)
    val from_int : int -> t     (* integer to rational conversion *)          
  end


module type GaussianRationalField =
  sig
    include Field with type t = (int * int) * (int * int)
    (* Gaussian rationals are represented as pairs of rationals *)
    exception Division_by_zero of string
    val from_rational : (int * int ) -> t   (* rational to complex *)     
    val conj : t -> t                       (* conjugate *)
    val re : t -> (int * int)               (* real part *)
    val im : t -> (int * int)               (* inaginary part *)
  end

module Rationals : RationalField =
    struct
    type t = int * int
    exception Bad_rational of string
    let zero = (0,1) 
    let one = (1,1)
    let rec divTillNoMore (a,b) acc = if acc > b then (a,b) else if ((a mod acc = 0) && (b mod acc = 0))
        then divTillNoMore( (a/acc) , (b/acc )) 2 else divTillNoMore (a,b) (acc+1) 
    let standard_form (a,b) = if b < 0 then let a = -a in let b = -b in divTillNoMore (a,b) 2  else divTillNoMore (a,b) 2

    let to_string (n,d) = let (n',d') = standard_form (n,d) in 
    if d' = 1 then (string_of_int n) 
    else if n' = 0 then "0"
    else (string_of_int n')^"/"^(string_of_int d')
 
   
    let from_int a = (a,1)
    let to_float (a,b) = (float_of_int a) /. (float_of_int b)

    let add (a,b) (c,d) = standard_form (( a*d+b*c ),(b*d) )
    let mul (a,b) (c,d) = standard_form((a*c),(b*d))
    let sub (a,b) (c,d) = standard_form((a*d - c*b) , (b*d))
    let div (a,b) (c,d) = if c=0 then raise (Bad_rational "can not divede by zero") else standard_form (mul (a,b) (d,c))
    let add_inv (a,b) = sub zero (a,b)
    let mul_inv (a,b) = div one (a,b)
    let compare (a,b) (c,d) =let(a,b)= standard_form (a,b) in let (c,d) = standard_form (c,d) in  let sign = a*d - b*c in if sign > 0 then 1 else (if sign = 0 then 0 else (-1) )

    end
module GaussianRationals : GaussianRationalField =
    struct
    open Rationals
    type t = (int * int) * (int * int)
    exception Division_by_zero of string
    let zero = (Rationals.zero , Rationals.zero)
    let one = (Rationals.one , (0,1))
    let compare ((r1,i1):t ) ( (r2,i2) :t ):int = let x = Rationals.compare r1 r2 in 
      if x <> 0 then x else ( Rationals.compare i1 i2 ) 

    let to_string (r,i) = let ((a,b), (c,d)) = (Rationals.standard_form r, standard_form i)
        in (string_of_int a) ^ "/" ^ (string_of_int b) ^ (string_of_int c) ^ "/" ^ (string_of_int d) ^"*I"

    let from_rational r =
        ((r) ,(0,1))
    
    let add (r1,i1) (r2,i2)  = ( (Rationals.add r1 r2) , (Rationals.add i1 i2) )
    let mul (r1,i1) (r2,i2)  = ((Rationals.sub (Rationals.mul r1 r2) (Rationals.mul i1 i2) ), ( Rationals.add (Rationals.mul r1 i2) (Rationals.mul i1 r2)) ) 
    let sub (r1,i1) (r2,i2)  = ( (Rationals.sub r1 r2) , (Rationals.sub i1 i2) )
    let div ((x1, y1), (x2,y2)) ((x3,y3), (x4,y4)) = try let n = (Rationals.mul (x3,y3) (x3,y3) 
    |> Rationals.add (Rationals.mul (x4,y4) (x4,y4)) |> Rationals.div (Rationals.sub (Rationals.mul (x2,y2) (x3,y3)) (Rationals.mul (x1,y1) (x4,y4)))) in
    let m = (Rationals.mul (x3,y3) (x3,y3) |> Rationals.add (Rationals.mul (x4,y3) (x4,y3)) 
    |> Rationals.div ((Rationals.mul (x1,y1) (x3,y3)) |> Rationals.add (Rationals.mul (x2,y2) (x4,y4)))) in 
    m,n with _ -> raise  (Division_by_zero "can not divide by 0") 
    let conj (r,i) = (r,(Rationals.add_inv (i))) 

    let add_inv (r,i) = (( Rationals.add_inv r) , ( Rationals.add_inv i) ) 
    let mul_inv ((a,b),(c,d)) = try div (conj ((a,b),(c,d))) ((Rationals.add ( Rationals.mul (a,b) (a,b) )  ( Rationals.mul (c,d) (c,d) )), (0,1)) 
    with _ ->  raise ( Division_by_zero "Can not divide by zero" )
    let re (r,i) = Rationals.standard_form r
    let im (r,i) = Rationals.standard_form i 
    end
