open Bool
open Int

type num = int
type den = One | Succ of den
type rat = num * den


let sgn (p, _ : rat) : sign = sgn p


let rec int_of_den (d : den) : int =
    match d with
    | One -> Next IZero
    | Succ e -> Next (int_of_den e)


let rec den_of_int (i : int) : den =
    match Int.sgn i with
    | Neg | SZero -> raise (Failure "bad denominator...")
    | Pos ->
        match i with
        | Next IZero -> One
        | Next p -> Succ (den_of_int p)
        | _ -> raise (Failure "unreachable")


let least_terms (p, q : rat) : rat =
    let g = gcd p (int_of_den q) in
    floordiv p g, den_of_int (floordiv (int_of_den q) g)


let (>>) (a, b : rat) (c, d : rat) : bool =
    (mul a (int_of_den d)) >> (mul c (int_of_den b))


let (<=) (p : rat) (q : rat) : bool = ~~ (p >> q)


let (==) (a, b : rat) (c, d : rat) : bool =
    (mul a (int_of_den d)) == (mul c (int_of_den b))


let (++) (a, b : rat) (c, d : rat) : rat =
    add (mul a (int_of_den d)) (mul c (int_of_den b)),
    den_of_int (mul (int_of_den b) (int_of_den d))


let ( ** ) (a, b : rat) (c, d : rat) : rat =
    mul a c, den_of_int (mul (int_of_den b) (int_of_den d))

let (//) (a, b : rat) (c, d : rat) : rat =
    mul a (int_of_den d), den_of_int (mul (int_of_den b) c)

