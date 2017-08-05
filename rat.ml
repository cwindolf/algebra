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
    p // g, den_of_int ((int_of_den q) // g)


let ( > ) (a, b : rat) (c, d : rat) : bool =
    (a * (int_of_den d)) > (c * (int_of_den b))


let ( <= ) (p : rat) (q : rat) : bool = ~~ (p > q)


let ( == ) (a, b : rat) (c, d : rat) : bool =
    (a * (int_of_den d)) == (c * (int_of_den b))


let ( + ) (a, b : rat) (c, d : rat) : rat =
    (a * (int_of_den d)) + (c * (int_of_den b)),
    den_of_int ((int_of_den b) * (int_of_den d))


let ( / ) (a, b : rat) (c, d : rat) : rat =
    a * (int_of_den d), den_of_int ((int_of_den b) * c)


let ( * ) (a, b : rat) (c, d : rat) : rat =
    a * c, den_of_int ((int_of_den b) * (int_of_den d))
