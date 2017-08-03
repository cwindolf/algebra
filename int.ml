open Bool

type sign = Z | Pos | Neg
type int = Zero | Next of int | Prev of int ;;


let inc x =
    match x with
    | Prev y -> y
    | _ -> Next x


let dec x =
    match x with
    | Next y -> y
    | _ -> Prev x


let neg x =
    let rec neg_to_pos inp out =
        match inp with
        | Prev z -> neg_to_pos z (inc out)
        | _ -> out in
    let rec pos_to_neg inp out =
        match inp with
        | Next z -> pos_to_neg z (dec out)
        | _ -> out in
    match x with
    | Zero -> Zero
    | Next _ -> pos_to_neg x Zero
    | Prev _ -> neg_to_pos x Zero


let abs x =
    match x with
    | Prev _ -> neg x
    | _ -> x


let sgn x =
    match x with
    | Prev _ -> Neg
    | Zero -> Z
    | Next _ -> Pos


let rec add x y =
    match x with
    | Zero -> y
    | Prev z -> add z (dec y)
    | Next z -> add z (inc y)


let sub x y = add x (neg y)


let mul x y =
    let rec helper x y z =
        match x with
        | Zero -> z
        | Prev a -> helper a y (sub z y)
        | Next a -> helper a y (add z y)
    in helper x y Zero


let floordiv x y =
    let rec helper c x y =
        match sgn x with
        | Z | Neg -> c
        | Pos -> helper (inc c) (sub x y) y in 
    match sgn x, sgn y with
    | Z, _ -> Zero
    | _, Z -> raise (Failure "divide by zero")
    | Neg, Neg -> helper Zero (neg x) (neg y)
    | Pos, Pos -> helper Zero x y
    | Neg, Pos -> neg (helper Zero (neg x) y)
    | Pos, Neg -> neg (helper Zero x (neg y))


let (%%) x y = sub x (mul y (floordiv x y))


let rec gcd x y =
    match y with
    | Zero -> x
    | _ -> gcd y (x %% y)


let lcm x y = floordiv (mul x y) (gcd x y)


let relprime x y =
    match gcd x y with
    | Next Zero -> True
    | _ -> False
