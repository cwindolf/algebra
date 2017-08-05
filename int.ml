open Bool
open Nat

type sign = SZero | Pos | Neg
type int = IZero | Next of int | Prev of int ;;


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
    | IZero -> IZero
    | Next _ -> pos_to_neg x IZero
    | Prev _ -> neg_to_pos x IZero


let abs x =
    match x with
    | Prev _ -> neg x
    | _ -> x


let sgn x =
    match x with
    | Prev _ -> Neg
    | IZero -> SZero
    | Next _ -> Pos


let rec ( + ) x y =
    match x with
    | IZero -> y
    | Prev z -> z + (dec y)
    | Next z -> z + (inc y)


let ( - ) x y = x + (neg y)


let ( * ) x y =
    let rec helper x y z =
        match x with
        | IZero -> z
        | Prev a -> helper a y (z - y)
        | Next a -> helper a y (z + y)
    in helper x y IZero


let ( // ) x y =
    let rec helper c x y =
        match sgn x with
        | SZero | Neg -> c
        | Pos -> helper (inc c) (x - y) y in 
    match sgn x, sgn y with
    | SZero, _ -> IZero
    | _, SZero -> raise (Failure "divide by zero")
    | Neg, Neg -> helper IZero (neg x) (neg y)
    | Pos, Pos -> helper IZero x y
    | Neg, Pos -> neg (helper IZero (neg x) y)
    | Pos, Neg -> neg (helper IZero x (neg y))


let ( %% ) x y = x - (y * (x // y))


let rec gcd x y =
    match y with
    | IZero -> x
    | _ -> gcd y (x %% y)


let lcm x y = (x * y) // (gcd x y)


let relprime x y =
    match gcd x y with
    | Next IZero -> True
    | _ -> False


let abs_as_nat (i : int) : nat =
    let rec nat_of_pos_int i n =
    match i with
    | IZero -> n
    | Next p -> nat_of_pos_int p (Succ n)
    | _ -> raise (Failure "bad") in
    match sgn i with
    | SZero -> NZero
    | Neg -> nat_of_pos_int (neg i) NZero
    | Pos -> nat_of_pos_int i NZero

let ( > ) (p : int) (q : int) : bool =
    match sgn p, sgn q with
    | Pos, Pos -> (abs_as_nat p) > (abs_as_nat q)
    | Neg, Neg -> (abs_as_nat (neg q)) > (abs_as_nat (neg p))
    | Pos, _ -> True
    | Neg, _ -> False
    | SZero, Pos -> False
    | SZero, SZero -> False
    | SZero, Neg -> True


let ( <= ) (p : int) (q : int) : bool = ~~ (p > q)


let ( == ) (p : int) (q : int) : bool =
    match sgn p, sgn q with
    | SZero, SZero -> True
    | Pos, Pos | Neg, Neg -> Nat.((abs_as_nat p) == (abs_as_nat q))
    | _, _ -> False
