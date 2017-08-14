open Bool
open Nat
open Cnt


type int = IZero | Pos of cnt | Neg of cnt


let ( == ) (p : int) (q : int) : bool =
    match p, q with
    | IZero, IZero -> True
    | Pos x, Pos y | Neg x, Neg y -> Nat.(Nat x == Nat y)
    | _, _ -> False


let ( > ) (p : int) (q : int) : bool =
    match p, q with
    | IZero, Neg _ | Pos _, IZero | Pos _, Neg _ -> True
    | IZero, _ | Neg _, IZero | Neg _, Pos _ -> False
    | Pos x, Pos y -> Nat.(Nat x > Nat y)
    | Neg x, Neg y -> Nat.(Nat x < Nat y)


let ( < ) (p : int) (q : int) : bool =
    match p, q with
    | Neg _, IZero | Neg _, Pos _ | IZero, Pos _ -> True
    | Pos _, IZero | Pos _, Neg _ | IZero, _ -> False
    | Pos x, Pos y -> Nat.(Nat x < Nat y)
    | Neg x, Neg y -> Nat.(Nat x > Nat y)


let ( <= ) (p : int) (q : int) : bool = ~~ (p > q)


let ( >= ) (p : int) (q : int) : bool = ~~ (p < q)


let inc x =
    match x with
    | Neg One -> IZero
    | Neg S x -> Neg x
    | IZero -> Pos One
    | Pos x -> Pos (S x)


let dec x =
    match x with
    | Pos One -> IZero
    | Pos S x -> Pos x
    | IZero -> Neg One
    | Neg x -> Neg (S x)


let of_nat (n : nat) : int =
    match n with
    | NZero -> IZero
    | Nat c -> Pos c


let neg x =
    match x with
    | IZero -> IZero
    | Neg y -> Pos y
    | Pos y -> Neg y


let abs x =
    match x with
    | Neg y -> Pos y
    | _ -> x


let rec ( + ) x y =
    (* O(x) *)
    match x with
    | IZero -> y
    | Neg One -> dec y
    | Neg S z -> Neg z + (dec y)
    | Pos One -> inc y
    | Pos S z -> Pos z + (inc y)


let ( - ) x y = x + (neg y)


let ( * ) x y =
    match x, y with
    | IZero, _ | _, IZero -> IZero
    | Pos xx, Pos yy | Neg xx, Neg yy -> Pos (xx * yy)
    | Neg xx, Pos yy | Pos xx, Neg yy -> Neg (xx * yy)


let ( // ) (x : int) (y : int) : int =
    match x, y with
    | _, IZero -> raise (Failure "Divide by zero.")
    | IZero, _ -> IZero
    | Pos xx, Pos yy | Neg xx, Neg yy -> of_nat Nat.(Nat xx // yy)
    | Neg xx, Pos yy | Pos xx, Neg yy -> neg @@ of_nat Nat.(Nat xx // yy)


let ( % ) x y = x - (y * (x // y))


let rec gcd x y =
    match y with
    | IZero -> x
    | _ -> gcd y (x % y)


let lcm x y = (x * y) // (gcd x y)


let relprime x y =
    match gcd x y with
    | Pos One -> True
    | _ -> False


let abs_as_nat (i : int) : nat =
    match i with
    | IZero -> NZero
    | Neg c -> Nat c
    | Pos c -> Nat c


let to_str x : string =
    match x with
    | Neg y -> "-" ^ (to_str @@ Nat y)
    | IZero -> "0"
    | Pos y -> to_str @@ Nat y


let one = Pos Cnt.One
let two = Pos (S Cnt.One)
let three = Pos (S (S Cnt.One))
let four = Pos (S (S (S Cnt.One)))
