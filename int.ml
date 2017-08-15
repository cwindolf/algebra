open Nat
open Cnt


type int = IZero | Pos of cnt | Neg of cnt


let ( == ) (p : int) (q : int) : bool =
    match p, q with
    | IZero, IZero -> true
    | Pos x, Pos y | Neg x, Neg y -> Nat.(Nat x == Nat y)
    | _, _ -> false


let ( > ) (p : int) (q : int) : bool =
    match p, q with
    | IZero, Neg _ | Pos _, IZero | Pos _, Neg _ -> true
    | IZero, _ | Neg _, IZero | Neg _, Pos _ -> false
    | Pos x, Pos y -> Nat.(Nat x > Nat y)
    | Neg x, Neg y -> Nat.(Nat x < Nat y)


let ( < ) (p : int) (q : int) : bool =
    match p, q with
    | Neg _, IZero | Neg _, Pos _ | IZero, Pos _ -> true
    | Pos _, IZero | Pos _, Neg _ | IZero, _ -> false
    | Pos x, Pos y -> Nat.(Nat x < Nat y)
    | Neg x, Neg y -> Nat.(Nat x > Nat y)


let ( <= ) (p : int) (q : int) : bool = not (p > q)


let ( >= ) (p : int) (q : int) : bool = not (p < q)


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


let abs_as_nat (i : int) : nat =
    match i with
    | IZero -> NZero
    | Neg c -> Nat c
    | Pos c -> Nat c


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


let rec euc (x : int) (y : int) : int * int =
    match x, y with
    | _, IZero -> raise (Failure "Divide by zero.")
    | _, Neg b ->
        let q, r = euc x (Pos b) in neg q, r
    | Neg a, _ ->
        let q, r = euc (Pos a) y in
        (match r with
         | IZero -> neg q, IZero
         | _ -> neg q, neg r)
    | _, Pos b ->
        let nq, nr = Nat.euc (abs_as_nat x) (Nat b) in
        of_nat nq, of_nat nr


let ( // ) (x : int) (y : int) : int =
    let q, _ = euc x y in q


let ( % ) (x : int) (y : int) : int =
    let _, r = euc x y in r


let rec gcd x y =
    match y with
    | IZero -> abs x
    | _ -> gcd y (x % y)


let lcm x y =
    match y with
    | IZero -> IZero
    | _ -> (abs (x * y)) // (gcd x y)


let relprime x y =
    match gcd x y with
    | Pos One -> true
    | _ -> false


let as_cnt (i : int) : cnt =
    match i with
    | Pos denominator -> denominator
    | Neg _ | IZero -> failwith "Int.as_cnt of zero or negative."


let to_str x : string =
    match x with
    | Neg y -> "-" ^ (to_str @@ Nat y)
    | IZero -> "0"
    | Pos y -> to_str @@ Nat y


let print x = x |> to_str |> print_endline


let one = Pos Cnt.One
let two = Pos (S Cnt.One)
let three = Pos (S (S Cnt.One))
let four = Pos (S (S (S Cnt.One)))
