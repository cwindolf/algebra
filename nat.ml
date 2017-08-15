open Cnt


type nat = NZero | Nat of cnt


let ( == ) (a : nat) (b : nat) : bool =
    match a, b with
    | NZero, NZero -> true
    | Nat x, Nat y -> Cnt.(x == y)
    | _, _ -> false


let ( > ) (a : nat) (b : nat) : bool =
    match a, b with
    | NZero, _ -> false
    | _, NZero -> true
    | Nat x, Nat y -> Cnt.(x > y)


let ( < ) (a : nat) (b : nat) : bool =
    match a, b with
    | NZero, Nat _ -> true
    | _, NZero -> false
    | Nat x, Nat y -> Cnt.(x < y)


let ( <= ) (a : nat) (b : nat) : bool =
    not (a > b)


let ( >= ) (a : nat) (b : nat) : bool =
    not (a < b)


let inc (n : nat) : nat =
    match n with
    | NZero -> Nat One
    | Nat x -> Nat (S x)


let dec (n : nat) : nat =
    match n with
    | NZero -> failwith "Nat.dec of zero"
    | Nat One -> NZero
    | Nat S x -> Nat x


let ( + ) (a : nat) (b : nat) : nat =
    match a, b with
    | NZero, _ -> b
    | _, NZero -> a
    | Nat x, Nat y -> Nat Cnt.(x + y)


let ( * ) (a : nat) (b : nat) : nat =
    match a, b with
    | NZero, _ | _, NZero -> NZero
    | Nat x, Nat y -> Nat Cnt.(x * y)


let ( // ) (a : nat) (b : cnt) : nat =
    let b = Nat b in
    let rec floordiv_helper q acc =
        match acc <= a with
        | true -> floordiv_helper (inc q) (acc + b)
        | false -> q
    in floordiv_helper NZero b


let rec ( - ) (a : nat) (b : nat) : nat =
    match a, b with
    | NZero, NZero -> NZero
    | NZero, _ -> raise (Failure "Bad natural subtraction. a < b.")
    | Nat One, Nat One -> NZero
    | Nat One, Nat _ -> raise (Failure "Bad natural subtraction. a < b.")
    | Nat (S y), Nat (One) -> Nat y
    | _, NZero -> a
    | Nat (S x), Nat (S y) -> Nat x - Nat y


let rec ( % ) (a : nat) (b : cnt) : nat =
    let bb = Nat b in
    match a < bb with
    | true -> a
    | false -> (a - bb) % b


let euc (a : nat) (b : nat) : nat * nat =
    let rec euc_helper q r =
        match r < b with
        | true -> q, r
        | false -> euc_helper (inc q) (r - b) in
    match b with
    | NZero -> NZero, a
    | Nat _ -> euc_helper NZero a


let rec to_str x : string =
    let ten = Nat (S (S (S (S (S (S (S (S (S One))))))))) in
    match x with
    | NZero -> "0"
    | Nat One -> "1"
    | Nat S One -> "2"
    | Nat S S One -> "3"
    | Nat S S S One -> "4"
    | Nat S S S S One -> "5"
    | Nat S S S S S One -> "6"
    | Nat S S S S S S One -> "7"
    | Nat S S S S S S S One -> "8"
    | Nat S S S S S S S S One -> "9"
    | x -> let q, r = euc ten x in (to_str q) ^ (to_str r)

let as_cnt n =
    match n with
    | Nat c -> c
    | NZero -> failwith "Bad Nat.as_cnt"

let two = Nat Cnt.two
let sixteen = Nat Cnt.sixteen
