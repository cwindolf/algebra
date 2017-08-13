open Bool
open Cnt


type nat = NZero | Nat of cnt


let ( == ) (a : nat) (b : nat) : bool =
    match a, b with
    | NZero, NZero -> True
    | Nat x, Nat y -> Cnt.(x == y)
    | _, _ -> False


let ( > ) (a : nat) (b : nat) : bool =
    match a, b with
    | NZero, _ -> False
    | _, NZero -> True
    | Nat x, Nat y -> Cnt.(x > y)


let ( < ) (a : nat) (b : nat) : bool =
    match a, b with
    | NZero, Nat _ -> True
    | _, NZero -> False
    | Nat x, Nat y -> Cnt.(x < y)


let ( <= ) (a : nat) (b : nat) : bool =
    ~~ (a > b)


let ( >= ) (a : nat) (b : nat) : bool =
    ~~ (a < b)


let inc (n : nat) : nat =
    match n with
    | NZero -> Nat One
    | Nat x -> Nat (S x)


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
        | True -> floordiv_helper (inc q) (acc + b)
        | False -> q
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
    | True -> a
    | False -> (a - bb) % b


let euc (a : nat) (b : nat) : nat * nat =
    match b with
    | NZero -> NZero, a
    | Nat _ ->
        let rec euc_helper q r =
            match r < b with
            | True -> q, r
            | False -> euc_helper (inc q) (r - b)
        in euc_helper NZero a


let rec to_str x : string =
    let ten = S (S (S (S (S (S (S (S (S One)))))))) in
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
    | x -> let q, r = euc x ten in (to_str q) ^ (to_str r)
