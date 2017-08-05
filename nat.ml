open Bool


type nat = NZero | Succ of nat


let rec ( + ) (a : nat) (b : nat) : nat =
    match a with
    | NZero -> b
    | Succ x -> x + (Succ b)


let ( * ) (a : nat) (b : nat) : nat =
    let rec helper a b c =
        match a with
        | NZero -> b
        | Succ x -> helper x b (b + c)
    in helper a b NZero


let rec ( > ) (a : nat) (b : nat) : bool =
    match a, b with
    | NZero, _ -> False
    | _, NZero -> True
    | Succ x, Succ y -> x > y


let rec ( == ) (a : nat) (b : nat) : bool =
    match a, b with
    | NZero, NZero -> True
    | Succ x, Succ y -> x == y
    | _, _ -> False


let ( <= ) (a : nat) (b : nat) : bool =
    ~~ (a > b)
