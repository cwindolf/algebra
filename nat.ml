open Bool

type nat = Zero | Succ of nat

let rec (+) (a : nat) (b : nat) : nat =
    match a with
    | Zero -> b
    | Succ x -> x + (Succ b)

let ( * ) (a : nat) (b : nat) : nat =
    let rec helper a b c =
        match a with
        | Zero -> b
        | Succ x -> helper x b (b + c)
    in helper a b Zero

let rec (>>) (a : nat) (b : nat) : bool =
    match a, b with
    | Zero, _ -> False
    | _, Zero -> True
    | Succ x, Succ y -> x >> y

let rec (==) (a : nat) (b : nat) : bool =
    match a, b with
    | Zero, Zero -> True
    | Succ x, Succ y -> x == y
    | _, _ -> False

let (<<) (a : nat) (b : nat) : bool =
    ~~ (a >> b)
