open Bool


type cnt = One | S of cnt


let rec ( + ) (n : cnt) (m : cnt) : cnt =
    match n with
    | One -> S m
    | S p -> p + (S m)


let ( * ) (n : cnt) (m : cnt) : cnt =
    let rec mul_helper rem acc =
        match rem with
        | One -> acc
        | S x -> mul_helper x (m + acc) in
    mul_helper n m


let rec ( > ) (a : cnt) (b : cnt) : bool =
    match a, b with
    | One, _ -> False
    | _, One -> True
    | S x, S y -> x > y


let rec ( < ) (a : cnt) (b : cnt) : bool =
    match a, b with
    | One, S _ -> True
    | _, One -> False
    | S x, S y -> x < y


let rec ( == ) (a : cnt) (b : cnt) : bool =
    match a, b with
    | One, One -> True
    | S x, S y -> x == y
    | _, _ -> False
