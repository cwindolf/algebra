type cnt = One | S of cnt


let ( + ) (n : cnt) (m : cnt) : cnt =
    (* O(n) *)
    let rec plus_helper rem acc =
        match rem with
        | One -> S acc
        | S r -> plus_helper r (S acc) in
    plus_helper n m


let ( * ) (n : cnt) (m : cnt) : cnt =
    (* O(nm) *)
    let rec mul_helper rem acc =
        match rem with
        | One -> acc
        | S x -> mul_helper x (m + acc) in
    mul_helper n m


let ( ** ) (n : cnt) (k : cnt) : cnt =
    (* O(kn^k) *)
    let rec pow_helper rem acc =
        match rem with
        | One -> acc
        | S x -> pow_helper x (n * acc) in
    pow_helper k n


let rec ( > ) (a : cnt) (b : cnt) : bool =
    match a, b with
    | One, _ -> false
    | _, One -> true
    | S x, S y -> x > y


let rec ( < ) (a : cnt) (b : cnt) : bool =
    match a, b with
    | One, S _ -> true
    | _, One -> false
    | S x, S y -> x < y


let rec ( == ) (a : cnt) (b : cnt) : bool =
    match a, b with
    | One, One -> true
    | S x, S y -> x == y
    | _, _ -> false


let ( >= ) a b = not (a < b)


let rec to_tally_str ?(acc="") c =
    match c with
    | One -> acc ^ "O"
    | S x -> to_tally_str ~acc:(acc^"|") x


let one = One
let two = S One
let three = S (S One)
let four = S (S (S One))
let five = S (S (S (S One)))
let six = S (S (S (S (S One))))
let seven = S (S (S (S (S (S One)))))
let eight = S (S (S (S (S (S (S One))))))
let nine = S (S (S (S (S (S (S (S One)))))))
let ten = S (S (S (S (S (S (S (S (S One))))))))
let sixteen = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S One))))))))))))))
let twentytwo = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S One))))))))))))))))))))
