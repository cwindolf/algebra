open Cnt
open Int
open Util


type rat = int * cnt


let least_terms (p, q : rat) : rat =
    (* TODO: There are probably some edge cases to be fixed here. *)
    let g = gcd p (Pos q) in
    p // g, Int.as_cnt ((Pos q) // g)


let ( > ) (a, b : rat) (c, d : rat) : bool =
    (a * (Pos d)) > (c * (Pos b))


let ( <= ) (p : rat) (q : rat) : bool = not (p > q)


let ( == ) (a, b : rat) (c, d : rat) : bool =
    (a * Pos d) == (c * Pos b)


let ( + ) (a, b : rat) (c, d : rat) : rat =
    (a * Pos d) + (c * Pos b), Cnt.(b * d)


let ( - ) (a, b : rat) (c, d : rat) : rat =
    (a * Pos d) - (c * Pos b), Cnt.(b * d)


let ( / ) (a, b : rat) (c, d : rat) : rat =
    least_terms (a * Pos d, Int.as_cnt (Pos b * c))


let ( * ) (a, b : rat) (c, d : rat) : rat =
    least_terms (a * c, Cnt.(b * d))


let of_int (i : int) : rat =
    (* The ususal embedding. *)
    i, One


let abs (p, q : rat) =
    match p with
    | Neg _ -> Int.neg p, q
    | _ -> p, q


let floor (a, b : rat) : int =
    Int.((Pos b) * (a // (Pos b)))


let frac (p : rat) : rat = 
    (* This might not always be what you want for negative numbers,
     * but it helps with printing.
     * e.g. frac -3.14 = .14 *)
    (abs p) - (of_int @@ floor (abs p))


let to_frac_str (r : rat) : string =
    let p, q = least_terms r in
    (Int.to_str p) ^ "/" ^ (Int.to_str (Pos q))


let to_dec_str ?(prec = (S (S One))) (r : rat) : string =
    (* Print whole integer part and `prec` places of fractional part. *)
    (Int.to_str (floor r))
    ^ "."
    ^ (Int.to_str (floor @@ (frac r) * (of_int (Pos prec))))


let print r = r |> to_dec_str |> print_endline


let one = Int.one, Cnt.One
let two = Int.two, Cnt.One
