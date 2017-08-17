open Cnt
open Nat
open Rat


(* BBP formula - going to be ridiculous with these unary representations.
 * https://en.wikipedia.org/wiki/Bailey%E2%80%93Borwein%E2%80%93Plouffe_formula *)


let bbp (prec : cnt) : rat =
    let p (k : cnt) =
        let eightk = Cnt.(eight * k) in
        Rat.( (Int.one, Cnt.sixteen ** k)
            * ( (Int.four, Cnt.(eightk + Cnt.One))
              - (Int.two, Cnt.(eightk + Cnt.four))
              - (Int.one, Cnt.(eightk + Cnt.five))
              - (Int.one, Cnt.(eightk + Cnt.six)))) in
    let rec bbp_helper (k : cnt) (acc : rat) : rat =
        match Cnt.(k >= prec) with
        | true -> acc
        | false -> bbp_helper (Cnt.S k) (least_terms ((p k) + acc)) in
    (Rat.of_int Int.three) + (bbp_helper One (Rat.of_int Int.IZero))


let wallis (iters : nat) : rat =
    let double = Nat.(two * iters) in
    let rec tail k acc =
        match k < double with
        | false -> acc
        | true -> let ik = Int.of_nat k in
                  tail (Nat.inc (Nat.inc k)) (acc * Int.((ik * ik), (as_cnt ((ik - one) * (ik + one))))) in
    tail Nat.two Rat.two


let _ =
    let prec = Nat.ten in
    let _ = print_endline "Wallis" in
    let _ = Rat.to_dec_str ~prec:prec (wallis Nat.two) |> print_endline in
    let _ = print_endline "BBP" in
    Rat.to_dec_str ~prec:prec (bbp Cnt.two) |> print_endline
