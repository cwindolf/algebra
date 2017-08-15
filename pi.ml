open Bool
open Cnt
open Nat
open Rat


(* BBP formula - going to be ridiculous with these unary representations.
 * https://en.wikipedia.org/wiki/Bailey%E2%80%93Borwein%E2%80%93Plouffe_formula *)


let bbp (prec : cnt) : rat =
    let p (k : cnt) =
        let _ = print_endline ("p " ^ Nat.(Nat.to_str (Nat k))) in
        Rat.( (Int.one, Cnt.sixteen ** k)
            * ( (Int.four, Cnt.(eight * k + Cnt.One))
              - (Int.two, Cnt.(eight * k + Cnt.four))
              - (Int.one, Cnt.(eight * k + Cnt.five))
              - (Int.one, Cnt.(eight * k + Cnt.six)))) in
    let rec bbp_helper (k : cnt) (acc : rat) : rat =
        match Cnt.(k >= prec) with
        | True -> acc
        | False -> bbp_helper (Cnt.S k) (least_terms ((p k) + acc)) in
    (Rat.of_int Int.three) + (bbp_helper One (Rat.of_int Int.IZero))


let wallis (iters : nat) : rat =
    let double = Nat.(two * iters) in
    let rec tail k acc =
        match k < double with
        | False -> acc
        | True -> let ik = Int.of_nat k in
                  tail (Nat.inc (Nat.inc k)) Int.((ik * ik), (as_cnt ((ik - one) * (ik + one)))) in
    tail Nat.two Rat.two

let _ =
    let prec = Cnt.(S One) in
    let _ = Rat.to_dec_str ~prec:prec (wallis (Nat prec)) |> print_endline in
    Rat.to_dec_str ~prec:prec (bbp prec) |> print_endline
