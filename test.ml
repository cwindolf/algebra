open Util


let test_cnt =
    let _ = print_endline "Testing Cnt....." in
    let open Cnt in
    (* == *)
    (check (One == One) "1 == 1") &&
    (check (S One == S One) "2 == 2") &&
    (check (not (S One == S (S One))) "2 != 3") &&
    (check (not (One == S (S (S One)))) "1 != 4") &&
    (* < *)
    (check (One < S One) "1 < 2") &&
    (check (not (One < One)) "1 !< 2") &&
    (check (S (S One) < S (S (S One))) "3 < 4") &&
    (* > *)
    (check (S One > One) "2 > 1") &&
    (check (not (One > S One)) "1 !> 2") &&
    (check (not (S One > S One)) "2 !> 2") &&
    (check (S (S One) > S One) "3 > 2") &&
    (check (not (One > One)) "1 > 1") &&
    (* + *)
    (check (One + One == S One) "1 + 1 = 2") &&
    (check (S One + S (S One) == S (S (S (S One)))) "2 + 3 = 5") &&
    (check
        (S One + S (S One) + One + (S One) == S (S (S (S (S (S (S One)))))))
        "2 + 3 + 1 + 2 = 8") &&
    (* * *)
    (check (One * One == One) "1 * 1 = 1") &&
    (check (S One * One == S One) "2 * 1 = 2") &&
    (check (S (S One) * S One == S (S (S (S (S One))))) "3 * 2 = 6") &&
    (check
        (S (S One) * S One * S (S (S One))
         == S (S (S (S (S (S (S (S (S (S (S (S (S
              (S (S (S (S (S (S (S (S (S (S One)))))))))))))))))))))))
        "3 * 2 * 4 = 24") &&
    (* ** *)
    (check (One ** One == One) "1 ** 1 = 1") &&
    (check (One ** S One == One) "1 ** 2 = 1") &&
    (check (S One ** S One == S (S (S One))) "2 ** 2 = 4") &&
    (check (S (S One) ** S One == S (S (S (S (S (S (S (S One)))))))) "2 ** 2 = 4") &&
    (check (Cnt.sixteen ** One == Cnt.sixteen) "16 ** 1 = 16") &&
    (check (Cnt.sixteen ** S One == Cnt.sixteen * Cnt.sixteen) "16 ** 2 = 16 * 16") &&
    (check
        (Cnt.sixteen ** S (S One) == Cnt.sixteen * Cnt.sixteen * Cnt.sixteen)
        "16 ** 3 = 16 * 16 * 16")


let test_nat =
    let _ = print_endline "Testing Nat....." in
    let one = Cnt.One in
    let open Nat in
    (* == *)
    (check (NZero == NZero) "0=0") &&
    (check (not (Nat.two == NZero)) "2!=0") &&
    (check (Nat.one == Nat.one) "1=1") &&
    (* > *)
    (check (not (NZero > NZero)) "0!>0") &&
    (check (Nat.one > NZero) "1>0") &&
    (check (not (NZero > Nat.one)) "0!>1") &&
    (check (not (Nat.one > Nat.one)) "1!>1") &&
    (* < *)
    (check (not (NZero < NZero)) "0!<0") &&
    (check (not (Nat.one < NZero)) "1!<0") &&
    (check (NZero < Nat.one) "0<1") &&
    (check (not (Nat.one < Nat.one)) "1!<1") &&
    (* inc *)
    (check (inc NZero == Nat.one) "++0 = 1") &&
    (check (inc (Nat.one) == Nat.two) "++1 = 2") &&
    (* save some time *)
    let six = inc (inc (inc three)) in
    let cthree = Nat.as_cnt three in
    let csix = Nat.as_cnt six in
    (* + *)
    (check (NZero + NZero == NZero) "0+0=0") &&
    (check (NZero + one == one) "0+1=1") &&
    (check (two + one == three) "2+1=3") &&
    (* * *)
    (check (NZero * NZero == NZero) "0+0=0") &&
    (check (NZero * one == NZero) "0+1=1") &&
    (check (two * one == two) "2+1=3") &&
    (check (two * three == six) "2+1=3") &&
    (* // *)
    (check (NZero // Cnt.One == NZero) "0 // 1 = 0") &&
    (check (one // Cnt.One == one) "1 // 1 = 1") &&
    (check (six // Cnt.One == six) "6 // 1 = 6") &&
    (check (six // cthree == two) "6 // 3 = 2") &&
    (check (three // csix == NZero) "3 // 6 = 0") &&
    (* % *)
    (check (NZero % Cnt.One == NZero) "0 % 1 = 0") &&
    (check (one % Cnt.One == NZero) "1 % 1 = 0") &&
    (check (two % Cnt.S Cnt.One == NZero) "2 % 2 = 0") &&
    (check (two % cthree == two) "2 % 3 = 2") &&
    (check (two % csix == two) "2 % 6 = 2") &&
    (check (six % Cnt.S Cnt.One == NZero) "6 % 2 = 0") &&
    (check (three % Cnt.S Cnt.One == one) "6 % 2 = 0") &&
    (* euclidean division *)
    let q, r = euc NZero NZero in
    (check ((q == NZero) && (r == NZero)) "0=0*0+0") &&
    let q, r = euc NZero one in
    (check ((q == NZero) && (r == NZero)) "0=0*1+0") &&
    let q, r = euc one NZero in
    (check ((q == NZero) && (r == one)) "1=0*0+1") &&
    let q, r = euc one one in
    (check ((q == one) && (r == NZero)) "1=1*1+0") &&
    let q, r = euc two one in
    (check ((q == two) && (r == NZero)) "2=2*1+0") &&
    let q, r = euc six two in
    (check ((q == three) && (r == NZero)) "6=3*2+0") &&
    let q, r = euc three two in
    (check ((q == one) && (r == one)) "3=1*2+1") &&
    let q, r = euc two three in
    (check ((q == NZero) && (r == two)) "2=0*3+2") &&
    let q, r = euc two six in
    (check ((q == NZero) && (r == two)) "2=0*6+2") &&
    let q, r = euc Nat.twentytwo Nat.seven in
    (check ((q == Nat.three) && (r == Nat.one)) "22=3*7+1") &&
    (* stringing *)
    (check ((to_str NZero) = "0") "str 0") &&
    (check ((to_str six) = "6") "str 6") &&
    (check ((to_str twentytwo) = "22") "str 22") &&
    (check ((to_str (six * six)) = "36") "str 36")


let test_int =
    let _ = print_endline "Testing Int......" in
    let open Int in
    (* keepers *)
    let z = IZero in
    let po = Pos Cnt.One in
    let pt = Pos (Cnt.S Cnt.One) in
    let no = Neg Cnt.One in
    let nt = Neg (Cnt.S Cnt.One) in
    (* == *)
    (check (z == z) "0 = 0") &&
    (check (not (po == z)) "1 != 0") &&
    (check (not (z == no)) "0 != -1") &&
    (check (po == po) "1 = 1") &&
    (check (no == no) "-1 = -1") &&
    (check (not (no == po)) "-1 != 1") &&
    (check (not (pt == nt)) "-2 != 2") &&
    (check (pt == pt) "2 = 2") &&
    (* < *)
    (check (nt < no) "-2 < -1") &&
    (check (no < z) "-1 < 0") &&
    (check (z < po) "0 < 1") &&
    (check (po < pt) "1 < 2") &&
    (check (not (no < nt)) "-1 !< -2") &&
    (check (not (z < no)) "0 !< -1") &&
    (check (not (po < z)) "1 !< 0") &&
    (check (not (pt < po)) "2 !< 1") &&
    (check (not (nt < nt)) "-2 !< -2") &&
    (check (not (z < z)) "0 !< 0") &&
    (check (not (po < po)) "1 !< 1") &&
    (* > *)
    (check (not (nt > no)) "-2 !> -1") &&
    (check (not (no > z)) "-1 !> 0") &&
    (check (not (z > po)) "0 !> 1") &&
    (check (not (po > pt)) "1 !> 2") &&
    (check (no > nt) "-1 > -2") &&
    (check (z > no) "0 > -1") &&
    (check (po > z) "1 > 0") &&
    (check (pt > po) "2 > 1") &&
    (check (not (nt > nt)) "-2 !> -2") &&
    (check (not (z > z)) "0 !> 0") &&
    (check (not (po > po)) "1 !> 1") &&
    (* inc *)
    (check ((inc nt) == no) "-2++ = -1") &&
    (check ((inc no) == z) "-1++ = 0") &&
    (check ((inc z) == po) "0++ = 1") &&
    (check ((inc po) == pt) "1++ = 2") &&
    (* dec *)
    (check ((dec no) == nt) "-1-- = -2") &&
    (check ((dec z) == no) "0-- = 1") &&
    (check ((dec po) == z) "1-- = 2") &&
    (check ((dec pt) == po) "2-- = 1") &&
    (* neg *)
    (check ((neg pt) == nt) "-(2) = -2") &&
    (check ((neg z) == z) "-(0) = 0") &&
    (check ((neg no) == po) "-(-1) = 1") &&
    (* abs *)
    let nth = dec nt in
    let nf = dec (dec nth) in
    let pf = neg nf in
    (check ((abs pt) == pt) "|2| = 2") &&
    (check ((abs z) == z) "|0| = 0") &&
    (check ((abs no) == po) "|-1| = 1") &&
    (* + *)
    (check (nt + z == nt) "-2 + 0 = -2") &&
    (check (z + z == z) "0 + 0 = 0") &&
    (check (po + po == pt) "1 + 1 = 2") &&
    (check (no + no == nt) "-1 + -1 = -2") &&
    (check (po + no == z) "1 + -1 = 0") &&
    (check (po + nt == no) "1 + -2 = -1") &&
    (* * *)
    (check (z * z == z) "0 * 0 = 0") &&
    (check (po * z == z) "1 * 0 = 0") &&
    (check (po * po == po) "1 * 1 = 1") &&
    (check (no * po == no) "-1 * 1 = -1") &&
    (check (no * no == po) "-1 * -1 = 1") &&
    (check (no * pt == nt) "-1 * 2 = -2") &&
    (check (pt * pt == pt + pt) "2 * 2 = 4") &&
    (* euclidean division *)
    let q, r = euc IZero po in
    (check ((q == IZero) && (r == IZero)) "0 = 0 * 1 0") &&
    let q, r = euc no nt in
    (check ((q == IZero) && (r == no)) "-1 = 0 * -2 + -1") &&
    let q, r = euc pt nf in
    (check ((q == IZero) && (r == pt)) "2 = 0 * -5 + 2") &&
    let q, r = euc (nf + nth) nf in
    (check ((q == po) && (r == nth)) "-8 = 1 * -5 + -3") &&
    (* floor division *)
    (raises (lazy (z // z == z)) "0 // 0 !") &&
    (raises (lazy (po // z == z)) "1 // 0 !") &&
    (check (z // po == z) "0 // 1 == 0") &&
    (check (pt // nt == no) "2 // -2 == -1") &&
    (check (nt // nt == po) "-2 // -2 == 1") &&
    (check (no // nt == z) "-1 // -2 == 0") &&
    (check (nt // po == nt) "-2 // 1 == -2") &&
    (check (Int.twentytwo // Int.seven == Int.three) "22 // 7 == 3") &&
    (check ((nt + nt) // nt == pt) "-4 // -2 == 2") &&
    (* modulo (or is this remainder?) *)
    (raises (lazy (po % z == z)) "0 % 1 !") &&
    (check (z % no == z) "0 % -1 == 0") &&
    (check (pt % nf == pt) "2 % -5 = 2") &&
    (check (nt % nf == nt) "-2 % -5 = -2") &&
    (check (pt % pf == pt) "2 % 5 = 2") &&
    (check (z % pf == z) "0 % 5 = 0") &&
    (check (pf % pf == z) "5 % 5 = 0") &&
    (check (nf % pf == z) "-5 % 5 = 0") &&
    (* gcd *)
    (check (gcd pf IZero == pf) "(5, 0) = 5") &&
    (check (gcd IZero pf == pf) "(0, 5) = 5") &&
    (check (gcd pt pt == pt) "(2, 2) = 2") &&
    (check (gcd nt po == po) "(-2, 1) = 1") &&
    (check (gcd pt no == po) "(2, -1) = 1") &&
    (check (gcd nf nf == pf) "(-5, -5) = 5")


let test_rat =
    let _ = print_endline "Testing Rat......" in
    let open Rat in
    let one = Int.Pos Cnt.One, Cnt.One in
    let half = Int.Pos Cnt.One, Cnt.S Cnt.One in
    let tutu = Int.Pos (Cnt.S Cnt.One), Cnt.S Cnt.One in
    let thsi = Int.Pos (Cnt.S (Cnt.S Cnt.One)),
               Cnt.S (Cnt.S (Cnt. S (Cnt.S (Cnt.S Cnt.One)))) in
    let two = Int.Pos (Cnt.S Cnt.One), Cnt.One in
    let sith = Int.Pos (Cnt.S (Cnt.S (Cnt. S (Cnt.S (Cnt.S Cnt.One))))),
               Cnt.S (Cnt.S Cnt.One) in
    let ntwo = Int.Neg (Cnt.S Cnt.One), Cnt.One in
    let nsith = Int.Neg (Cnt.S (Cnt.S (Cnt. S (Cnt.S (Cnt.S Cnt.One))))),
               Cnt.S (Cnt.S Cnt.One) in
    (* least terms *)
    (check (least_terms one == one) "1/1=1/1") &&
    (check (least_terms half == half) "1/2=1/2") &&
    (check (least_terms tutu == one) "2/2=1/1") &&
    (check (least_terms thsi == half) "3/6=1/2") &&
    (check (least_terms two == two) "2/1=2/1") &&
    (check (least_terms ntwo == ntwo) "-2/1=-2/1") &&
    (check (least_terms sith == two) "6/3=2/1") &&
    (check (least_terms nsith == ntwo) "-6/3=-2/1") &&
    (* floor *)
    let x = (floor (Int.twentytwo, Cnt.seven)) in
    (check Int.(x == Int.three) "_22/7_=3") &&
    (* dec str *)
    (check (to_dec_str one = "1.") "str 1/1") &&
    (check ((to_dec_str (Int.twentytwo, Cnt.seven)) = "3.142857") "str 22/7") &&
    (check (to_dec_str ~prec:(Nat.ten) (Int.twentytwo, Cnt.seven) = "3.1428571428") "str 22/7")


let _ =
    match test_cnt && test_nat && test_int && test_rat with
    | true -> print_endline "All good"
    | false -> print_endline "Some problems"
