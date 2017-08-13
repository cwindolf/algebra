open Bool
open Util

(* Test Cnt *)
let test_cnt =
    let _ = print_endline "Testing Cnt....." in
    let open Cnt in
    (* == *)
    (check (One == One) "1 == 1") &&
    (check (S One == S One) "2 == 2") &&
    (check (~~ (S One == S (S One))) "2 != 3") &&
    (* < *)
    (check (One < S One) "1 < 2") &&
    (check (~~ (One < One)) "1 !< 2") &&
    (check (S (S One) < S (S (S One))) "3 < 4")
    (* > *)

