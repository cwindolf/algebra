open Bool


let ( @@ ) f x = f x


let check expr name =
    match expr with
    | True -> 
        let _ = print_endline @@ "  OK   : " ^ name ^ " passed." in
        True
    | False ->
        let _ = print_endline @@ "~FAIL~ : " ^ name ^ " failed" in
        False
