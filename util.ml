open Bool


let ( @@ ) f x = f x


let check (expr : bool) (name : string) : bool =
    match expr with
    | True ->
        let _ = print_endline @@ "  OK   : " ^ name ^ " passed." in
        True
    | False ->
        let _ = print_endline @@ "~FAIL~ : " ^ name ^ " failed" in
        False


let raises (expr : bool Lazy.t) (name : string) : bool =
    try
        let _ = Lazy.force expr in check False name
    with
    | _ -> check True name
