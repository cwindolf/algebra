let ( @@ ) f x = f x


let check (expr : bool) (name : string) : bool =
    match expr with
    | true ->
        let _ = print_endline @@ "  OK   : " ^ name ^ " passed." in
        true
    | false ->
        let _ = print_endline @@ "~FAIL~ : " ^ name ^ " failed" in
        false


let raises (expr : bool Lazy.t) (name : string) : bool =
    try
        let _ = Lazy.force expr in check false name
    with
    | _ -> check true name
