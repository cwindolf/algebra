type bool = True | False

let (~~) (a : bool) : bool =
    match a with
    |  True -> False
    | False -> True

let (&&) (a : bool) (b : bool) : bool =
    match a, b with
    | True, True -> True
    | _, _ -> False

let (||) (a : bool) (b : bool) : bool =
    match a, b with
    | False, False -> False
    | _, _ -> True

let (==) (a : bool) (b : bool) : bool =
    match a, b with
    | True, False | False, True -> True
    | _, _ -> False
