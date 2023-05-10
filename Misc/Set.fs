module Loewe.Misc.Set

let choose (fn : 'a -> 'b option) (set : 'a Set) : 'b Set =
    set
    |> Set.map fn
    |> Set.filter (
        function
        | Some _ -> true
        | _ -> false
    )
    |> Set.map (
        function
        | Some x -> x
        | None -> raise (Failure "will not be raised")
    )