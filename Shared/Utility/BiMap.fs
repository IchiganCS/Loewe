module Loewe.Shared.Utility.BiMap

type BiMap<'Val1, 'Val2> = ('Val1 * 'Val2) list

module BiMap =
    let rec tryFindForward searchValue bimap =
        match bimap with
        | (possibleMatch, resultVal)::tail -> 
            if searchValue = possibleMatch then
                Some resultVal
            else
                tryFindForward searchValue bimap
        | [] -> None

    let rec tryFindBackwards searchValue bimap =
        match bimap with
        | (resultVal, possibleMatch)::tail -> 
            if searchValue = possibleMatch then
                Some resultVal
            else
                tryFindBackwards searchValue bimap
        | [] -> None

    /// Verifies that all values are unique and can be mapped. Returns true if the condition is matched
    let verify bimap =
        let itemCount = bimap |> List.length

        bimap
        |> List.distinctBy fst
        |> List.length
            = itemCount

        &&

        bimap
        |> List.distinctBy snd
        |> List.length
            = itemCount

