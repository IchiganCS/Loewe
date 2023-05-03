namespace Loewe.Parsing.Composing.Error
open Loewe.Parsing.Composing.Types
open Loewe.Parsing.Tokenizing

type ErrorCause = CodeParts

/// A type for specifying a parsing error. The list of tokens starts with the tokens responsible for the error.
type ErrorTrace = 
    | End
    | Linear of Token list * ErrorCause * ErrorTrace
    | Multiple of ErrorTrace Set

    

module Error =
    /// Calcualtes the longest token list given a set of error traces.
    let rec longestTokenList errorTraces =
        errorTraces
        |> List.map (
            function
            | End -> []
            | Linear (ls, _, _) -> ls
            | Multiple set -> longestTokenList (set |> Set.toList)
            )
        |> List.maxBy List.length

    /// Formats an error trace as a string.
    let string errorTrace =
        let pad = 2
        let rec format depth =
            function
            | End -> ""
            | Linear (_, cause, et) -> 
                "\n".PadRight(pad * depth) + (string cause) + (format depth et)
            | Multiple set ->
                set
                |> Set.fold (fun x y -> x + (format (depth + 1) y)) "\n"

        format 0 errorTrace

    // /// Returns a trace to the path with least tokens (e.g. the path that could do most work)
    // let deepestError error =

    //     // takes an error trace and returns a linear (as good as possible) trace with the least tokens
    //     // in its paths. The second element is the length of the token
    //     let rec helper =
    //         function
    //         | End -> End, 0 // should not be reached
    //         | Linear (toks, str, next) -> 
    //             match next with
    //             | End -> Linear (toks, str, End), (toks |> List.length)
    //             | _ ->
    //                 let (err, len) = helper next
    //                 Linear (toks, str, err), len

    //         | Multiple set ->
    //             let minTraces, len = 
    //                 set |>
    //                 Set.toList |>
    //                 List.minBy (fun error -> snd (helper error)) |>
    //                 fun minError -> 
    //                     List.filter (fun error -> snd (helper error) = snd (helper minError)) (Set.toList set), snd (helper minError)                
    
    //             let cleanedTraces =
    //                 minTraces |>
    //                 Set.ofList |>
    //                 Set.map (fun err -> fst (helper err))

    //             let maxTokens = longestTokenList (set |> Set.toList)
                
    //             if Set.count cleanedTraces = 1 then
    //                 Linear (maxTokens, str, cleanedTraces.MaximumElement), len
    //             else
    //                 Multiple cleanedTraces, len

    //     fst (helper error)