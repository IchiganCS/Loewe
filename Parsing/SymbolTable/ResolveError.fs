module Loewe.Parsing.SymbolTable.ResolveError
open Loewe.Definition.CodeConstructs


/// Any kind of error which can occur during resolving
type ResolveError =
    /// A symbol could not be resolved. The first set of namespaces identify the searched namespaces, the second element
    /// the name of the symbol.
    | UnknownSymbol of Namespace Set * string

    /// In the specified namespaces with the given name, the symbol was found multiple times.
    | DuplicateSymbol of Namespace Set * string

/// Shorthand for the built-in result type with a resolve error.
type ResolveResult<'a> = Result<'a, ResolveError Set>

module ResolveError =
    let mergeResults (results: ResolveResult<'a> list) : ResolveResult<'a list> =
        results
        |> List.fold
            (fun state item ->
                match state, item with
                | Ok handled, Ok newResult -> Ok (handled @ [ newResult ])
                | Ok _, Error e
                | Error e, Ok _ -> Error e
                | Error e1, Error e2 -> Error (Set.union e1 e2))
            (Ok [])
