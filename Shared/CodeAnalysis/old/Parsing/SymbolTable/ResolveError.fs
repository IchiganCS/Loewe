module Loewe.Parsing.SymbolTable.ResolveError
open Loewe.Definition.CodeConstructs
open Loewe.Definition.Primitives


/// Any kind of error which can occur during resolving
type ResolveError =
    | AttributeOfPrimitive of Primitive * string
    | MethodOfPrimitive of Primitive * string
    | UndefinedBinaryPrimitiveOperation of Primitive * BinaryOperation * Primitive
    | UndefinedBinaryNonPrimitiveOperation of Type * BinaryOperation * Type
    | UndefinedUnaryPrimitiveOperation of Primitive * UnaryOperation
    | UndefinedUnaryNonPrimitiveOperation of Type * UnaryOperation
    | DuplicateFunction of string //todo
    | UnknownFunction of string //todo
    | DuplicateAttribute of Class * string
    | UnknownAttribute of Class * string
    | DuplicateVariable of string
    | UnknownVariable of string
    /// A symbol could not be resolved. The first set of namespaces identify the searched namespaces, the second element
    /// the name of the symbol.
    | UnknownType of Namespace list * string

    /// In the specified namespaces with the given name, the symbol was found multiple times.
    | DuplicateSymbol of Namespace list * string

/// Shorthand for the built-in result type with a resolve error.
type ResolveResult<'a> = Result<'a, ResolveError list>

module ResolveError =
    let mergeResults (results: ResolveResult<'a> list) : ResolveResult<'a list> =
        results
        |> List.fold
            (fun state item ->
                match state, item with
                | Ok handled, Ok newResult -> Ok (handled @ [ newResult ])
                | Ok _, Error e
                | Error e, Ok _ -> Error e
                | Error e1, Error e2 -> Error (List.append e1 e2))
            (Ok [])
