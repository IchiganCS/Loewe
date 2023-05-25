module Loewe.Parsing.SymbolTable.PartialResolve

open Loewe.Misc
open Loewe.Definition.Primitives
open Loewe.Definition.CodeConstructs
open ResolveError


/// Given a set of forward declarations, it can, using a set of openend namespaces and a string
/// for a type name, find matches in the given set. A `Result` type is returned
/// if there is one match with `Ok`, if there is not one match (multiple or none), `Error` is returned
/// with a `ResolveError`.
let resolveType (classDeclarations: Class list) searchNamespaces typeName : ResolveResult<Type> =
    match sourceToPrimitiveMap |> Map.tryFind typeName with
    | Some x -> Ok (PrimitiveType x)
    | None ->

        let matches =
            classDeclarations
            |> List.filter (fun decl -> searchNamespaces |> List.contains decl.Namespace && decl.Name = typeName)
            |> List.map ClassType

        match matches with
        | [one] -> Ok one
        | [] -> Error [UnknownType (searchNamespaces, typeName)]
        | _ -> Error [DuplicateSymbol (searchNamespaces, typeName)]

/// Resolves a qualified type. The qualified identifier has a `Option` for `Namespace` attached to it, if it is none, the
/// `alternativeNamespaces` is used as the search set. It wraps a call to `resolveType`.
let resolveUnknownIdentifier declarations alternativeNamespaces qi : ResolveResult<Type> =
    let name, namespaceOption = qi

    let namesp =
        match namespaceOption with
        | Some x -> [x]
        | None -> alternativeNamespaces

    resolveType declarations namesp name

let resolveMethod declarations openedNamespaces ((method, codeblock): UnresolvedMethod) =
    match resolveUnknownIdentifier declarations openedNamespaces method.Return with
    | Error e -> Error e
    | Ok returnType ->

        let parameterResolvingResult =
            method.Parameters
            |> List.map (fun (t, n) ->
                resolveUnknownIdentifier declarations openedNamespaces t
                |> Result.map (fun x -> x, n))
            |> ResolveError.mergeResults

        match parameterResolvingResult with
        | Error e -> Error e
        | Ok parameters ->
            let methodSym = {
                Name = method.Name
                AccessModifier = method.AccessModifier
                Return = returnType
                Parameters = parameters
                Owner = method.Owner
            }

            MethodSymbol (methodSym, codeblock) |> Ok

let resolveAttribute declarations openedNamespaces (attribute: UnresolvedAttribute) =
    match resolveUnknownIdentifier declarations openedNamespaces attribute.Type with
    | Error e -> Error e
    | Ok attriType ->
        AttributeSymbol {
            Name = attribute.Name
            AccessModifier = attribute.AccessModifier
            Type = attriType
            Owner = attribute.Owner
        }
        |> Ok

let resolveFunction declarations openedNamespaces declaredNamespace ((func, codeblock): UnresolvedFunction) =
    match resolveUnknownIdentifier declarations openedNamespaces func.Return with
    | Error e -> Error e
    | Ok returnType ->

        let parameterResolvingResult =
            func.Parameters
            |> List.map (fun (t, n) ->
                resolveUnknownIdentifier declarations openedNamespaces t
                |> Result.map (fun x -> x, n))
            |> ResolveError.mergeResults

        match parameterResolvingResult with
        | Error e -> Error e
        | Ok parameters ->
            let funcSym = {
                Name = func.Name
                Return = returnType
                Parameters = parameters
                Namespace = declaredNamespace
            }

            FunctionSymbol (funcSym, codeblock) |> Ok


let fromFile content  : ResolveResult<PartialResolvedSymbol list * Namespace list> =
    let fileNamespace, openedNamespaces, members = content

    let forwardDeclarations =
        members
        |> List.choose (function
            | ClassSymbol c -> Some c
            | _ -> None)


    members
    |> List.map (function
        | ClassSymbol c -> Ok (ClassSymbol c)
        | FunctionSymbol (f, c) -> resolveFunction forwardDeclarations openedNamespaces fileNamespace (f, c)
        | AttributeSymbol a -> resolveAttribute forwardDeclarations openedNamespaces a
        | MethodSymbol (m, c) -> resolveMethod forwardDeclarations openedNamespaces (m, c))
    |> ResolveError.mergeResults
    |> Result.map (fun members -> members, openedNamespaces)
