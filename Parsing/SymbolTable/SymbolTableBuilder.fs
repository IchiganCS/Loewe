/// This module provides methods for building a complete symbol table.
/// It resolves given symbols and returns them, overall. Several helper functions are publicly available, but the function
/// `resolveFromFile` is probably the most useful.
module Loewe.Parsing.SymbolTable.SymbolTableBuilder

open Loewe.Parsing.Composition.CompositionTypes
open Loewe.Parsing.Composition
open Loewe.Misc
open Loewe.Parsing.Types
open SymbolTypes

/// Any kind of error which can occur during resolving
type ResolveError =
    /// A symbol could not be resolved. The first set of namespaces identify the searched namespaces, the second element
    /// the name of the symbol.
    | UnknownSymbol of Namespace Set * string

    /// In the specified namespaces with the given name, the symbol was found multiple times.
    | DuplicateSymbol of Namespace Set * string

/// Shorthand for the built-in result type with a resolve error.
type ResolveResult<'a> = Result<'a, ResolveError Set>

let private mergeResolveResults (results: ResolveResult<'a> list) : ResolveResult<'a list> =
    results
    |> List.fold
        (fun state item ->
            match state, item with
            | Ok handled, Ok newResult -> Ok (handled @ [ newResult ])
            | Ok _, Error e
            | Error e, Ok _ -> Error e
            | Error e1, Error e2 -> Error (Set.union e1 e2))
        (Ok [])

/// A list of primitive types.
/// TODO: generalize this.
let primitiveTypes =
    Set [ "int"; "uint"; "long"; "ulong"; "float"; "double"; "string"; "bool" ]


/// Given a set of forward declarations, it can, using a set of openend namespaces and a string
/// for a type name, find matches in the given set. A `Result` type is returned
/// if there is one match with `Ok`, if there is not one match (multiple or none), `Error` is returned
/// with a `ResolveError`.
let resolveType (classDeclarations: ClassStump Set) searchNamespaces typeName : ResolveResult<Type> =
    let classMatches =
        classDeclarations
        |> Set.filter (fun decl -> Set.contains decl.Namespace searchNamespaces)
        |> Set.filter (fun decl -> decl.Name = typeName)
        |> Set.map ClassType

    let primitiveMatches =
        primitiveTypes |> Set.filter ((=) typeName) |> Set.map PrimitiveType

    let matches = Set.union primitiveMatches classMatches

    if matches |> Set.count = 1 then
        Ok (matches |> Set.maxElement)
    else if matches |> Set.count = 0 then
        Error (UnknownSymbol (searchNamespaces, typeName) |> Set.singleton)
    else
        Error (DuplicateSymbol (searchNamespaces, typeName) |> Set.singleton)

/// Resolves a qualified type. The qualified identifier has a `Option` for `Namespace` attached to it, if it is none, the
/// `alternativeNamespaces` is used as the search set. It wraps a call to `resolveType`.
let resolveQualifiedType declarations alternativeNamespaces qi : ResolveResult<Type> =
    let name, namespaceOption = qi

    let namesp =
        namespaceOption
        |> Option.bind (fun x -> Some (Set.singleton x))
        |> Option.defaultValue alternativeNamespaces

    resolveType declarations namesp name

let resolveQualifiedTypeOrVoid declarations alternativeNamespaces qi : ResolveResult<Type option> =
    let name, namespaceOptions = qi

    if namespaceOptions = None && name = "void" then
        Ok None
    else
        resolveQualifiedType declarations alternativeNamespaces qi |> Result.map Some

let resolveMethod declarations openedNamespaces ownerClass (method: CompositionTypes.Method) =
    match resolveQualifiedTypeOrVoid declarations openedNamespaces method.Return with
    | Error e -> Error e
    | Ok returnType ->

        let parameterResolvingResult =
            method.Parameters
            |> List.map (fun (t, n) ->
                resolveQualifiedType declarations openedNamespaces t
                |> Result.map (fun x -> x, n))
            |> mergeResolveResults

        match parameterResolvingResult with
        | Error e -> Error e
        | Ok parameters ->

            MethodSymbol {
                Name = method.Name
                Code = method.Code
                AccessModifier = method.AccessModifier
                Return = returnType
                Parameters = parameters
                Owner = ownerClass
            }
            |> Ok

let resolveAttribute declarations openedNamespaces ownerClass (attribute: CompositionTypes.Attribute) =
    match resolveQualifiedType declarations openedNamespaces attribute.Type with
    | Error e -> Error e
    | Ok attriType ->

        AttributeSymbol {
            Name = attribute.Name
            AccessModifier = attribute.AccessModifier
            Type = attriType
            Owner = ownerClass
        }
        |> Ok

let resolveMember declarations openedNamespaces ownerClass memb =
    match memb with
    | Method m -> resolveMethod declarations openedNamespaces ownerClass m
    | Attribute a -> resolveAttribute declarations openedNamespaces ownerClass a


let resolveClass declarations openedNamespaces definedNamespace (cl: Class) =
    let stump = {
        Name = cl.Name
        Namespace = definedNamespace
    }

    let resolvedMembers, unresolvedMembers =
        cl.Members
        |> Set.map (resolveMember declarations openedNamespaces stump)
        |> Set.partition Result.isOk


    if unresolvedMembers |> Set.count > 0 then
        unresolvedMembers |> Set.map (Result.map Set.singleton) |> Set.maxElement
    else
        resolvedMembers 
        |> Set.map Result.get 
        |> Set.add (ClassSymbol stump)
        |> Ok


let resolveFunction declarations openedNamespaces declaredNamespace (func: CompositionTypes.Function) =
    match resolveQualifiedTypeOrVoid declarations openedNamespaces func.Return with
    | Error e -> Error e
    | Ok returnType ->

        let parameterResolvingResult =
            func.Parameters
            |> List.map (fun (t, n) ->
                resolveQualifiedType declarations openedNamespaces t
                |> Result.map (fun x -> x, n))
            |> mergeResolveResults

        match parameterResolvingResult with
        | Error e -> Error e
        | Ok parameters ->

            FunctionSymbol {
                Name = func.Name
                Return = returnType
                Parameters = parameters
                Code = func.Code
                Namespace = declaredNamespace
            }
            |> Ok


let fromFile content =
    let fileNamespace, openedNamespaces, members = content

    let forwardDeclarations: ClassStump Set =
        members
        |> Set.choose (function
            | ClassEntry x ->
                Some {
                    Name = x.Name
                    Namespace = fileNamespace
                }
            | _ -> None)

    members
    |> Set.toList
    |> List.map (function
        | ClassEntry c ->
            resolveClass forwardDeclarations openedNamespaces fileNamespace c
            |> Result.map Set.toList
        | FunctionEntry f ->
            resolveFunction forwardDeclarations openedNamespaces fileNamespace f
            |> Result.map List.singleton)
    |> mergeResolveResults
    |> Result.map List.concat
    |> Result.map Set.ofList
