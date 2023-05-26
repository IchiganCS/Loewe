module Loewe.Parsing.Composition.FileComposition

open Loewe.Parsing.Composition.CompositionTypes
open Loewe.Definition.CodeConstructs

let private buildUndefinedFunction declaredNamespace (ret, name, parameters, block) =
    FunctionSymbol (
        {
            Name = name
            Return = ret
            Parameters = parameters
            Namespace = declaredNamespace
        },
        block
    )

let private buildUndefinedMethod ownerClass (acc, ret, name, parameters, block) =
    MethodSymbol (
        {
            Name = name
            Return = ret
            Parameters = parameters
            Owner = ownerClass
            AccessModifier = acc
        },
        block
    )

let private buildUndefinedAttribute ownerClass (acc, t, name) =
    AttributeSymbol {
        Name = name
        Type = t
        Owner = ownerClass
        AccessModifier = acc
    }

let private buildSymbolsFromEntry declaredNamespace =
    function
    | FunctionEntry (ret, name, parameters, block) ->
        buildUndefinedFunction declaredNamespace (ret, name, parameters, block)
        |> List.singleton
    | ClassEntry (name, members) ->
        let ownerClass = {
            Name = name
            Namespace = declaredNamespace
        }

        (ClassSymbol ownerClass)
        :: (members
            |> List.map (function
                | MethodMember (acc, ret, name, parameters, block) ->
                    buildUndefinedMethod ownerClass (acc, ret, name, parameters, block)
                | AttributeMember (acc, t, name) -> buildUndefinedAttribute ownerClass (acc, t, name)))


let entireFile tokens =
    match ConstructComposition.fileContent tokens with
    | Ok (_, (declaredNamespace, openedNamespaces, symbols)) ->
        Ok (
            declaredNamespace,
            openedNamespaces,
            symbols |> List.map (buildSymbolsFromEntry declaredNamespace) |> List.concat
        )
    | Error cet -> Error cet
