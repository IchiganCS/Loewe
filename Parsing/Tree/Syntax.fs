module Loewe.Parsing.Tree.Syntax

open Loewe.Parsing.Tree.UnresolvedTypes
open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.CommonTypes


let expectLogger writer requiredStr (accepter : Token list -> Token list option) (tokens : Token list option) =
    match tokens with
    | Some l -> 
        match accepter l with
        | Some rest -> Some rest
        | None ->
            writer (sprintf "Exepected \"%s\", but received %s." requiredStr (string l.Head))
            None
    | _ -> 
        None

let expect = expectLogger System.Console.WriteLine
let rec expectMany str accepter tokens : Token list option = 
    match expect str accepter tokens with
    | Some toks -> 
        // weird recursion to always return Some after first expect
        // None would be wrong
        match expectMany str accepter (Some toks) with
        | Some endToks -> Some endToks
        | None -> Some toks
    | None -> None

let expectSingle single tokens = 
    expect 
        (string single)
        (function 
        | head::tail -> if head = single then Some tail else None
        | _ -> None)
        tokens




let acceptDotSeparatedIdentifier handler tokens =
    let rec tryParseDotSeparatedIdentifier tokens : (string list * Token list) option =
        // first try to read an identifier
        match tokens with
        | (Token.Identifier name)::tail -> 
            // found identifier - look recursively for more
            // cut the dot off - if none is found, return only current name
            match tail with
            | (Token.Separator Separator.Dot)::rest ->
                match tryParseDotSeparatedIdentifier rest with
                | None -> Some ([name], tail)
                | Some (strList, tokList) -> Some (name::strList, tokList)
            | _ -> 
                Some ([name], tail)
        | _ -> None


    match tryParseDotSeparatedIdentifier tokens with
    | Some (strList, toks) ->
        handler strList
        Some toks
    | None -> None

let acceptNamespace handler tokens =

    let buildNamespace strList = 
        let rec buildNamespaceRev strList = 
            match strList with
            | head::tail -> Namespace.Child (buildNamespaceRev tail, head)
            | _ -> Namespace.Global
        buildNamespaceRev (List.rev strList)

    tokens |>
    acceptDotSeparatedIdentifier (fun strList -> handler (buildNamespace strList))

let acceptOpenDirective handler tokens = 
    Some tokens |>
    expectSingle (Token.Keyword Keyword.Open) |>
    expect "Namespace identifier" (acceptNamespace handler) |>
    expectSingle (Token.Separator Separator.Semicolon)

let acceptNamespaceDirective handler tokens =
    tokens |>
    expectSingle (Token.Keyword Keyword.Namespace) |>
    expect "Namespace identifier" (acceptNamespace handler) |>
    expectSingle (Token.Separator Separator.Semicolon)

let acceptFileHeader handleNamespace handleOpen tokens = 
    Some tokens |>
    expect "Namespace declaration" (acceptNamespace handleNamespace) |>
    expectMany "Namespace openings" (acceptOpenDirective handleOpen)


/// <summary>
/// Extracts all information from a single file from the analyzed tokens. It returns unresolved references.
/// </summary>
let buildUnresolvedFromFile (tokens : Token list) : (UnresolvedFunction Set * UnresolvedType Set) option = 
    let (restTokens, namesp) = expect "Namespace" acceptNamespaceDirective
    let mutable opened = Set.empty

    let restTokens = 
        Some tokens |>
        expect "File header" (acceptFileHeader (fun n -> namesp <- n) (fun o -> opened <- Set.add o opened)) |>
        expect "File content" (acceptFileContent )

    match restTokens with
    | None
    | Some (_::_) -> 
        None
    | Some [] ->
        Some (Set.empty, Set.empty)
        