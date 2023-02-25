module Loewe.Parsing.Tree.Unresolved.Main
open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.Tree.Unresolved.Helpers
open Types



/// <summary>
/// Extracts all information from a single file from the analyzed tokens. It returns unresolved references.
/// </summary>
let buildUnresolvedFromFile (tokens : Token list) : (UnresolvedTopLevelEntry Set) option = 
    match expect "File header" acceptFileHeader (Some tokens) with
    | None -> None
    | Some (restTokens, (namesp, opened)) ->
        let allopened = Set.add namesp opened

        match 
            Some restTokens |>
            expectMany "File body content" (acceptFileBodyMember namesp allopened) with
        | None
        | Some (_::_, _) -> None
        | Some ([], entries) -> Some (Set.ofList entries)