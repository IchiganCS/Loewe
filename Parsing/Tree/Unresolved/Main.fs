module Loewe.Parsing.Tree.Unresolved.Main
open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.Tree.Unresolved.Helpers
open Types



/// <summary>
/// Extracts all information from a single file from the analyzed tokens. It returns unresolved references.
/// </summary>
let buildUnresolvedFromFile (tokens : Token list) : (UnresolvedTopLevelEntry Set) option = 
    match acceptFileHeader tokens with
    | None -> None
    | Some (restTokens, (namesp, opened)) ->
        let allopened = Set.add namesp opened

        let rec acceptManyFileBodiesMembers tokens = 
            match acceptFileBodyMember namesp allopened tokens with
            | None -> tokens, Set.empty
            | Some (toks, args) ->        
                let (rest, lst) = acceptManyFileBodiesMembers toks
                rest, lst |> Set.add args

        match acceptManyFileBodiesMembers restTokens with
        | _::_, _ -> None
        | _, entries -> Some entries