module Loewe.Parsing.Tree.Unresolved.Main
open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.Tree.Unresolved.Helpers
open Types

type ComposingFileResult = 
    | Success of UnresolvedFile
    | Failure of ComposingErrorTrace

let composeFile tokens = 
    match file tokens with
    | ComposingResult.Success (_, value) -> Success value
    | ComposingResult.Failure cet -> Failure cet