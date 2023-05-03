module Loewe.Parsing.Composing.Main

open Helpers
open Loewe.Parsing.Composing.Error
open Loewe.Parsing.Composing.Types

type ComposingFileResult = 
    | Success of FileContent
    | Failure of ErrorTrace

let composeFile tokens = 
    match file tokens with
    | Result.Success (_, value) -> Success value
    | Result.Failure cet -> Failure cet