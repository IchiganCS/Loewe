module Loewe.Parsing.Composing.Main

open Helpers
open Loewe.Parsing.Composing.Error
open Loewe.Parsing.Composing.Types

let composeFile tokens = 
    match file tokens with
    | Ok (_, value) -> Ok value
    | Error cet -> Error cet