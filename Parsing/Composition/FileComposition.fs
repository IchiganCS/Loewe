module Loewe.Parsing.Composition.FileComposition
open Loewe.Parsing.Composition

let entireFile tokens = 
    match ConstructComposition.fileContent tokens with
    | Ok (_, value) -> Ok value
    | Error cet -> Error cet