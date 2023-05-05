module Loewe.Parser.Composer.FileComposer
open Loewe.Parser.Composer

let entireFile tokens = 
    match ConstructComposer.fileContent tokens with
    | Ok (_, value) -> Ok value
    | Error cet -> Error cet