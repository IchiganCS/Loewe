module Loewe.Parsing.SyntaxDefinitions
open System.Text.RegularExpressions
open Loewe.AST.Defaults
open Loewe.AST.Types

let typeKeywords = 
    defaultPrimitives
    |> Set.map (fun (p : Primitive) -> p.Name)

let otherKeywords = Set.ofList [
    "const";
]

let cKeywords = 
    defaultPrimitives
    |> Set.toArray 
    |> Array.collect (fun (p : Primitive) -> p.TranslatedName.Split ())
    |> Set.ofArray

let keywords = Set.unionMany [typeKeywords; otherKeywords; cKeywords]

let isKeyword str = Set.contains str keywords
let isNotKeyword str = str |> isKeyword |> not

let variableRegex = Regex ("[a-z | a-Z | _][a-z | a-Z | \d | _]*", RegexOptions.Compiled)

let classNameRegex = variableRegex

let isNotKeywordAndMatchesRegex str (regex : Regex) =
    (isNotKeyword str) && (regex.IsMatch str)

let isValidVariableName str =
    isNotKeywordAndMatchesRegex str variableRegex

let isValidClassName str =
    isNotKeywordAndMatchesRegex str classNameRegex

