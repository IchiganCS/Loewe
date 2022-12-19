module Loewe.AST.Writer.Main

open Loewe.AST.Types
open System.IO

let rec formatNamespace = function
    | Root -> ""
    | Child (parent, name) -> (formatNamespace parent) + name

let formatTopSymbol (symbol: TopSymbol) =
    (formatNamespace symbol.Namespace) + symbol.Name


let formatType (typ: Type) =
    match typ with
    | Primitive p -> p.TranslatedName
    | Class c -> (formatTopSymbol c.Symbol)

let formatQualifiedType (qTyp: QualifiedType) =
    match qTyp.Qualifier with
    | Owner -> (formatType qTyp.Type)
    | Ref | Ptr -> (formatType qTyp.Type) + "*"
    | ConstPtr | ConstRef -> "const " + (formatType qTyp.Type) + "*"

let formatSignatureWithName (signature: CodeSignature) name =
    let mutable res = sprintf "%s %s(" (formatQualifiedType signature.Return) name
    for arg in signature.Parameters do
        res <- res + (sprintf "%s %s, " (formatQualifiedType arg.Type) arg.Name)

    if signature.Parameters.Length = 0 then
        res + ")"
    else
        res[0..res.Length - 3] + ")"

let formatCodeBlock (code: Statement list) =
    "{ }"


let formatFunction (func: Function) =
    (formatSignatureWithName func.Signature (formatTopSymbol func.Symbol)) +
    (formatCodeBlock func.Code)

let writeToStream (stream: TextWriter) (defines: Class Set * Function Set) =

    let classes = fst defines
    if classes.Count > 0 then
        printfn "Error: can't handle classes"
    
    let functions = snd defines
    for func in functions do
        stream.WriteLine (formatFunction func)