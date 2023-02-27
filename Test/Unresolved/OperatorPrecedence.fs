module Loewe.Unresolved.OperatorPrecedence

open Loewe.Parsing.Tree.Unresolved.Helpers
open Loewe.Parsing.Tree.Unresolved.Types
open Loewe.Parsing.Tokenizing.Main
open Loewe.Parsing.Types
open Xunit

let buildTestTokens str = 
    match tokenize str with
    | Success posList -> posList |> List.map (fun pt -> pt.Token)
    | Failure _ -> raise (System.Exception "The test is faulty")


let private aVar = UnresolvedExpression.Variable (UnresolvedQualifiedIdentifier ("a", None))
let private bVar = UnresolvedExpression.Variable (UnresolvedQualifiedIdentifier ("b", None))
let private cVar = UnresolvedExpression.Variable (UnresolvedQualifiedIdentifier ("c", None))
let private dVar = UnresolvedExpression.Variable (UnresolvedQualifiedIdentifier ("d", None))
let private eVar = UnresolvedExpression.Variable (UnresolvedQualifiedIdentifier ("e", None))


let private minus expr1 expr2 = UnresolvedExpression.BinaryOperation (expr1, BinaryOperation.Subtraction, expr2)
let private plus expr1 expr2 = UnresolvedExpression.BinaryOperation (expr1, BinaryOperation.Addition, expr2)
let private divide expr1 expr2 = UnresolvedExpression.BinaryOperation (expr1, BinaryOperation.Division, expr2)
let private multiply expr1 expr2 = UnresolvedExpression.BinaryOperation (expr1, BinaryOperation.Multiplication, expr2)
let private modulo expr1 expr2 = UnresolvedExpression.BinaryOperation (expr1, BinaryOperation.Modulo, expr2)

let toksBinOpPrecedence1 = buildTestTokens "a * b + c - d * e"
let toksBinOpPrecedence2 = buildTestTokens "a % b - c * d + e"
let toksBinOpPrecedence3 = buildTestTokens "a - b * c / d / e"

[<Fact>]
let testBinOpPrecdence1 () =
    match expression toksBinOpPrecedence1 with
    | ComposingResult.Success ([], expr) -> 
        Assert.Equal (expr, 
            (minus 
                (plus (multiply aVar bVar) cVar)
                (multiply dVar eVar)))
    | _ -> Assert.True false

[<Fact>]
let testBinOpPrecdence2 () =
    match expression toksBinOpPrecedence2 with
    | ComposingResult.Success ([], expr) -> 
        Assert.Equal (expr, 
            (plus 
                (minus
                    (modulo aVar bVar)
                    (multiply cVar dVar))
                eVar))
    | _ -> Assert.True false


[<Fact>]
let testBinOpPrecdence3 () =
    match expression toksBinOpPrecedence3 with
    | ComposingResult.Success ([], expr) -> 
        Assert.Equal (expr, 
            (minus 
                aVar
                (divide (divide 
                    (multiply bVar cVar)
                    dVar) eVar)))
    | _ -> Assert.True false