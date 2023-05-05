module Loewe.Test.Composing.OperatorPrecedence

open Loewe.Parser.Composer.ConstructComposer
open Loewe.Parser.Composer.ComposerTypes
open Loewe.Parser.Lexer
open Loewe.Parser.Types
open Xunit

let buildTestTokens str =
    match MultiTokenLexer.fullString str with
    | Ok posList -> posList |> List.map (fun pt -> pt.Token)
    | Error _ -> raise (System.Exception "The test is faulty")


let private aVar = Expression.Variable ("a", None)
let private bVar = Expression.Variable ("b", None)
let private cVar = Expression.Variable ("c", None)
let private dVar = Expression.Variable ("d", None)
let private eVar = Expression.Variable ("e", None)


let private minus expr1 expr2 =
    Expression.BinaryOperation (expr1, BinaryOperation.Subtraction, expr2)

let private plus expr1 expr2 =
    Expression.BinaryOperation (expr1, BinaryOperation.Addition, expr2)

let private divide expr1 expr2 =
    Expression.BinaryOperation (expr1, BinaryOperation.Division, expr2)

let private multiply expr1 expr2 =
    Expression.BinaryOperation (expr1, BinaryOperation.Multiplication, expr2)

let private modulo expr1 expr2 =
    Expression.BinaryOperation (expr1, BinaryOperation.Modulo, expr2)

let toksBinOpPrecedence1 = buildTestTokens "a * b + c - d * e"
let toksBinOpPrecedence2 = buildTestTokens "a % b - c * d + e"
let toksBinOpPrecedence3 = buildTestTokens "a - b * c / d / e"

[<Fact>]
let testBinOpPrecdence1 () =
    match expression toksBinOpPrecedence1 with
    | Ok ([], expr) -> Assert.Equal (expr, (minus (plus (multiply aVar bVar) cVar) (multiply dVar eVar)))
    | _ -> Assert.True false

[<Fact>]
let testBinOpPrecdence2 () =
    match expression toksBinOpPrecedence2 with
    | Ok ([], expr) -> Assert.Equal (expr, (plus (minus (modulo aVar bVar) (multiply cVar dVar)) eVar))
    | _ -> Assert.True false


[<Fact>]
let testBinOpPrecdence3 () =
    match expression toksBinOpPrecedence3 with
    | Ok ([], expr) -> Assert.Equal (expr, (minus aVar (divide (divide (multiply bVar cVar) dVar) eVar)))
    | _ -> Assert.True false
