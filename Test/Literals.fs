module Loewe.Test.Literals

open Loewe.Parsing.Lexer.SingleTokenLexer
open Loewe.Parsing.Lexer.TokenTypes
open Xunit



[<Fact>]
let ``Simple string literal`` () =
    Assert.Equal (Some ((Literal (String "sdg")), 5), (stringLiteral (ref "\"sdg\"")))

[<Fact>]
let ``Hard string literal`` () =
    Assert.Equal ((Some ((Literal (String "\ns\td\"g\\")), 13)), (stringLiteral (ref "\"\\ns\\td\\\"g\\\\\"")))

[<Fact>]
let ``Simple int literal`` () =
    Assert.Equal ((Some (Literal (Int 123), 3)), (numberLiteral (ref "123")))

[<Fact>]
let ``Hard int literal`` () =
    Assert.Equal ((Some ((Literal (Int -0x123)), 6)), (numberLiteral (ref "-0x123")))

[<Fact>]
let ``Simple double literal`` () =
    Assert.Equal ((Some ((Literal (Float 0123.125)), 8)), (numberLiteral (ref "0123.125")))

[<Fact>]
let ``Hard float literal`` () =
    Assert.Equal ((Some ((Literal (Float (float (-0.125f)))), 6)), (numberLiteral (ref "-.125f")))

[<Fact>]
let ``Faulty float literal`` () =
    Assert.Equal (None, (numberLiteral (ref "-0x125125f")))


[<Fact>]
let ``Faulty int literal`` () =
    Assert.Equal (None, (numberLiteral (ref "0x125125s")))
