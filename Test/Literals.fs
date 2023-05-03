module Loewe.Test.Literals

open Loewe.Parsing.Tokenizing.Helpers
open Loewe.Parsing.Tokenizing
open Xunit



[<Fact>]
let ``Simple string literal`` () =
    Assert.Equal (Some ((Literal (String "sdg")), 5), (tokenizeStringLiteral (ref "\"sdg\"")))

[<Fact>]
let ``Hard string literal`` () =
    Assert.Equal ((Some ((Literal (String "\ns\td\"g\\")), 13)), (tokenizeStringLiteral (ref "\"\\ns\\td\\\"g\\\\\"")))

[<Fact>]
let ``Simple int literal`` () =
    Assert.Equal ((Some ((Literal (Int 123)), 3)), (tokenizeNumberLiteral (ref "123")))

[<Fact>]
let ``Hard int literal`` () =
    Assert.Equal ((Some ((Literal (Int -0x123)), 6)), (tokenizeNumberLiteral (ref "-0x123")))

[<Fact>]
let ``Simple double literal`` () =
    Assert.Equal ((Some ((Literal (Float 0123.125)), 8)), (tokenizeNumberLiteral (ref "0123.125")))

[<Fact>]
let ``Hard float literal`` () =
    Assert.Equal ((Some ((Literal (Float (float (-0.125f)))), 6)), (tokenizeNumberLiteral (ref "-.125f")))

[<Fact>]
let ``Faulty float literal`` () =
    Assert.Equal (None, (tokenizeNumberLiteral (ref "-0x125125f")))

    
[<Fact>]
let ``Faulty int literal`` () =
    Assert.Equal (None, (tokenizeNumberLiteral (ref "0x125125s")))