module Loewe.Test.Literals

open Loewe.Parsing.Tokenizing.Helpers
open Loewe.Parsing.Tokenizing.Types
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type LiteralsTester () =
    [<TestMethod>]
    member this.``Simple string literal`` () =
        match tokenizeStringLiteral (ref "\"sdg\"") with
        | Some v ->
            match fst v with
            | Literal l ->
                match l with
                | String str -> 
                    if str <> "sdg" then
                        Assert.Fail "Didn't match simple string literal"
                | _ -> Assert.Fail "Didn't match simple string literal"
            | _ -> Assert.Fail "Didn't match simple string literal"
        | None -> Assert.Fail "Didn't match simple string literal"

    [<TestMethod>]
    member this.``Hard string literal`` () =
        match tokenizeStringLiteral (ref "\"\\ns\\td\\\"g\\\\\"") with
        | Some v ->
            match fst v with
            | Literal l ->
                match l with
                | String str -> 
                    if str <> "\ns\td\"g\\" then
                        Assert.Fail "Didn't match hard string literal"
                | _ -> Assert.Fail "Didn't match hard string literal"
            | _ -> Assert.Fail "Didn't match hard string literal"
        | None -> Assert.Fail "Didn't match hard string literal"

    [<TestMethod>]
    member this.``Simple int literal`` () =
        match tokenizeNumberLiteral (ref "123") with
        | Some _ -> ()
        | None -> Assert.Fail "Didn't match simple literal"

    [<TestMethod>]
    member this.``Hard int literal`` () =
        match tokenizeNumberLiteral (ref "-0x123") with
        | Some v ->
            match fst v with
            | Literal l ->
                match l with
                | Int i -> 
                    if i <> -0x123 then
                        Assert.Fail "Didn't match hexadecimal literal"
                | _ -> Assert.Fail "Didn't match hexadecimal literal"
            | _ -> Assert.Fail "Didn't match hexadecimal literal"
        | None -> Assert.Fail "Didn't match hexadecimal literal"

    [<TestMethod>]
    member this.``Simple double literal`` () =
        match tokenizeNumberLiteral (ref "0123.125") with
        | Some _ -> ()
        | None -> Assert.Fail "Didn't match double literal"

    [<TestMethod>]
    member this.``Hard float literal`` () =
        match tokenizeNumberLiteral (ref "-.125f") with
        | Some v ->
            match fst v with
            | Literal l ->
                match l with
                | Float f -> 
                    if f <> float (-0.125f) then
                        Assert.Fail "Didn't match hard float literal"
                | _ -> Assert.Fail "Didn't match hard float literal"
            | _ -> Assert.Fail "Didn't match hard float literal"
        | None -> Assert.Fail "Didn't match hard float literal"

    [<TestMethod>]
    member this.``Faulty float literal`` () =
        match tokenizeNumberLiteral (ref "-0x125125f") with
        | Some _ -> Assert.Fail "Matched faulty float literal"
        | None -> ()

        
    [<TestMethod>]
    member this.``Faulty int literal`` () =
        match tokenizeNumberLiteral (ref "0x125125s") with
        | Some _ -> Assert.Fail "Matched faulty int literal"
        | None -> ()