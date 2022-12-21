module Loewe.Test.Literals

open Loewe.Tokenizing.Helpers
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type LiteralsTester () =
    [<TestMethod>]
    member this.``Simple string literal`` () =
        match tokenizeStringLiteral "\"sdg\"" with
        | Some _ -> ()
        | None -> Assert.Fail "Didn't match simple string"

    [<TestMethod>]
    member this.``Hard string literal`` () =
        match tokenizeStringLiteral "\"\ns\td\\\"g\\\"" with
        | Some _ -> ()
        | None -> Assert.Fail "Didn't match hard string"

    [<TestMethod>]
    member this.``Simple int literal`` () =
        match tokenizeNumberLiteral "123" with
        | Some _ -> ()
        | None -> Assert.Fail "Didn't match simple literal"

    [<TestMethod>]
    member this.``Hard int literal`` () =
        match tokenizeNumberLiteral "-0x123" with
        | Some _ -> ()
        | None -> Assert.Fail "Didn't match hexadecimal literal"

    [<TestMethod>]
    member this.``Simple double literal`` () =
        match tokenizeNumberLiteral "0123.125" with
        | Some _ -> ()
        | None -> Assert.Fail "Didn't match double literal"

    [<TestMethod>]
    member this.``Hard float literal`` () =
        match tokenizeNumberLiteral "-.125f" with
        | Some _ -> ()
        | None -> Assert.Fail "Didn't match hard float literal"

    [<TestMethod>]
    member this.``Faulty float literal`` () =
        match tokenizeNumberLiteral "-0x125125f" with
        | Some _ -> Assert.Fail "Matched faulty float literal"
        | None -> ()

        
    [<TestMethod>]
    member this.``Faulty int literal`` () =
        match tokenizeNumberLiteral "0x125125s" with
        | Some _ -> Assert.Fail "Matched faulty int literal"
        | None -> ()