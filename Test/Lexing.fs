module Loewe.Test.Lexing

open Loewe.Parser.Lexer

open Xunit


let simpleProgram = "int main() { string x = \"test\"; }"

let commentProgram =
    "
int main() // test
{ //// stop
    // this should work 
    // emulated multiline comment
    int x = \"test\" //  notice spaces here ->    
;
    int works = \"3\";//3//s
}"

let tokenCountCommentProgram = 16

[<Fact>]
let ``Most simple test program`` () =
    Assert.True (
        match MultiTokenLexer.fullString simpleProgram with
        | Ok _ -> true
        | _ -> false
    )

[<Fact>]
let ``Comment program`` () =
    match MultiTokenLexer.fullString commentProgram with
    | Ok tokens ->
        if tokens.Length <> tokenCountCommentProgram then
            Assert.True true
    | _ -> Assert.True false
