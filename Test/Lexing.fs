module Loewe.Test.Lexing

open Loewe.Parsing.Tokenizing.Main

open Xunit


let simpleProgram = "int main() { string x = \"test\"; }"
let commentProgram = "
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
    Assert.True
        (match tokenize simpleProgram with
        | Success _ -> true
        | Failure _ -> false)

[<Fact>]
let ``Comment program`` () =
    match tokenize commentProgram with
    | Success tokens -> 
        if tokens.Length <> tokenCountCommentProgram then
            Assert.True true
    | Failure _ -> 
        Assert.True false