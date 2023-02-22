module Loewe.Test.Lexing

open Loewe.Parsing.Tokenizing.Main

open Microsoft.VisualStudio.TestTools.UnitTesting


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

[<TestClass>]
type LexingTester () =


    [<TestMethod>]
    member this.``Most simple test program - lexing`` () =
        match tokenize simpleProgram with
        | Success _ -> ()
        | Failure _ -> Assert.Fail "Didn't successfully tokenize simple program"

    [<TestMethod>]
    member this.``Comment program - lexing`` () =
        match tokenize commentProgram with
        | Success tokens -> 
            if tokens.Length <> tokenCountCommentProgram then
                Assert.Fail "Parsing successfully failed - token count does not match"
        | Failure _ -> 
            Assert.Fail "Didn't successfully tokenize comment program"