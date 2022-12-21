module Loewe.Test.Lexing

open Loewe.Parsing.Main

open Xunit

let simpleProgram = "int main() { string x = \"test\"; }"
let commentProgram = "
int main() // test
{ /* another test * fsd / confuseing marks/*// stop
    int x = 4 // stop  notice spaces here ->    
    ;
    /**/ int works = /*3*/ 3//3
}/*
*/"

[<Fact>]
let ``Always true`` () =
    Assert.True true

[<Fact>]
let ``Most simple test program - lexing`` () =
    match parse simpleProgram with
    | Success _ -> ()
    | Failure _ -> Assert.Fail "Didn't successfully tokenize simple program"

[<Fact>]
let ``Comment program - lexing`` () =
    match parse simpleProgram with
    | Success _ -> ()
    | Failure _ -> Assert.Fail "Didn't successfully tokenize comment program"