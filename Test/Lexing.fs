module Loewe.Test.Lexing

open Loewe.Parsing.Lexer

open Xunit



let simpleProgram = "int main() { string x = \"test\"; }"


[<Fact>]
let ``Most simple test program`` () =
    Assert.True (
        match MultiTokenLexer.fullString simpleProgram with
        | Ok _ -> true
        | _ -> false
    )


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
let ``Comment program`` () =
    match MultiTokenLexer.fullString commentProgram with
    | Ok tokens ->
        Assert.True (tokens.Length = tokenCountCommentProgram)
    | _ -> Assert.True false


let operatorProgram =
    "// the numbers indicate the token count
int main() // 4
{ // 5
    int x = \"test\" == \"test\"; // 12
    int y = 3 + 5 % 2; // 21 
    int z = 3 == 4; // 28
    bool a = !false; // 34
    int b = ~2; // 40
    int c = !false != !true; // 29
} //50"

let tokenCountoperatorProgram = 50

[<Fact>]
let ``Operator program`` () =
    match MultiTokenLexer.fullString operatorProgram with
    | Ok tokens ->
        Assert.Equal (tokens |> List.length, tokenCountoperatorProgram)
    | _ -> Assert.True false
