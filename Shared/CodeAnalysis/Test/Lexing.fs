module Loewe.Shared.CodeAnalysis.Test

open Loewe.Shared.CodeAnalysis.Lexing
open Loewe.Shared.Utility.StringRef
open Xunit
open Position
open Token

let simpleProgram = "int main() { string x = \"test\"; }"


[<Fact>]
let ``Most simple test program`` () =
    Assert.True (
        match lexString "" simpleProgram with
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

let faultyCommentProgram =
    "
int ma$in() // notice the faulty dollar sign on the left
{ //// stop
    // this should work 
    // emulated multiline comment
    int x = \"test\" //  notice spaces here ->    
;
    int works = \"3\";//3//s
}"


let tokenCountCommentProgram = 24

[<Fact>]
let ``Comment program`` () =
    match lexString "" commentProgram with
    | Ok tokens -> Assert.True (tokens.Length = tokenCountCommentProgram)
    | _ -> Assert.True false

[<Fact>]
let ``Faulty comment program`` () =
    match lexString "" faultyCommentProgram with
    | Ok _ -> Assert.True false
    | Error (errors, toks) ->
        Assert.Equal (tokenCountCommentProgram + 1, toks |> List.length)
        Assert.Equal<(LinePosition * char) list>([ { FileName = ""; Line = 1; Column = 6; Length = 1 }, '$' ], errors)


let operatorProgram =
    "// 1 - the numbers indicate the token count
int main() // 6
{ // 8
    int x = \"test\" == \"test\"; // 16
    int y = 3 + 5 % 2; // 26
    int z = 3 == -4; // 34
    bool a = !false; // 41
    int b = ~2; // 48
    int c = !false != !true; // 58
} //60
"

let tokenCountoperatorProgram = 60

[<Fact>]
let ``Operator program`` () =
    match lexString "" operatorProgram with
    | Ok tokens -> Assert.Equal (tokens |> List.length, tokenCountoperatorProgram)
    | _ -> Assert.True false



[<Fact>]
let ``Simple string literal`` () =
    match lexStringLiteral ("\"sdg\"" |> StringRef.ofString) with
    | Some ("sdg", _) -> ()
    | _ -> Assert.True false

[<Fact>]
let ``Hard string literal`` () =
    match lexStringLiteral ("\"\\ns\\td\\\"g\\\\\"" |> StringRef.ofString) with
    | Some ("\ns\td\"g\\", _) -> ()
    | _ -> Assert.True false

[<Fact>]
let ``Simple int literal`` () =
    match lexNumberLiteral ("123" |> StringRef.ofString) with
    | Some (Int 123, _) -> ()
    | _ -> Assert.True false

[<Fact>]
let ``Hard int literal`` () =
    match lexNumberLiteral ("-0x123" |> StringRef.ofString) with
    | Some (Int -0x123, _) -> ()
    | _ -> Assert.True false

[<Fact>]
let ``Simple double literal`` () =
    match lexNumberLiteral ("0123.125" |> StringRef.ofString) with
    | Some (Float 0123.125f, _) -> ()
    | _ -> Assert.True false

[<Fact>]
let ``Hard float literal`` () =
    match lexNumberLiteral ("-.125f" |> StringRef.ofString) with
    | Some (Float (-0.125f), _) -> ()
    | _ -> Assert.True false

[<Fact>]
let ``Faulty float literal`` () =
    match lexNumberLiteral ("-0x125125f" |> StringRef.ofString) with
    | None -> ()
    | _ -> Assert.True false


[<Fact>]
let ``Faulty int literal`` () =
    match lexNumberLiteral ("0x125125s" |> StringRef.ofString) with
    | None -> ()
    | _ -> Assert.True false
