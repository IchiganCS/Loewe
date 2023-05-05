module Loewe.Test.Composing.General

open Loewe.Parser.Composer
open Loewe.Parser.Lexer
open Loewe.Parser.Types
open Xunit

let buildTestTokens str =
    match MultiTokenLexer.fullString str with
    | Ok posList -> posList |> List.map (fun pt -> pt.Token)
    | Error _ -> raise (System.Exception "The test is faulty")



let testProgram =
    buildTestTokens
        "
namespace Test;
open Test:Asgasg;

class Holter {
    public int diPolter;

    private void dont() {
        
    }
}

asgpih asdg(awef name, sdg aegas) {
    int x = asd + asdgasd * etst + 124;
    if (y) {
        asgun(12512, 346, \"asfasf\");
        y = 125125;
    }

    while (x) {
        Holter sadf = Holter();
        sadf.diPolter = 4124;
        sadf.dont();
    }
}"

[<Fact>]
let ``Simple test program`` () =
    match FileComposer.entireFile testProgram with
    | Ok file ->
        let (_, _, members) = file
        Assert.Equal (2, members |> Set.count)
    | Error _ -> Assert.True false
