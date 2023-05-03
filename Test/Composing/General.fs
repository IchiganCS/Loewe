module Loewe.Test.Composing.General

open Loewe.Parsing.Composing
open Loewe.Parsing.Composing.Main
open Loewe.Parsing.Tokenizing.Main
open Loewe.Parsing.Types
open Xunit

let buildTestTokens str = 
    match tokenize str with
    | Success posList -> posList |> List.map (fun pt -> pt.Token)
    | Failure _ -> raise (System.Exception "The test is faulty")



let testProgram = buildTestTokens "
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
    match composeFile testProgram with
    | ComposingFileResult.Success file -> 
        let (_, _, members) = file
        Assert.Equal (2, members |> Set.count)
    | ComposingFileResult.Failure _ -> Assert.True false