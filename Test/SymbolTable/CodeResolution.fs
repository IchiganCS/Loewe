module Loewe.Test.SymbolTable.CodeResolution

open Loewe.Definition.CodeConstructs
open Loewe.Parsing.SymbolTable.PartialResolve
open Loewe.Parsing.Lexer
open Loewe.Parsing.Composition

open Xunit

let testProgram = 
    """
namespace Test:Test;

class Cs {
    public int SomeCounter;

    public Cs RecursionTest;

    public Cs getRecursionTest(int newCounter) {
        this.setCounter(newCounter);
        return this.RecursionTest;
    }

    private void setCounter(int newCounter) {
        SomeCounter = newCounter;
    }
}


bool initializeCsWithCounter(int counter) {
    Test:Test:Cs cs = Cs();
    cs.RecursionTest = cs;
    if (cs.getRecursionTest(4) == cs) {
        return true;
    }
    return false;
}
"""

[<Fact>]
let ``Global symbol resolution`` () =
    match MultiTokenLexer.fullString testProgram with
    | Error _ -> raise (Failure "faulty test")
    | Ok tokens ->

    match FileComposition.entireFile (tokens |> List.map (fun t -> t.Token)) with
    | Error _ -> raise (Failure "faulty test")
    | Ok composedFile ->

    match fromFile composedFile with
    | Error _ -> Assert.True false
    | Ok res -> 
        Assert.Equal ((1, 1, 2, 2),
            (fst res |> List.sumBy (function | ClassSymbol _ -> 1 | _ -> 0),
            fst res |> List.sumBy (function | FunctionSymbol _ -> 1 | _ -> 0),
            fst res |> List.sumBy (function | MethodSymbol _ -> 1 | _ -> 0),
            fst res |> List.sumBy (function | AttributeSymbol _ -> 1 | _ -> 0))
        )