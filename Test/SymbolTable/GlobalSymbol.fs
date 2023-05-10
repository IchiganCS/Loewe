module Loewe.Test.SymbolTable

open Loewe.Parsing.SymbolTable
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
        setCounter(newCounter);
        return RecursionTest;
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

    match SymbolTableBuilder.fromFile composedFile with
    | Error _ -> Assert.True false
    | Ok res -> 
        Assert.Equal ((1, 1, 2, 2),
            (res |> Set.filter (function | SymbolTypes.ClassSymbol _ -> true | _ -> false) |> Set.count,
            res |> Set.filter (function | SymbolTypes.FunctionSymbol _ -> true | _ -> false) |> Set.count,
            res |> Set.filter (function | SymbolTypes.AttributeSymbol _ -> true | _ -> false) |> Set.count,
            res |> Set.filter (function | SymbolTypes.MethodSymbol _ -> true | _ -> false) |> Set.count)
        )