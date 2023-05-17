module Loewe.Test.SymbolTable.GlobalSymbol

open Loewe.Definition.CodeConstructs
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
        this.setCounter(newCounter);
        return this.RecursionTest;
    }

    private void setCounter(int newCounter) {
        this.SomeCounter = newCounter;
    }
}


bool initializeCsWithCounter(int counter) {
    Test:Test:Cs cs = 3;
    cs.RecursionTest = cs;
    if (cs.getRecursionTest(4) == cs) {
        return true;
    }
    return false;
}
"""

[<Fact>]
let ``Code resolution test`` () =
    match MultiTokenLexer.fullString testProgram with
    | Error _ -> failwith "faulty test"
    | Ok tokens ->

    match FileComposition.entireFile (tokens |> List.map (fun t -> t.Token)) with
    | Error _ -> failwith "faulty test"
    | Ok composedFile ->

    match PartialResolve.fromFile composedFile with
    | Error _ -> failwith "faulty test"
    | Ok (symbols, namespaces) -> 

    match CodeResolve.fullResolve symbols namespaces with
    | Error e -> 
        printf "%s" (string e)
        Assert.True false
    | Ok _ -> ()
    