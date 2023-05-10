open Loewe.Parsing.Lexer
open Loewe.Parsing.Types
open Loewe.Parsing.Composition
open Loewe.Parsing.Composition.CompositionError
open Loewe.Parsing.SymbolTable

open Loewe.Parsing


let buildTestTokens str = 
    match MultiTokenLexer.fullString str with
    | Ok posList -> posList |> List.map (fun pt -> pt.Token)
    | Error _ -> raise (System.Exception "The test is faulty")



let testProgram = buildTestTokens """
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

    
match FileComposition.entireFile testProgram with
| Ok members -> 
    let tes = SymbolTableBuilder.fromFile members
    ()
| Error cet -> 
    printfn "%s" (cet |> Error.deepest |> Error.string)