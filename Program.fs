open Loewe.Parsing.Lexer
open Loewe.Parsing.Composition
open Loewe.Parsing.Composition.CompositionError
open Loewe.Parsing.SymbolTable

open Loewe.Parsing


let buildTestTokens str = 
    match MultiTokenLexer.fullString str with
    | Ok posList -> posList |> List.map (fun pt -> pt.Token)
    | Error _ -> failwith "The test is faulty"



let testProgram = buildTestTokens """
namespace Test:Test;

class Cs {
    public Cs RecursionTest;

    public Cs testMethod(int count) {
        return this.RecursionTest;
    }
}

bool initializeCsWithCounter(int counter) {
    Test:Test:Cs cs = 3;
    cs.RecursionTest = cs;
    if (cs.RecursionTest == cs) {
        return cs.testMethod();
    }
    return false;
}
"""

    
match FileComposition.entireFile testProgram with
| Ok members -> 
    match PartialResolve.fromFile members with
    | Error a -> ()
    | Ok (symbols, namespaces) ->
        match CodeResolve.fullResolve symbols namespaces with
        | Error e -> ()
        | Ok resolvedSymbols -> ()
| Error cet -> 
    printfn "%s" (cet |> Error.deepest |> Error.string)