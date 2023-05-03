open Loewe.Parsing.Tokenizing.Main
open Loewe.Parsing.Tokenizing
open Loewe.Parsing.Tokenizing.Main
open Loewe.Parsing.Types
open Loewe.Parsing.Composing.Main

open Loewe.Parsing
open Loewe.Parsing.Composing.Error

let buildTestTokens str = 
    match tokenize str with
    | Result.Success posList -> posList |> List.map (fun pt -> pt.Token)
    | Result.Failure _ -> raise (System.Exception "The test is faulty")



let testProgram = buildTestTokens "
    namespace Test;
    open Test:Asgasg;
    
    class Holter {
        public int diPolter;
    
        private void dont() {
            
        }

        private int s() {
            return diPolter;
        }
    }
    
    asgpih asdg(awef name, sdg aegas) {
        int x = asd + asdgasd * etst + 124;
        if (y) {
            asgun(12512, 346, \"asfasf\");
            3 + 5;
            y = 125125;
            c.s() = 3;
            x = 3;
        }
    
        while (sadf.diPolter) {
            Test:Holter sadf = Test:Holter();
            sadf.diPolter = 4124;
            sadf.dont();();
        }
    }"

    
match composeFile testProgram with
| ComposingFileResult.Success members -> ()
| ComposingFileResult.Failure cet -> ()