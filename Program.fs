open Loewe.Parser.Lexer
open Loewe.Parser.Types
open Loewe.Parser.Composer

open Loewe.Parser
open Loewe.Parser.Composer.Error

let buildTestTokens str = 
    match MultiTokenLexer.fullString str with
    | Ok posList -> posList |> List.map (fun pt -> pt.Token)
    | Error _ -> raise (System.Exception "The test is faulty")



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

    
match FileComposer.entireFile testProgram with
| Ok members -> ()
| Error cet -> 
    printfn "%s" (cet |> Error.deepest |> Error.string)