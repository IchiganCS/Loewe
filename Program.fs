open Loewe.Tokenizing.Main
open Loewe.AST.Types
open Loewe.AST.Writer.Main
open System.IO
open Loewe.Tokenizing.Helpers

let content = System.IO.File.ReadAllText "test.loewe"


match tokenizeNumberLiteral "0123.125" with
| Some _ -> ()
| None -> ()


let main : Function = { 
    Symbol = {
        Namespace = Child (Root, "Test")
        Name = "main"
    }
    Code = List.singleton 
        (InitializeVariable ({
            Name = "test"
            Type = {
                Type = Primitive {
                    Name = "int"
                    TranslatedName = "int"
                }
                Qualifier = Owner
            }
        }, Constant "3"))
    
    Signature = {
        Return = {
            Type = Primitive {
                Name = "int"
                TranslatedName = "int"
            }
            Qualifier = Owner
        }
        Parameters = List.empty
    }
}


writeToStream (System.Console.Out) (Set.empty, Set.singleton main)