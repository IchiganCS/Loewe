open Loewe.Parsing.Main
open Loewe.AST.Types
open Loewe.AST.Writer.Main
open System.IO

let content = System.IO.File.ReadAllText "test.loewe"

let res = parse "int main() \n{ string x = \"test\"; }"


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