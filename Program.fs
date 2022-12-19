open Loewe.Parsing.Main
open Loewe.AST.Types
open Loewe.AST.Writer.Main
open System.IO

let content = System.IO.File.ReadAllText "test.loewe"

let main : Function = { 
    Symbol = {
        Namespace = Child (Root, "Test")
        Name = "main"
    }
    Code = List.empty
    Signature = {
        Return = {
            Type = Primitive {
                Name = "int"
                TranslatedName = "int"
            }
            Qualifier = ConstPtr
        }
        Parameters = List.empty
    }
}

let out = new FileStream ("out.txt", FileMode.Create)


writeToStream (System.Console.Out) (Set.empty, Set.singleton main)