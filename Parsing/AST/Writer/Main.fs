module Loewe.AST.Writer.Main

open Loewe.AST.Types
open System.IO
open Format



let writeToStream (stream: TextWriter) (defines: Class Set * Function Set) =

    let functions = snd defines
    let classes = fst defines

    for clas in classes do
        stream.WriteLine (formatForwardClassDeclaration clas)
        
    for clas in classes do
        stream.WriteLine (formatClassDefinition clas)

    for func in functions do
        stream.WriteLine (formatFunctionDeclaration func)
        

    for func in functions do
        stream.WriteLine (formatFunctionDefinition func)