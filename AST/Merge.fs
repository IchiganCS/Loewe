module Loewe.AST.Merge

open Loewe.AST.Types
open Loewe.AST.EmptyTypes

/// The following functions provide a unified interface to merge different kinds of data.
/// This is done by using the option type extensively which also provides an option to check
/// if the data can even be merged. This is used for any name or namespace or any other kind of type.

let mergeName name1 name2 : string option =
    if name2 = emptyName then
        Some name1
    elif name1 = emptyName then
        Some name2
    elif name1 = name2 then
        Some name1
    else
        None

let mergeNamespace namesp1 namesp2 : Namespace option =
    if namesp1 = emptyNamespace then
        Some namesp2
    elif namesp2 = emptyNamespace then
        Some namesp1
    elif namesp1 = namesp2 then
        Some namesp1
    else
        None


// let mergeClass class1 class2 : Class option =
//     match mergeName class1.Name class2.Name with
//     | None -> None
//     | Some name ->

//     match mergeNamespace class1.Namespace class2.Namespace with
//     | None -> None
//     | Some namesp ->



//     { Name = name; Namespace = namesp }




// let mergeClasses (classes1 : Set<Class>) classes2 : Set<Class> =
//     let conflicts =
//         classes1
//         |> Set.filter (fun cl -> Set.exists (isEqualQualificationClass cl) classes2)
    
        


// let mergeFunctions functions1 functions2 : Set<Function> =



// let mergeDefines def1 def2 : Defines =
//     (mergeClasses def1.Classes def2.Classes), (mergeFunctions def2.Functions def2.Functions)