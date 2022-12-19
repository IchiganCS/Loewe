module Loewe.AST.Requirements.Checks

open Loewe.AST.Requirements.Types
open Loewe.AST.Types

type RequirementError =
    | InaccurateSymbol
    | MissingSymbol

// let areRequirementsMet (defines: Class Set * Function Set) (requirements: Requirement Set) =
//     for req in requirements do
//         match req with
//         | Class slr -> 
//             fst defines
//             |> Set.filter (fun cl -> slr)