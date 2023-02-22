module Loewe.AST.Defaults

open Loewe.AST.Types

let defaultPrimitives = Set.ofList [
    { Name = "float"; TranslatedName = "float" }
    { Name = "double"; TranslatedName = "double" }
    { Name = "int"; TranslatedName = "int" }
    { Name = "uint"; TranslatedName = "unsigned int" }
    { Name = "long"; TranslatedName = "long long" }
    { Name = "ulong"; TranslatedName = "unsigned long long" }
    { Name = "bool"; TranslatedName = "int" }
    { Name = "byte"; TranslatedName = "char"}
]


type DefaultValues = {
    Value: string
    TranslatedValue: string
}

let defaultValues = Set.ofList [
    { Value = "true"; TranslatedValue = "1" }
    { Value = "false"; TranslatedValue = "0" }
]