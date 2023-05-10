module Loewe.AST.EmptyTypes

open Loewe.AST.Types

let emptyName = ""
let emptyNamespace = Root
let emptyTopSymbol = {
    Name = emptyName
    Namespace = emptyNamespace
}
let emptyClass = {
    Symbol = emptyTopSymbol
    InstanceFields = Set.empty
    InstanceMethods = Set.empty
    ClassFields = Set.empty
    ClassMethods = Set.empty
}
let emptyClassMemberSymbol = {
    Name = emptyName
    Parent = emptyClass
}
let emptyType = Class emptyClass
let emptyQualifiedType = {
    Type = emptyType
    Qualifier = Owner
}
let emptyCodeSignature = {
    Parameters = List.empty
    Return = emptyQualifiedType
}
let emptyField = {
    Symbol = emptyClassMemberSymbol
    Type = emptyQualifiedType
    Name = emptyName
    AccessModifier = Public
}
let emptyMethod : Method = {
    Symbol = emptyClassMemberSymbol
    Signature = emptyCodeSignature
    Code = List.empty
    AccessModifier = Public
}
let emptyFunction = {
    Symbol = emptyTopSymbol
    Signature = emptyCodeSignature
    Code = List.empty
}