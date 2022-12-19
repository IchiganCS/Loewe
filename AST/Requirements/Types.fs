module Loewe.AST.Requirements.Types

open Loewe.AST.Types

type QualifiedTypeRequirement = {
    Name: string
    Qualifier: TypeQualifier
}
type SignatureRequirement = {
    Name: string
    Return: QualifiedTypeRequirement
    Parameters: (string * QualifiedTypeRequirement) Set
}
type SymbolLocationRequirement = {
    Name: string
    Namespaces: Namespace Set
}

type Requirement =
    | Class of SymbolLocationRequirement
    | Method of SymbolLocationRequirement * AccessModifier * SignatureRequirement
    | Function of SymbolLocationRequirement * SignatureRequirement