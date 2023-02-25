module Loewe.Parsing.Tree.Unresolved.Types

open Loewe.Parsing.Types
open Loewe.Parsing.Tokenizing.Types


type UnresolvedQualifiedTypeIdentifier = string * Namespace option

type UnresolvedExpression = 
    | BinaryOperation of UnresolvedExpression * BinaryOperation * UnresolvedExpression
    | UnaryOperation of UnaryOperation * UnresolvedExpression
    | Literal of Literal
    | Variable of string
    | Bracketed of UnresolvedExpression

and UnresolvedStatement = 
    | KnownAssignment of string * UnresolvedExpression
    | NewAssignment of UnresolvedQualifiedTypeIdentifier * string * UnresolvedExpression


type UnresolvedFunction = {
    Name: string
    Return: UnresolvedQualifiedTypeIdentifier
    Parameters: (UnresolvedQualifiedTypeIdentifier * string) list
    Code: UnresolvedStatement list
}

type UnresolvedAttribute = {
    Name: string
    Type: UnresolvedQualifiedTypeIdentifier
    AccessModifier: AccessModifier
}

type UnresolvedType = {
    Name: string
    Methods: (AccessModifier * UnresolvedFunction) Set
    Attributes: UnresolvedAttribute Set
}

type UnresolvedTopLevelEntry =
    | Class of Namespace * UnresolvedType * Namespace Set
    | Function of Namespace * UnresolvedFunction * Namespace Set