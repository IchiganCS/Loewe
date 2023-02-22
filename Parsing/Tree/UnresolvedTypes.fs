module Loewe.Parsing.Tree.UnresolvedTypes

open Loewe.Parsing.CommonTypes


type UnresolvedNamespacedIdentifier = string * Namespace Set

type UnresolvedExpression = 
    | BinaryOperation of UnresolvedExpression * BinaryOperation * UnresolvedExpression

and UnresolvedStatement = 
    | KnownAssignment of string * UnresolvedExpression
    | NewAssignment of UnresolvedNamespacedIdentifier * string * UnresolvedExpression


type UnresolvedCodeBlock = UnresolvedCodePart list
and UnresolvedCodePart =
    | UnresolvedStatement of UnresolvedStatement
    | UnresolvedCodeBlock of UnresolvedCodeBlock


type UnresolvedFunction = {
    Name: UnresolvedNamespacedIdentifier
    Return: UnresolvedNamespacedIdentifier
    Parameters: (UnresolvedNamespacedIdentifier * string) list
    Code: UnresolvedCodeBlock
}

type UnresolvedAttribute = {
    Name: string
    Type: UnresolvedNamespacedIdentifier
    AccessModifier: AccessModifier
}

type UnresolvedType = {
    Name: string
    Methods: UnresolvedFunction Set
    Attributes: UnresolvedAttribute Set
    Namespace: Namespace
}