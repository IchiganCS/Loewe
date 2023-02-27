module Loewe.Parsing.Tree.Unresolved.Types

open Loewe.Parsing.Types
open Loewe.Parsing.Tokenizing.Types


type UnresolvedQualifiedIdentifier = string * Namespace option

type UnresolvedExpression = 
    | BinaryOperation of UnresolvedExpression * BinaryOperation * UnresolvedExpression
    | UnaryOperation of UnaryOperation * UnresolvedExpression
    | Literal of Literal
    | Variable of UnresolvedQualifiedIdentifier
    | Attribute of UnresolvedExpression * string
    | Bracketed of UnresolvedExpression
    | FunctionCall of UnresolvedQualifiedIdentifier * UnresolvedExpression list
    | MethodCall of UnresolvedExpression * string * UnresolvedExpression list

type UnresolvedStatement = 
    | KnownAssignment of UnresolvedExpression * UnresolvedExpression
    | NewAssignment of UnresolvedQualifiedIdentifier * string * UnresolvedExpression
    | If of UnresolvedExpression * UnresolvedStatement list
    | IfElse of UnresolvedExpression * UnresolvedStatement list * UnresolvedStatement list
    | IfElifs of (UnresolvedExpression * UnresolvedStatement list) list
    | IfElifElse of (UnresolvedExpression * UnresolvedStatement list) list * UnresolvedStatement list
    | Expression of UnresolvedExpression
    | While of UnresolvedExpression * UnresolvedStatement list
    | Return of UnresolvedExpression


type UnresolvedFunction = {
    Name: string
    Return: UnresolvedQualifiedIdentifier
    Parameters: (UnresolvedQualifiedIdentifier * string) list
    Code: UnresolvedStatement list
}

type UnresolvedMethod = {
    Name: string
    Return: UnresolvedQualifiedIdentifier
    Parameters: (UnresolvedQualifiedIdentifier * string) list
    Code: UnresolvedStatement list
    AccessModifier: AccessModifier
}

type UnresolvedAttribute = {
    Name: string
    Type: UnresolvedQualifiedIdentifier
    AccessModifier: AccessModifier
}

type UnresolvedClassMember =
    | Method of UnresolvedMethod
    | Attribute of UnresolvedAttribute

type UnresolvedType = {
    Name: string
    Members: UnresolvedClassMember Set
}

type UnresolvedTopLevelEntry =
    | Class of UnresolvedType
    | Function of UnresolvedFunction

type UnresolvedFile = Namespace * Namespace Set * UnresolvedTopLevelEntry Set



type ComposingErrorTrace = 
    | End
    | Linear of Token list * string * ComposingErrorTrace
    | Multiple of Token list * string * ComposingErrorTrace Set
    
/// A result of composing. If the process was completed successfully, the list of remaining tokens and the identified object is returned.
/// In case of a failure, a list of failures (similar to a backtrace) is provided.
type ComposingResult<'a> =
    | Success of Token list * 'a
    | Failure of ComposingErrorTrace