namespace Loewe.Parsing.Composition.CompositionTypes

open Loewe.Parsing.Types
open Loewe.Parsing.Lexer.TokenTypes


type QualifiedIdentifier = string * Namespace option

type Expression = 
    | BinaryOperation of Expression * BinaryOperation * Expression
    | UnaryOperation of UnaryOperation * Expression
    | Literal of Literal
    | Variable of QualifiedIdentifier
    | Attribute of Expression * string
    | Bracketed of Expression
    | FunctionCall of QualifiedIdentifier * Expression list
    | MethodCall of Expression * string * Expression list

type Statement = 
    | KnownAssignment of Expression * Expression
    | NewAssignment of QualifiedIdentifier * string * Expression
    | If of Expression * Statement list
    | IfElse of Expression * Statement list * Statement list
    | IfElifs of (Expression * Statement list) list
    | IfElifElse of (Expression * Statement list) list * Statement list
    | Expression of Expression
    | While of Expression * Statement list
    | Return of Expression


type Function = {
    Name: string
    Return: QualifiedIdentifier
    Parameters: (QualifiedIdentifier * string) list
    Code: Statement list
}

type Method = {
    Name: string
    Return: QualifiedIdentifier
    Parameters: (QualifiedIdentifier * string) list
    Code: Statement list
    AccessModifier: AccessModifier
}

type Attribute = {
    Name: string
    Type: QualifiedIdentifier
    AccessModifier: AccessModifier
}

type ClassMember =
    | Method of Method
    | Attribute of Attribute

type Class = {
    Name: string
    Members: ClassMember Set
}

type TopLevelEntry =
    | ClassEntry of Class
    | FunctionEntry of Function

type FileContent = Namespace * Namespace Set * TopLevelEntry Set
