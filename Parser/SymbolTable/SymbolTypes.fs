module Loewe.Parser.SymbolTypes
open Types

type Attribute = {
    AccessModifier: AccessModifier
    Name: string
    Type: Type
    Owner: ClassStump
}

and Method = {
    AccessModifier: AccessModifier
    Name: string
    Parameters: (Type * string) list
    Return: Type
    CodeBlock: Statement list
}

and Type =
    | Primitive of string
    | Class of ClassStump

/// Classes are only stumps since we would have to make their attribute and method lists mutable.
/// They could not be created otherwise.
and ClassStump = {
    Namespace: Namespace
    Name: string
}

and Function = {
    Namespace: Namespace
    Name: string
    Parameters: (Type * string) list
    Return: Type
    CodeBlock: Statement list
}

and Expression = 
    | BinaryOperation of Expression * BinaryOperation * Expression

and Statement =
    | NewAssignment of Type * string * Expression
    | If of Expression * Statement list


type GlobalSymbol = 
    | ClassSymbol of ClassStump
    | FunctionSymbol of Function
    | AttributeSymbol of Attribute
    | MethodSymbol of Method