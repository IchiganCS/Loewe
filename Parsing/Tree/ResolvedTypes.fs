module Loewe.Parsing.Tree.ResolvedTypes
open Loewe.Parsing.CommonTypes


type AttributeDefinition = {
    Name: string
    Type: TypeDefinition
    Owner: TypeDefinition
}

and MethodDefinition = {
    Name: string
    Owner: TypeDefinition
    Return: TypeDefinition
    Parameters: (TypeDefinition * string) list
    Code: Codeblock
}

and FunctionDefinition = {
    Name: string
    Namespace: Namespace
    Return: TypeDefinition
    Parameters: (TypeDefinition * string) list
    Code: Codeblock
}


and TypeDefinition = {
    Name: string
    Namespace: Namespace
    mutable Methods: MethodDefinition Set
    mutable StaticMethods: MethodDefinition Set
    mutable Attributes: AttributeDefinition Set
    mutable StaticAttributes: AttributeDefinition Set
}

and Codeblock = Codeblockpart list

and Codeblockpart =
    | Statement of Statement
    | Block of Codeblock

and Scope = 
    | Namespace of Namespace
    | Codeblock of Codeblock
    | Class of TypeDefinition

and Expression = 
    | BinaryOperation of Expression * BinaryOperation * Expression

and Statement =
    | NewAssignment of TypeDefinition * string * Expression
    | If of Expression * Codeblock