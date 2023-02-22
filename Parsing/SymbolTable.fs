module Loewe.Parsing.SymbolTable
open CommonTypes


type UnresolvedClass = {
    Name: string option
    Namespace: Namespace Set option
    Methods: MethodSymbol Set
}

and ResolvedClass = {
    Name: string
    Namespace: Namespace
    Methods: ResolvedMethod Set
}

and ResolvedType = 
    | Class of ResolvedClass
    | Primitive of string

and ResolvedMethod = {
    Name: string
    Owner: ResolvedClass
    Return: ResolvedType
    Parameters: ResolvedType list
}

and UnresolvedMethod = {
    Name: string
    Owner: TypeSymbol
    Parameters: TypeSymbol list
}

and ResolvedFunction = {
    Name: string
    Namespace: Namespace
    Return: ResolvedType
    Parameters: ResolvedType list
}

and UnresolvedFunction = {
    Name: string
    Namespace: Namespace Set
    
}


and TypeSymbol =
    | ResolvedType of ResolvedType
    | UnresolvedClass of UnresolvedClass

and MethodSymbol =
    | ResolvedMethod of ResolvedMethod
    | UnresolvedMethod of UnresolvedMethod