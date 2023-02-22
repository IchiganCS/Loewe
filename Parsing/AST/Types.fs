module Loewe.AST.Types
open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.CommonTypes


type Primitive = {
    Name: string
    TranslatedName: string
}

type QualifiedType = { 
    Type: Type
    Qualifier: TypeQualifier 
}

and TopSymbol = {
    Name: string
    Namespace: Namespace
}

and ClassMemberSymbol = {
    Name: string
    Parent: Class
}

and Type =
    | Class of Class
    | Primitive of Primitive

and Variable = {
    Name : string
    Type: QualifiedType
}
    
and CodeSignature = {
    Parameters: Variable list
    Return: QualifiedType
}


and Method = {
    Symbol: ClassMemberSymbol
    Signature: CodeSignature
    Code: Statement list
    AccessModifier: AccessModifier
}

and Field = {
    Symbol: ClassMemberSymbol
    Type: QualifiedType
    Name: string
    AccessModifier: AccessModifier
}

and Function = {
    Symbol: TopSymbol
    Signature : CodeSignature
    Code : Statement list
}


and Expression =
    | FunctionCall of Function * Expression list
    | MethodCall of Expression * Method * Expression list
    | Variable of Variable
    | Literal of Literal

and Statement =
    | AssignValue of Variable * Expression
    | InitializeVariable of Variable * Expression
    | Expression of Expression
    | Branches of Expression list * Statement list list
    | While of Expression * Statement list
    | For of Statement * Expression * Statement * Statement list
    | Return of Expression option



and Class = { 
    Symbol: TopSymbol
    InstanceFields: Field Set
    InstanceMethods: Method Set
    ClassFields: Field Set
    ClassMethods: Method Set
}





type UnresolvedClassReference = {
    Name: string
    Namespace: Namespace
    Methods: string list
}

type UnresolvedReference =
    | UnknownClass of UnresolvedClassReference

type ResolvedReference =
    | Function of Function
    | Type of Type
    | Method of Method

type IncompleteReference = {
    Required: UnresolvedReference list
    Supplier: (ResolvedReference -> int -> UnresolvedReference list * ResolvedReference option)
}
