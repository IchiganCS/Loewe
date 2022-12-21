module Loewe.AST.Types

type TypeQualifier =
    | Owner
    | Ref
    | ConstRef
    | Ptr
    | ConstPtr

and AccessModifier =
    | Private
    | Public

and QualifiedType = { 
    Type: Type
    Qualifier: TypeQualifier 
}

and Primitive = {
    Name: string
    TranslatedName: string
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

and Namespace =
    | Root
    | Child of Namespace * string
    
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
    | FunctionCall of TopSymbol * Expression list
    | Variable of Variable
    | Constant of string

and Statement =
    | AssignValue of Expression * Expression
    | InitializeVariable of Variable * Expression
    | Expression of Expression
    | Branches of Expression list * Statement list list
    | While of Expression * Statement list
    | For of Expression * Expression * Expression * Statement list
    | Return of Expression option



and Class = { 
    Symbol: TopSymbol
    InstanceFields: Field Set
    InstanceMethods: Method Set
    ClassFields: Field Set
    ClassMethods: Method Set
}