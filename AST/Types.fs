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
}

and Field = {
    Symbol: ClassMemberSymbol
    Type: QualifiedType
}

and Function = {
    Symbol: TopSymbol
    Signature : CodeSignature
    Code : Statement list
}


and Expression =
    | Call of Expression * Method * Expression list
    | Variable of Variable

and Statement =
    | AssignValue of Expression * Expression
    | AssignVariable of Variable * Expression
    | Expression of Expression
    | Branch of Expression list * Statement list list
    | While of Expression * Statement list
    | For of Expression * Expression * Expression * Statement list



and Class = { 
    Symbol: TopSymbol
    InstanceFields: (AccessModifier * Variable) Set
    InstanceMethods: (AccessModifier * Method) Set
    ClassFields: (AccessModifier * Variable) Set
    ClassMethods: (AccessModifier * Method) Set
}