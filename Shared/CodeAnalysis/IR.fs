module Loewe.Shared.CodeAnalysis.IR

/// Empty means global. A string prepended is the child of the tail
type Namespace = string list

type AccessModifier =
    | Private
    | Public

type UnaryOperation =
    | Not
    | BitwiseNot
    | Negate

type BinaryOperation =
    | Addition
    | Subtraction
    | Multiplication
    | Division
    | Modulo
    | And
    | Or
    | Equal
    | NotEqual
    | BitwiseAnd
    | BitwiseOr

type Primitive =
    | IntType
    | LongType
    | FloatType
    | DoubleType
    | UnsignedIntType
    | UnsignedLongType
    | CharType
    | VoidType
    | BoolType
    | StringTpye

type Literal =
    | Int of int32
    | UnsignedInt of uint32
    | Long of int64
    | UnsignedLong of uint64
    | Float of float32
    | Double of double
    | String of string
    | Bool of bool
    | Char of char

/// This union is highly customizable in order to save repetition. Its four generic type parameters symoblize how
/// different symbols should be encoded. It is differetiated into function, attribute, method, variable, typed variable. For the first
/// step of parsing, probably strings are good enough. Later these can be full fledged references.
type Expression<'f, 'a, 'm, 'v> =
    | BinaryOperation of Expression<'f, 'a, 'm, 'v> * BinaryOperation * Expression<'f, 'a, 'm, 'v>
    | UnaryOperation of UnaryOperation * Expression<'f, 'a, 'm, 'v>
    | Literal of Literal
    | Variable of 'v
    | Attribute of Expression<'f, 'a, 'm, 'v> * 'a
    | Bracketed of Expression<'f, 'a, 'm, 'v>
    | FunctionCall of 'f * Expression<'f, 'a, 'm, 'v> list
    | MethodCall of Expression<'f, 'a, 'm, 'v> * 'm * Expression<'f, 'a, 'm, 'v> list

type Statement<'f, 'a, 'm, 'v, 'na> =
    | KnownAssignment of Expression<'f, 'a, 'm, 'v> * Expression<'f, 'a, 'm, 'v>
    | NewAssignment of 'na * Expression<'f, 'a, 'm, 'v>
    | If of Expression<'f, 'a, 'm, 'v> * Statement<'f, 'a, 'm, 'v, 'na> list
    | IfElse of Expression<'f, 'a, 'm, 'v> * Statement<'f, 'a, 'm, 'v, 'na> list * Statement<'f, 'a, 'm, 'v, 'na> list
    | Expression of Expression<'f, 'a, 'm, 'v>
    | While of Expression<'f, 'a, 'm, 'v> * Statement<'f, 'a, 'm, 'v, 'na> list
    | Return of Expression<'f, 'a, 'm, 'v>


type UnknownIdentifier = string * Namespace option
type UnknownTypedVariable = UnknownIdentifier * string
type UnknownVariable = string
type UnresolvedStatement = Statement<UnknownIdentifier, string, string, UnknownVariable, UnknownTypedVariable>
type UnresolvedCode = UnresolvedStatement list

/// Classes are stumps to avoid mutual recursion.
type Class = { Namespace: Namespace; Name: string }

type Type =
    | PrimitiveType of Primitive
    | ClassType of Class


type Attribute<'t> = {
    AccessModifier: AccessModifier
    Name: string
    Type: 't
    Owner: Class
}

type MethodSignature<'t> = {
    AccessModifier: AccessModifier
    Name: string
    Parameters: ('t * string) list
    Return: 't
    Owner: Class
}

type Variable<'t> = { Name: string; Type: 't }

type FunctionSignature<'t> = {
    Namespace: Namespace
    Name: string
    Parameters: ('t * string) list
    Return: 't
}

type UnresolvedMethod = MethodSignature<UnknownIdentifier> * UnresolvedCode
type UnresolvedFunction = FunctionSignature<UnknownIdentifier> * UnresolvedCode
type UnresolvedAttribute = Attribute<UnknownIdentifier>

type PartialResolvedMethod = MethodSignature<Type> * UnresolvedCode
type PartialResolvedFunction = FunctionSignature<Type> * UnresolvedCode

type ResolvedExpression = Expression<FunctionSignature<Type>, Attribute<Type>, MethodSignature<Type>, Variable<Type>> 
type ResolvedStatement = Statement<FunctionSignature<Type>, Attribute<Type>, MethodSignature<Type>, Variable<Type>, Variable<Type>>
type ResolvedCode = ResolvedStatement list

type ResolvedAttribute = Attribute<Type>
type ResolvedMethod = MethodSignature<Type> * ResolvedCode
type ResolvedFunction = FunctionSignature<Type> * ResolvedCode



type GlobalSymbol<'type_, 'code> =
    | AttributeSymbol of Attribute<'type_>
    | FunctionSymbol of FunctionSignature<'type_> * 'code
    | MethodSymbol of MethodSignature<'type_> * 'code
    | ClassSymbol of Class

type UnresolvedSymbol = GlobalSymbol<UnknownIdentifier, UnresolvedCode>
type PartialResolvedSymbol = GlobalSymbol<Type, UnresolvedCode>
type ResolvedSymbol = GlobalSymbol<Type, ResolvedCode>