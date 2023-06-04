module Loewe.Shared.CodeAnalysis.IR

open Position

/// Empty means global. A string prepended is the child of the tail
type Module = string list


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

type Keyword =
    | Const
    | While
    | For
    | Public
    | Private
    | Return
    | Module
    | Use
    | If
    | Else
    | ElseIf
    | Class

type Separator =
    | BracketOpen
    | BracketClose
    | CurvedBracketOpen
    | CurvedBracketClose
    | SharpBracketOpen
    | SharpBracketClose
    | Semicolon
    | Comma
    | Dot
    | Assignment
    | DoubleDot
    | NewLine


type Operator =
    | EqualSign
    | NotEqualSign
    | NotSign
    | TildeSign
    | AndSign
    | OrSign
    | Plus
    | Minus
    | Star
    | Slash
    | Percent
    | DoubleAnd
    | DoubleOr


type Token =
    | Keyword of Keyword
    | Identifier of string
    | Separator of Separator
    | Operator of Operator
    | Literal of Literal
type TokenType =
    | KeywordType
    | IdentifierType
    | SeparatorType
    | OperatorType
    | LiteralType



type PositionedToken = Position * Token
type WithToken<'a> = 'a * PositionedToken
type WithTokens<'a> = 'a * PositionedToken list


type TypedParametersData<'t> = {
    BracketOpen: PositionedToken
    BracketClose: PositionedToken
    Members: ('t WithTokens * string WithToken) list
    Tokens: PositionedToken list
}

type LiteralData = {
    Literal: Literal
    Token: PositionedToken
}

type VariableData = {
    Identifier: string
    Token: PositionedToken
}

type TypedVariableData<'t> = {
    Identifier: string WithToken
    Type: 't WithTokens

    Tokens: PositionedToken list
}



type AttributeData<'f, 'a, 'm> = {
    Value: Expression<'f, 'a, 'm>
    DotToken: PositionedToken
    Attribute: 'a WithToken
    Tokens: PositionedToken list
}


and BinaryOperationData<'f, 'a, 'm> = {
    Left: Expression<'f, 'a, 'm>
    Operator: BinaryOperation WithToken
    Right: Expression<'f, 'a, 'm>
    Tokens: PositionedToken list
}

and UnaryOperationData<'f, 'a, 'm> = {
    Operand: Expression<'f, 'a, 'm>
    Operator: UnaryOperation WithToken
    Tokens: PositionedToken list
}

and BracketedData<'f, 'a, 'm> = {
    LeftBracket: PositionedToken
    Value: Expression<'f, 'a, 'm>
    RightBracket: PositionedToken
    Tokens: PositionedToken list
}

and FunctionCallData<'f, 'a, 'm> = {
    Identifier: 'f
    LeftBracket: PositionedToken
    /// The second tupled argument always points to the token after the expression, mostly
    Parameters: (Expression<'f, 'a, 'm> * PositionedToken) list
    Tokens: PositionedToken list
}

and MethodCallData<'f, 'a, 'm> = {
    Identifier: 'm
    Value: Expression<'f, 'a, 'm>
    LeftBracket: PositionedToken
    /// The second tupled argument always points to the token after the expression, mostly
    Parameters: (Expression<'f, 'a, 'm> * PositionedToken) list
    Tokens: PositionedToken list
}


/// This union is highly customizable in order to save repetition. Its four generic type parameters symoblize how
/// different symbols should be encoded. It is differetiated into function, attribute, method, variable, typed variable. For the first
/// step of parsing, probably strings are good enough. Later these can be full fledged references.
and Expression<'f, 'a, 'm> =
    | BinaryOperationExpr of BinaryOperationData<'f, 'a, 'm>
    | UnaryOperationExpr of UnaryOperationData<'f, 'a, 'm>
    | LiteralExpr of LiteralData
    | VariableExpr of VariableData
    | AttributeExpr of AttributeData<'f, 'a, 'm>
    | BracketedExpr of BracketedData<'f, 'a, 'm>
    | FunctionCallExpr of FunctionCallData<'f, 'a, 'm>
    | MethodCallExpr of MethodCallData<'f, 'a, 'm>


type KnownAssignmentData<'f, 'a, 'm, 'na> = {
    Left: Expression<'f, 'a, 'm>
    AssignemntSign: PositionedToken
    Right: Expression<'f, 'a, 'm>
    Tokens: PositionedToken list
}

type NewAssignmentData<'f, 'a, 'm, 't> = {
    Left: TypedVariableData<'t>
    AssignmentSign: PositionedToken
    Right: Expression<'f, 'a, 'm>
    Tokens: PositionedToken list
}

type ReturnStatData<'f, 'a, 'm, 'na> = {
    ReturnWord: PositionedToken
    Condition: Expression<'f, 'a, 'm>
    Tokens: PositionedToken list
}


type BlockData<'f, 'a, 'm, 'na> = {
    BracketOpen: PositionedToken
    BracketClose: PositionedToken
    Statements: Statement<'f, 'a, 'm, 'na> list
    Tokens: PositionedToken list
}

and WhileStatData<'f, 'a, 'm, 'na> = {
    WhileWord: PositionedToken
    Condition: Expression<'f, 'a, 'm>
    CodeBlock: BlockData<'f, 'a, 'm, 'na>
    Tokens: PositionedToken list
}


and Statement<'f, 'a, 'm, 'na> =
    | KnownAssignmentStat of KnownAssignmentData<'f, 'a, 'm, 'na>
    | NewAssignmentStat of NewAssignmentData<'f, 'a, 'm, 'na>
    | ExpressionStat of Expression<'f, 'a, 'm>
    | WhileStat of WhileStatData<'f, 'a, 'm, 'na>
    | ReturnStat of ReturnStatData<'f, 'a, 'm, 'na>


/// Classes are stumps to avoid mutual recursion.
type Class = { Module: Module; Name: string }


type AttributeData<'t> = {
    AccessModifier: AccessModifier WithToken
    Name: string WithToken
    Type: 't WithTokens
    Tokens: PositionedToken list
    Owner: Class
}

type MethodSignatureData<'t> = {
    AccessModifier: AccessModifier WithToken
    Name: string WithToken
    Parameters: TypedParametersData<'t>
    Return: 't WithTokens
    Tokens: PositionedToken list
    Owner: Class
}

type FunctionSignatureData<'t> = {
    Name: string WithToken
    Parameters: TypedParametersData<'t>
    Return: 't WithTokens
    Tokens: PositionedToken list
    Module: Module
}


type Type =
    | PrimitiveType of Primitive
    | ClassType of Class






type UnknownIdentifier = string * Module option
type UnknownTypedVariable = UnknownIdentifier * string
type UnresolvedCode = BlockData<UnknownIdentifier, string, string, UnknownTypedVariable>


type UnresolvedMethod = MethodSignatureData<UnknownIdentifier> * UnresolvedCode
type UnresolvedFunction = FunctionSignatureData<UnknownIdentifier> * UnresolvedCode
type UnresolvedAttribute = AttributeData<UnknownIdentifier>

type PartialResolvedMethod = MethodSignatureData<Type> * UnresolvedCode
type PartialResolvedFunction = FunctionSignatureData<Type> * UnresolvedCode

type ResolvedExpression = Expression<FunctionSignatureData<Type>, AttributeData<Type>, MethodSignatureData<Type>>

type ResolvedCode = BlockData<FunctionSignatureData<Type>, AttributeData<Type>, MethodSignatureData<Type>, VariableData>

type ResolvedAttribute = AttributeData<Type>
type ResolvedMethod = MethodSignatureData<Type> * ResolvedCode
type ResolvedFunction = FunctionSignatureData<Type> * ResolvedCode



type GlobalSymbol<'type_, 'code> =
    | AttributeSymbol of AttributeData<'type_>
    | FunctionSymbol of FunctionSignatureData<'type_> * 'code
    | MethodSymbol of MethodSignatureData<'type_> * 'code
    | ClassSymbol of Class

type UnresolvedSymbol = GlobalSymbol<UnknownIdentifier, UnresolvedCode>
type PartialResolvedSymbol = GlobalSymbol<Type, UnresolvedCode>
type ResolvedSymbol = GlobalSymbol<Type, ResolvedCode>
