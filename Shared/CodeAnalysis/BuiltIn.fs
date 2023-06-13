module Loewe.Shared.CodeAnalysis.BuiltIn


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
    | StringType