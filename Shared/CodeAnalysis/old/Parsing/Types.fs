module Loewe.Parsing.Types

type AccessModifier =
    | Public
    | Private

type Namespace =
    | Global
    | Child of Namespace * string

type TypeQualifier =
    | Owner
    | MutRef
    | ConstRef
    | MutPtr
    | ConstPtr



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

/// returns the operator with the highest precedence - if equal, binOp1 is returned

type Position =
    { Line: int; Coloumn: int; Length: int }
