module Loewe.Parser.Types

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
let checkOperatorPrecedence binOp1 binOp2 =
    match binOp1 with
    | Multiplication
    | Division
    | Modulo
    | Equal
    | NotEqual
    | And
    | BitwiseAnd -> binOp1
    | Addition
    | Subtraction
    | Or
    | BitwiseOr ->
        match binOp2 with
        | Addition
        | Subtraction
        | Or
        | BitwiseOr -> binOp1
        | _ -> binOp2

type Position =
    { Line: int; Coloumn: int; Length: int }
