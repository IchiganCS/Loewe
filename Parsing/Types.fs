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

/// returns the operator with the highest precedence - if equal, binOp1 is returned
let checkOperatorPrecedence binOp1 binOp2 =
    match binOp1 with
    | Addition | Subtraction -> 
        match binOp2 with
        | Addition | Subtraction -> binOp1
        | _ -> binOp2
    | Multiplication | Division | Modulo -> binOp1
    | Equal | NotEqual | And -> binOp1
    | Or -> 
        match binOp2 with
        | Or -> binOp1
        | _ -> binOp2

type UnaryOperation =
    | Not
    | Negate

type Position = {
    Line: int
    Coloumn: int
    Length: int
}