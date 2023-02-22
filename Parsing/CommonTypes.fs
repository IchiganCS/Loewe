module Loewe.Parsing.CommonTypes

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

type Position = {
    Line: int
    Coloumn: int
    Length: int
}