module Loewe.Parsing.Types

type Position = {
    Line: int
    Coloumn: int
    Length: int
}

type Keyword =
    | Const
    | While
    | For
    | Public
    | Private
    | Return
    | If
    | Else
    | ElseIf

type Separator =
    | BracketOpen
    | BracketClose
    | CurvedBracketOpen
    | CurvedBracketClose
    | SharpBracketOpen
    | SharpBracketClose
    | Semicolon
    | Comma

type Operator = 
    | Assign
    | Equal
    | EqualNot
    | Not
    | And
    | Or
    | Plus
    | Minus
    | Multiply
    | Divide

type Literal =
    | Int of int
    | String of string
    | Bool of bool

type Token =
    | Keyword of Keyword
    | Name of string
    | Separator of Separator
    | Operator of Operator
    | Literal of Literal


type PositionedToken = {
    Token: Token
    Position: Position
}