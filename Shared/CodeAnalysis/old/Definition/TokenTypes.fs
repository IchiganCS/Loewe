module Loewe.Definition.TokenTypes

type Keyword =
    | Const
    | While
    | For
    | Public
    | Private
    | Return
    | Namespace
    | Open
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
    | Divide
    | Percent
    | DoubleAnd
    | DoubleOr


type Token =
    | Keyword of Keyword
    | Identifier of string
    | Separator of Separator
    | Operator of Operator
    | Literal of Literal