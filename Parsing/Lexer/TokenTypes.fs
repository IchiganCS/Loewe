module Loewe.Parser.Lexer.TokenTypes

open Loewe.Parser.Types

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


type Literal =
    | Int of int32
    | UnsignedInt of uint32
    | Long of int64
    | UnsignedLong of uint64
    | Float of float
    | Double of double
    | String of string
    | Bool of bool

type Token =
    | Keyword of Keyword
    | Identifier of string
    | Separator of Separator
    | Operator of Operator
    | Literal of Literal


type PositionedToken = { Token: Token; Position: Position }