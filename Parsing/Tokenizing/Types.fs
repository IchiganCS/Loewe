module Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.Types



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

let keywords = 
    Map [
        Public, "public"
        Private, "private"
        Const, "const"
        While, "while"
        For, "for"
        Return, "return"
        If, "if"
        ElseIf, "elif"
        Else, "else"
        Open, "open"
        Namespace, "namespace"
        Class, "class"
    ]







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
    | EqualSign
    | DoubleDot


let separators = 
    Map [
        BracketOpen, "("
        BracketClose, ")"
        CurvedBracketOpen, "{"
        CurvedBracketClose, "}"
        SharpBracketOpen, "["
        SharpBracketClose, "]"
        Semicolon, ";"
        Comma, ","
        Dot, "."
        EqualSign, "="
        DoubleDot, ":"
    ]




type Operator =
    | Equal
    | EqualNot
    | Not
    | And
    | Or
    | Plus
    | Minus
    | Star
    | Divide
    | Percent

let operators =
    Map [
        Percent, "%"
        Equal, "=="
        EqualNot, "!="
        Not, "!"
        And, "&&"
        Or, "||"
        Plus, "+"
        Minus, "-"
        Star, "*"
        Divide, "/"
    ]





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
    override this.ToString() =
        match this with
        | Keyword keyword -> keywords[keyword]
        | Identifier id -> id
        | Separator separator -> separators[separator]
        | Operator op -> operators[op]
        | Literal lit ->
            match lit with
            | Int i -> string i
            | UnsignedInt i -> string i
            | Long i -> string i
            | UnsignedLong i -> string i
            | Float i -> string i
            | Double i -> string i
            | String i -> string i
            | Bool i -> string i

type PositionedToken = {
    Token: Token
    Position: Position
}