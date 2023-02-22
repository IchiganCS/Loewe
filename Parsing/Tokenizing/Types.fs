module Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.CommonTypes



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
    ]




type Operator =
    | Equal
    | EqualNot
    | Not
    | And
    | Or
    | Plus
    | Minus
    | Multiply
    | Divide

let operators =
    Map [
        Equal, "=="
        EqualNot, "!="
        Not, "!"
        And, "&&"
        Or, "||"
        Plus, "+"
        Minus, "-"
        Multiply, "*"
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