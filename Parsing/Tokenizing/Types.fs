namespace rec Loewe.Parsing.Tokenizing


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
    override this.ToString() =
        match this with
        | Keyword keyword -> Mappings.keywords[keyword]
        | Identifier id -> id
        | Separator separator -> Mappings.separators[separator]
        | Operator op -> Mappings.operators[op]
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


module Mappings =
    let keywords : Map<Keyword, string> = 
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

    let separators : Map<Separator, string> = 
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
            Assignment, "="
            DoubleDot, ":"
        ]
            
    let operators : Map<Operator, string> =
        Map [
            Percent, "%"
            EqualSign, "=="
            NotEqualSign, "!="
            NotSign, "!"
            AndSign, "&"
            OrSign, "|"
            Plus, "+"
            Minus, "-"
            Star, "*"
            Divide, "/"
            DoubleAnd, "&&"
            TildeSign, "~"
            DoubleOr, "||"
        ]

