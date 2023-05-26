module Loewe.Shared.CodeAnalysis.Token

open IR

type Keyword =
    | Const
    | While
    | For
    | Public
    | Private
    | Return
    | Module
    | Use
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
    | NewLine


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
    | Slash
    | Percent
    | DoubleAnd
    | DoubleOr


type Token =
    | Keyword of Keyword
    | Identifier of string
    | Separator of Separator
    | Operator of Operator
    | Literal of Literal



[<RequireQualifiedAccess>]
module Tokens =
    let operatorMap: Map<string, Operator> =
        Map [
            "==", EqualSign
            "!=", NotEqualSign
            "!", NotSign
            "~", TildeSign
            "&", AndSign
            "|", OrSign
            "+", Plus
            "-", Minus
            "*", Star
            "/", Slash
            "%", Percent
            "&&", DoubleAnd
            "||", DoubleOr
        ]


    let separatorMap: Map<string, Separator> =
        Map [
            "(", BracketOpen
            ")", BracketClose
            "{", CurvedBracketOpen
            "}", CurvedBracketClose
            "[", SharpBracketOpen
            "]", SharpBracketClose
            ";", Semicolon
            ",", Comma
            ".", Dot
            "=", Assignment
            ":", DoubleDot
            "\n", NewLine
            "\r\n", NewLine
        ]

    let KeywordMap: Map<string, Keyword> =
        Map [
            "const", Const
            "while", While
            "for", For
            "public", Public
            "private", Private
            "return", Return
            "module", Module
            "use", Use
            "if", If
            "else", Else
            "elif", ElseIf
            "class", Class
        ]

    let stringOperator op =
        operatorMap
        |> Map.findKey (fun _ assocOp -> assocOp = op)

    let stringSeparator sep =
        separatorMap
        |> Map.findKey (fun _ assocSep -> assocSep = sep)

    let stringKeyword keyword =
        KeywordMap
        |> Map.findKey (fun _ assocKey -> assocKey = keyword)

    let stringLiteral literal =
        match literal with
        | Int x -> string x
        | UnsignedInt x -> string x
        | Long x -> string x
        | UnsignedLong x -> string x
        | Float x -> string x
        | Double x -> string x
        | String x -> x
        | Char x -> string x
        | Bool x -> string x

    let string tok =
        match tok with
        | Literal l -> stringLiteral l
        | Separator sep -> stringSeparator sep
        | Operator op -> stringOperator op
        | Keyword key -> stringKeyword key
        | Identifier id -> sprintf "Ident: %s" id