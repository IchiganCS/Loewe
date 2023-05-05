module Loewe.Parser.Lexer.TokenStringMapping

open TokenTypes

let keywordMappings: Map<Keyword, string> =
    Map
        [ Public, "public"
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
          Class, "class" ]

let separatorMappings: Map<Separator, string> =
    Map
        [ BracketOpen, "("
          BracketClose, ")"
          CurvedBracketOpen, "{"
          CurvedBracketClose, "}"
          SharpBracketOpen, "["
          SharpBracketClose, "]"
          Semicolon, ";"
          Comma, ","
          Dot, "."
          Assignment, "="
          DoubleDot, ":" ]

let operatorMappings: Map<Operator, string> =
    Map
        [ Percent, "%"
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
          DoubleOr, "||" ]



let tokenToString tok =
    match tok with
    | Keyword keyword -> keywordMappings[keyword]
    | Identifier id -> id
    | Separator separator -> separatorMappings[separator]
    | Operator op -> operatorMappings[op]
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
