module Loewe.Parsing.Helpers
open System.Text.RegularExpressions
open Loewe.Parsing.Types

/// How many rows to skip (first component) and to which column to skip (second component)
/// to not be at a comment. Third is the remaining substring
let rec skipComment (str: string) : (int * int * string) =
    if str.StartsWith "//" then
        let nextLineBreak = str.IndexOf('\n')
        if nextLineBreak = -1 then 
            (0, str.Length, "")
        else
            let (skippedRows, targetColumn, substring) = skipComment (str.Substring (nextLineBreak + 1))
            (1 + skippedRows, targetColumn, substring)
    elif str.StartsWith "/*" then
        let mutable skippedLines = 0
        let mutable endFound = false
        let mutable i = 2
        let mutable col = 0
        while not endFound && str.Length > i do
            if str[i] = '\n' then
                skippedLines <- skippedLines + 1
                col <- 0
            elif str[i] = '/' && str[i - 1] = '*' then
                endFound <- true
            i <- i + 1
            col <- col + 1
        if not endFound then
            (0, 0, str)
        else
            let (skippedRows, targetColumn, substring) = skipComment (str.Substring (i))
            (skippedLines + skippedRows, col + targetColumn, substring)
    else 
        (0, 0, str)

let keywords = 
    Map [
        (Public, "public")
        (Private, "private")
        (Const, "const")
        (While, "while")
        (For, "for")
        (Return, "return")
        (If, "if")
        (ElseIf, "elif")
        (Else, "else")
    ]

let keywordKeys =
    keywords.Keys
    |> Set.ofSeq



let separators = 
    Map [
        (BracketOpen, "(")
        (BracketClose, ")")
        (CurvedBracketOpen, "{")
        (CurvedBracketClose, "}")
        (SharpBracketOpen, "[")
        (SharpBracketClose, "]")
        (Semicolon, ";")
        (Comma, ",")
    ]

let separatorKeys =
    separators.Keys
    |> Set.ofSeq

let operators =
    Map [
        (Assign, "=")
        (Equal, "==")
        (EqualNot, "!=")
        (Not, "!")
        (And, "&&")
        (Or, "||")
        (Plus, "+")
        (Minus, "-")
        (Multiply, "*")
        (Divide, "/")
    ]

let operatorKeys =
    operators.Keys
    |> Set.ofSeq

let nameRegex = Regex ("[a-z|A-Z|_][a-z|A-Z|\d|_]*", RegexOptions.Compiled ||| RegexOptions.NonBacktracking)


let parseSeparator (str: string) : (Token * string) option =
    let hits =
        separatorKeys
        |> Set.filter (fun key -> str.StartsWith (separators[key]))
    
    if hits.Count = 1 then
        Some (Separator hits.MaximumElement, separators[hits.MaximumElement])
    else
        None

let parseOperator (str: string) : (Token * string) option =
    let hits =
        operatorKeys
        |> Set.filter (fun key -> str.StartsWith (operators[key]))
    
    
    if hits.Count > 0 then
        let greatestHit =
            hits
            |> Set.toList
            |> List.maxBy (fun key -> operators[key].Length)

        Some (Operator greatestHit, operators[greatestHit])
    else
        None

let parseName str : (Token * string) option =
    let mat = nameRegex.Match str
    if mat.Success && mat.Index = 0 then
        Some (Name mat.Value, mat.Value)
    else
        None
    
let parseKeyword (str: string) : (Token * string) option =
    let hits =
        keywordKeys
        |> Set.filter (fun key -> str.StartsWith (keywords[key]) || str.StartsWith (keywords[key]))

    if hits.Count = 1 then
        let hitWord = keywords[hits.MaximumElement]
        if str.Length = hitWord.Length || str[hitWord.Length] = ' ' || str[hitWord.Length] = '\n' || (parseSeparator (str[hitWord.Length].ToString ()) <> None) then
            Some (Keyword hits.MaximumElement, keywords[hits.MaximumElement])
        else
            None
    else
        None


let parseStringLiteral (str: string) : (Token * string) option =
    // this seems to be a little hard to do with Regex
    if str.Length < 2 || str[0] <> '\"' then
        None
    else

    let mutable res = ""
    let add (char: char) = res <- res + char.ToString ()

    let mutable i = 1
    let mutable hasBackslash = false
    let mutable reachedEnd = false
    while str.Length > i && not reachedEnd do
        if str[i] = '\"' then
            reachedEnd <- true
        elif hasBackslash then
            match str[i] with
            | 'n' -> add '\n'
            | 't' -> add '\t'
            | '\\' -> add '\\'
            | _ -> ()
            hasBackslash <- false
        elif str[i] = '\\' then
            hasBackslash <- true
        else
            add str[i]

        i <- i + 1
    
    if i >= str.Length || not reachedEnd then
        // we left the loop because there are no more characters, not because we reached the end
        None
    else
        Some (Literal (String res), str.Substring (0, i))
  
let parseBoolLiteral (str: string) : (Token * string) option =
    if str.StartsWith "true " || str.StartsWith "true\n" then
        Some (Literal (Bool true), "true")
    elif str.StartsWith "false " || str.StartsWith "false\n" then
        Some (Literal (Bool false), "false")
    else
        None
        
let parseLiteral str : (Token * string) option =
    match parseStringLiteral str with
    | Some res -> Some res
    | None ->

    match parseBoolLiteral str with
    | Some res -> Some res
    | None ->

    // add other literals
    None
