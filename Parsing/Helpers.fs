module Loewe.Tokenizing.Helpers
open System.Text.RegularExpressions
open Loewe.Tokenizing.Types

/// Return rest of string
let tokenizeComment (str: string) : string option =
    if str.StartsWith "//" then
        // check for ending of line
        let nextLineBreak = str.IndexOf '\n'
        if nextLineBreak = -1 then 
            // we are in the last line and it is commented out - no recursion necessary
            Some str
        else
            Some (str.Substring (0, nextLineBreak + 1))

    else 
        None


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

let nameRegex = Regex ("^[a-z|A-Z|_][a-z|A-Z|\d|_]*", RegexOptions.Compiled ||| RegexOptions.NonBacktracking)


let tokenizeSeparator (str: string) : (Token * string) option =
    let hits =
        separatorKeys
        |> Set.filter (fun key -> str.StartsWith (separators[key]))
    
    if hits.Count = 1 then
        Some (Separator hits.MaximumElement, separators[hits.MaximumElement])
    else
        None

let isSeperatorOrSpace (char: char) : bool =
    char = ' ' 
    || char = '\n' 
    || char = '\r' 
    || (tokenizeSeparator (char.ToString ()) <> None) 

let tokenizeOperator (str: string) : (Token * string) option =
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

let tokenizeName str : (Token * string) option =
    let mat = nameRegex.Match str
    if mat.Success then
        Some (Name mat.Value, mat.Value)
    else
        None
    
let tokenizeKeyword (str: string) : (Token * string) option =
    let hits =
        keywordKeys
        |> Set.filter (fun key -> str.StartsWith (keywords[key]) || str.StartsWith (keywords[key]))

    if hits.Count = 1 then
        let hitWord = keywords[hits.MaximumElement]
        if str.Length = hitWord.Length || (isSeperatorOrSpace str[hitWord.Length]) then
            Some (Keyword hits.MaximumElement, keywords[hits.MaximumElement])
        else
            None
    else
        None


let tokenizeStringLiteral (str: string) : (Token * string) option =
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
    
    if i > str.Length || not reachedEnd then
        // we left the loop because there are no more characters, not because we reached the end
        None
    else
        Some (Literal (Literal.String res), str.Substring (0, i))
  
let tokenizeBoolLiteral (str: string) : (Token * string) option =
    if str.StartsWith "true " || str.StartsWith "true\n" then
        Some (Literal (Bool true), "true")
    elif str.StartsWith "false " || str.StartsWith "false\n" then
        Some (Literal (Bool false), "false")
    else
        None


let numbers = "0123456789ABCDEF"

let numberRegex = Regex ("^(?<sign>\+|-)?(?<sig>0x|0o|0b)?(?<numbersBeforeDot>[0123456789]*)(?<dot>\.?)(?<numbersAfterDot>[0123456789ABCDEF]+)(?<end>ul|u|l|f|d)?", RegexOptions.Compiled ||| RegexOptions.NonBacktracking)

let tokenizeNumberLiteral (str: string) : (Token * string) option =
    let mat = numberRegex.Match str
    
    
    if not mat.Success then
        None
    elif mat.ValueSpan.Length < str.Length && not (isSeperatorOrSpace str[mat.ValueSpan.Length]) then
        None
    else

    let sign = mat.Groups[1].Value
    let signMul =
        if sign = "-" then
            -1
        else
            1
    let numberBase = mat.Groups[2].Value
    let numberBaseVal =
        if numberBase = "0x" then
            16
        elif numberBase = "0o" then
            8
        elif numberBase = "0b" then
            2
        else 
            10
    let beforeDot = mat.Groups[3].Value
    let dot = mat.Groups[4].Value
    let afterDot = mat.Groups[5].Value
    let numberType = mat.Groups[6].Value

    if numberBase <> "" && (dot <> "" || numberType = "f" || numberType = "d") then
        None        
    elif signMul = -1 && (numberType = "u" || numberType = "ul") then
        None
    elif dot <> "" && not (numberType = "d" || numberType = "f" || numberType = "") then
        None
    else 

    // check valid inputs (before dot is only filled if float or double and it only matches to decimal digits)
    let validNumbers = numbers.Substring (0, numberBaseVal)
    if not (String.forall (fun ch -> validNumbers.Contains ch) afterDot) then
        None
    else

    let dottedString = beforeDot + "." + afterDot
    if numberType = "f" || (numberType = "" && dot <> "")then
        Some (Literal (Float ((float signMul) * float (System.Convert.ToDouble dottedString))), mat.Value)
    elif numberType = "d" then
        Some (Literal (Double ((double signMul) * System.Convert.ToDouble dottedString)), mat.Value)
    elif numberType = "" then
        Some (Literal (Int (signMul * System.Convert.ToInt32 afterDot)), mat.Value)
    elif numberType = "u" then
        Some (Literal (UnsignedInt (System.Convert.ToUInt32 afterDot)), mat.Value)
    elif numberType = "ul" then
        Some (Literal (UnsignedLong (System.Convert.ToUInt64 afterDot)), mat.Value)
    elif numberType = "l" then
        Some (Literal (Long ((int64 signMul) * System.Convert.ToInt64 afterDot)), mat.Value)
    else
        // shouldn't be reached
        None

    



    
        
let tokenizeLiteral str : (Token * string) option =
    match tokenizeStringLiteral str with
    | Some res -> Some res
    | None ->

    match tokenizeBoolLiteral str with
    | Some res -> Some res
    | None ->

    match tokenizeNumberLiteral str with
    | Some res -> Some res
    | None ->
    
    None
