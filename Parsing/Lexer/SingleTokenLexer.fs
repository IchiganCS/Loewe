module Loewe.Parser.Lexer.SingleTokenLexer

open System.Text.RegularExpressions
open Loewe.Parser.Lexer.TokenStringMapping
open Loewe.Parser.Lexer.TokenTypes


let private keywordKeys = keywordMappings.Keys |> Set.ofSeq

let private separatorKeys = separatorMappings.Keys |> Set.ofSeq

let private operatorKeys = operatorMappings.Keys |> Set.ofSeq

let private nameRegex =
    Regex ("^[a-z|A-Z|_][a-z|A-Z|\d|_]*", RegexOptions.Compiled ||| RegexOptions.NonBacktracking)

let private digits = "0123456789ABCDEF"

let private numberRegex =
    Regex (
        "^(?<sign>\+|-)?(?<sig>0x|0o|0b)?(?<numbersBeforeDot>[0123456789ABCDEF]*)(?<dot>\.?)(?<numbersAfterDot>[0123456789ABCDEF]+)(?<end>ul|u|l|f|d)?",
        RegexOptions.Compiled ||| RegexOptions.NonBacktracking
    )


let comment (strRef: string ref) : int option =
    let str = strRef.Value

    if not (str.StartsWith "//") then
        None
    else
        // check for ending of line
        let nextLineBreak = str.IndexOf '\n'

        if nextLineBreak = -1 then
            // we are in the last line and it is commented out - no recursion necessary
            Some str.Length
        else
            Some (nextLineBreak + 1)

let separator (strRef: string ref) : (Token * int) option =
    let str = strRef.Value

    let hits =
        separatorKeys |> Set.filter (fun key -> str.StartsWith (separatorMappings[key]))

    if hits.Count = 1 then
        Some (Separator hits.MaximumElement, separatorMappings[hits.MaximumElement].Length)
    else
        None

let private isSeperatorOrSpace (char: char) : bool =
    char = ' '
    || char = '\n'
    || char = '\r'
    || (separator (ref (Operators.string char)) <> None)

let operator (strRef: string ref) : (Token * int) option =
    let str = strRef.Value

    let hits =
        operatorKeys |> Set.filter (fun key -> str.StartsWith (operatorMappings[key]))


    if hits.Count > 0 then
        let greatestHit =
            hits |> Set.toList |> List.maxBy (fun key -> operatorMappings[key].Length)

        Some (Operator greatestHit, operatorMappings[greatestHit].Length)
    else
        None

let identifier (strRef: string ref) : (Token * int) option =
    let str = strRef.Value
    let mat = nameRegex.Match str

    if mat.Success then
        Some (Identifier mat.Value, mat.ValueSpan.Length)
    else
        None

let keyword (strRef: string ref) : (Token * int) option =
    let str = strRef.Value

    let hits =
        keywordKeys
        |> Set.filter (fun key -> str.StartsWith (keywordMappings[key]) || str.StartsWith (keywordMappings[key]))

    if hits.Count = 1 then
        let hitWord = keywordMappings[hits.MaximumElement]

        if str.Length = hitWord.Length || (isSeperatorOrSpace str[hitWord.Length]) then
            Some (Keyword hits.MaximumElement, keywordMappings[hits.MaximumElement].Length)
        else
            None
    else
        None

let stringLiteral (strRef: string ref) : (Token * int) option =
    let str = strRef.Value
    // this seems to be a little hard to do with Regex
    if str.Length < 2 || str[0] <> '\"' then
        None
    else

        let mutable res = ""
        let add (char: char) = res <- res + Operators.string char

        let mutable i = 1
        let mutable hasBackslash = false
        let mutable reachedEnd = false

        while str.Length > i && not reachedEnd do
            if str[i] = '\"' && not hasBackslash then
                reachedEnd <- true
            elif hasBackslash then
                match str[i] with
                | 'n' -> add '\n'
                | 't' -> add '\t'
                | '\\' -> add '\\'
                | '\"' -> add '\"'
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
            Some (Literal (Literal.String res), i)

let boolLiteral (strRef: string ref) : (Token * int) option =
    let str = strRef.Value

    if str.StartsWith "true " || str.StartsWith "true\n" then
        Some (Literal (Bool true), "true".Length)
    elif str.StartsWith "false " || str.StartsWith "false\n" then
        Some (Literal (Bool false), "false".Length)
    else
        None

let numberLiteral (strRef: string ref) : (Token * int) option =
    let str = strRef.Value
    let mat = numberRegex.Match str


    if not mat.Success then
        None
    elif
        mat.ValueSpan.Length < str.Length
        && not (isSeperatorOrSpace str[mat.ValueSpan.Length])
    then
        None
    else

        let sign = mat.Groups[1].Value
        let signMul = if sign = "-" then -1 else 1
        let numberBase = mat.Groups[2].Value

        let numberBaseVal =
            if numberBase = "0x" then 16
            elif numberBase = "0o" then 8
            elif numberBase = "0b" then 2
            else 10

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
            let validNumbers = digits.Substring (0, numberBaseVal)
            let concatString = beforeDot + afterDot

            if not (String.forall (fun ch -> validNumbers.Contains ch) concatString) then
                None
            else


                let dottedString = beforeDot + "." + afterDot

                if numberType = "f" || (numberType = "" && dot <> "") then
                    Some (Literal (Float ((float signMul) * float dottedString)), mat.ValueSpan.Length)
                elif numberType = "d" then
                    Some (Literal (Double ((double signMul) * double dottedString)), mat.ValueSpan.Length)
                elif numberType = "" then
                    Some (
                        Literal (Int (signMul * System.Convert.ToInt32 (concatString, numberBaseVal))),
                        mat.ValueSpan.Length
                    )
                elif numberType = "u" then
                    Some (
                        Literal (UnsignedInt (System.Convert.ToUInt32 (concatString, numberBaseVal))),
                        mat.ValueSpan.Length
                    )
                elif numberType = "ul" then
                    Some (
                        Literal (UnsignedLong (System.Convert.ToUInt64 (concatString, numberBaseVal))),
                        mat.ValueSpan.Length
                    )
                elif numberType = "l" then
                    Some (
                        Literal (Long ((int64 signMul) * System.Convert.ToInt64 (concatString, numberBaseVal))),
                        mat.ValueSpan.Length
                    )
                else
                    // shouldn't be reached
                    None

let anyLiteral (strRef: string ref) : (Token * int) option =
    match stringLiteral strRef with
    | Some res -> Some res
    | None ->

        match boolLiteral strRef with
        | Some res -> Some res
        | None ->

            match numberLiteral strRef with
            | Some res -> Some res
            | None ->

                None



/// Starts at the beginning of the given string and tries to match any kind of token.
/// If a match could be made, returns the parsed token and the number of chars responsible for the match.
/// None if no match could be made.
let any (str: string ref) : (Token option * int) option =
    match comment str with
    | Some len -> Some (None, len)
    | None ->

        match keyword str with
        | Some (tok, len) -> Some (Some tok, len)
        | None ->

            match anyLiteral str with
            | Some (tok, len) -> Some (Some tok, len)
            | None ->

                match separator str with
                | Some (tok, len) -> Some (Some tok, len)
                | None ->

                    match operator str with
                    | Some (tok, len) -> Some (Some tok, len)
                    | None ->

                        match identifier str with
                        | Some (tok, len) -> Some (Some tok, len)
                        | None ->

                            None
