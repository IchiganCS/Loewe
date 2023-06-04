module Loewe.Shared.CodeAnalysis.Lexing

open Loewe.Shared.Utility.StringRef
open Token
open System.Text.RegularExpressions
open IR
open Position


/// Strips string of any whitespaces and comments, but not of newlines
let private stripBeginning string =
    let noWhiteSpaces = string |> StringRef.trimStart

    if noWhiteSpaces |> StringRef.startsWith "//" then
        match noWhiteSpaces |> StringRef.stripUntil '\n' with
        | Some s -> s
        | None -> noWhiteSpaces
    else
        noWhiteSpaces

let private digits = "0123456789ABCDEF"

let private numberRegex =
    Regex (
        "\G(?<sign>\+|-)?(?<sig>0x|0o|0b)?(?<numbersBeforeDot>[0123456789ABCDEF]*)(?<dot>\.?)(?<numbersAfterDot>[0123456789ABCDEF]+)(?<end>ul|u|l|f|d)?",
        RegexOptions.Compiled
    )

let private nameRegex =
    Regex ("\G[a-z|A-Z|_][a-z|A-Z|\d|_]*", RegexOptions.Compiled)

let private isSeperatorOrSpace char =
    let str = string char

    if char = ' ' || Token.separatorMap |> Map.keys |> Seq.contains str then
        true
    else
        false



let lexSeparator string =
    let hits =
        separatorMap
        |> Map.filter (fun sep _ -> string |> StringRef.startsWith sep)

    if hits |> Map.count = 1 then
        let str, sep = hits |> Map.maxKeyValue
        Some (sep, string |> StringRef.advance (str |> String.length))
    else
        None

let lexKeyword string =
    let hits =
        keywordMap
        |> Map.filter (fun sep _ -> string |> StringRef.startsWith sep)

    if hits |> Map.count = 1 then
        let str, k = hits |> Map.maxKeyValue

        Some (k, string |> StringRef.advance (str |> String.length))
    else
        None

let lexOperator string =
    // there may be multiple hits, consider ! and !=
    // the longer hit should be selected
    let hits =
        operatorMap
        |> Map.filter (fun op _ -> string |> StringRef.startsWith op)
        |> Map.toList

    if hits |> List.length > 0 then
        let finalHit =
            if hits |> List.length = 1 then
                hits |> List.head
            else
                hits |> List.maxBy (fun (s, _) -> s |> String.length)

        Some (finalHit |> snd, string |> StringRef.advance (finalHit |> fst |> String.length))
    else
        None

let lexBoolLiteral string =
    let trueStr = "true"
    let falseStr = "false"

    if string |> StringRef.startsWith trueStr then
        Some (true, string |> StringRef.advance (trueStr |> String.length))
    else if string |> StringRef.startsWith falseStr then
        Some (false, string |> StringRef.advance (falseStr |> String.length))
    else
        None

let lexNumberLiteral string =
    let mat = numberRegex.Match (string.String, string.Offset)


    if not mat.Success then
        None
    elif
        // make sure the last char is a valid follow up char
        // additionally, make sure to not be out of bounds
        mat.ValueSpan.Length < string.Length
        && not (isSeperatorOrSpace (string |> StringRef.index mat.ValueSpan.Length))
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


                let returnString = string |> StringRef.advance mat.ValueSpan.Length
                let dottedString = beforeDot + "." + afterDot

                if numberType = "f" || (numberType = "" && dot <> "") then
                    Some (IR.Float ((float32 signMul) * float32 dottedString), returnString)
                elif numberType = "d" then
                    Some (IR.Double ((double signMul) * double dottedString), returnString)
                elif numberType = "" then
                    Some (IR.Int (signMul * System.Convert.ToInt32 (concatString, numberBaseVal)), returnString)
                elif numberType = "u" then
                    Some (IR.UnsignedInt (System.Convert.ToUInt32 (concatString, numberBaseVal)), returnString)
                elif numberType = "ul" then
                    Some (IR.UnsignedLong (System.Convert.ToUInt64 (concatString, numberBaseVal)), returnString)
                elif numberType = "l" then
                    Some (
                        IR.Long ((int64 signMul) * System.Convert.ToInt64 (concatString, numberBaseVal)),
                        returnString
                    )
                else
                    // shouldn't be reached
                    failwith "invalid regex match for number literal"

let lexStringLiteral (str: StringRef) =
    // this seems to be a little hard to do with Regex
    if str.Length < 2 || '\"' <> (str |> StringRef.first) then
        None
    else

        let mutable res = ""
        let add char = res <- res + string char

        let mutable i = 1
        let mutable hasBackslash = false
        let mutable reachedEnd = false

        while str.Length > i && not reachedEnd do
            if str |> StringRef.index i = '\"' && not hasBackslash then
                reachedEnd <- true
            elif hasBackslash then
                match str |> StringRef.index i with
                | 'n' -> add '\n'
                | 't' -> add '\t'
                | '\\' -> add '\\'
                | '\"' -> add '\"'
                | _ -> ()

                hasBackslash <- false
            elif str |> StringRef.index i = '\\' then
                hasBackslash <- true
            else
                add (str |> StringRef.index i)

            i <- i + 1

        if i > str.Length || not reachedEnd then
            // we left the loop because there are no more characters, not because we reached the end
            None
        else
            Some (res, str |> StringRef.advance i)

let lexCharLiteral string =
    if string |> StringRef.startsWith "\'" then
        if string.Length >= 2 then
            let ch = string |> StringRef.first

            if string |> StringRef.startsWith "\'" then
                Some (ch, string |> StringRef.advance 3)
            else
                None
        else
            None
    else
        None

let lexLiteral string =
    match lexCharLiteral string with
    | Some (c, str) -> Some (IR.Char c, str)
    | None ->

        match lexStringLiteral string with
        | Some (st, str) -> Some (IR.String st, str)
        | None ->

            match lexBoolLiteral string with
            | Some (b, str) -> Some (IR.Bool b, str)
            | None ->

                match lexNumberLiteral string with
                | Some (lit, str) -> Some (lit, str)
                | None -> None

let lexIdentifier string =
    let mat = nameRegex.Match (string.String, string.Offset)

    if mat.Success then
        Some (mat.Value, string |> StringRef.advance mat.ValueSpan.Length)
    else
        None

let lexSingleToken string =
    // literals need to before operators because literals can start with "-"
    match lexLiteral string with
    | Some (l, str) -> Some (Literal l, str)
    | None ->

        // operators should go before separators because operators can start with "="
        match lexOperator string with
        | Some (op, str) -> Some (Operator op, str)
        | None ->

            match lexSeparator string with
            | Some (sep, str) -> Some (Separator sep, str)
            | None ->

                match lexKeyword string with
                | Some (key, str) -> Some (Keyword key, str)
                | None ->

                    match lexIdentifier string with
                    | Some (id, str) -> Some (Identifier id, str)
                    | None -> None




/// The result for lexing a string to multiple tokens. For success, a list of positioned tokens is returned.
/// On error, there is an attempt made to continue tokenizing by skipping faulty chars one by one. All of the chars required to be skipped
/// are stored as well as all other tokens which could be tokenized.
type LexingResult = Result<PositionedToken list, (Position * char) list * PositionedToken list>



let rec private lexStringRef fileName string startColumn startLine =
    let cleaned = string |> stripBeginning

    if cleaned |> StringRef.empty then
        Ok []
    else
        let startColumn = startColumn + (cleaned |> StringRef.difference string)
        let string = cleaned

        match lexSingleToken string with
        | Some (currentToken, restString) ->
            let isNewLine = currentToken = Separator NewLine

            let endColumn =
                if isNewLine then
                    0
                else
                    startColumn + (string |> StringRef.difference restString)

            let endLine = if isNewLine then startLine + 1 else startLine

            let currentPos = {
                FileName = fileName
                Column = startColumn
                Line = startLine
                Length = if isNewLine then 1 else endColumn - startColumn
            }

            let positionedToken = currentPos, currentToken

            match lexStringRef fileName restString endColumn endLine with
            | Ok ptList -> Ok (positionedToken :: ptList)
            | Error (errors, ptList) -> Error (errors, positionedToken :: ptList)

        | None -> // we have to skip a char
            let skippedChar = string |> StringRef.first
            let rest = string |> StringRef.advance 1

            let isNewLine = skippedChar = '\n'

            let endColumn = if isNewLine then 0 else startColumn + 1

            let endLine = if isNewLine then startLine + 1 else startLine

            let currentPos = {
                FileName = fileName
                Column = startColumn
                Line = startLine
                Length = 1
            }

            let error = currentPos, skippedChar

            match lexStringRef fileName rest endColumn endLine with
            | Ok ptList -> Error ([ error ], ptList)
            | Error (errors, ptList) -> Error (error :: errors, ptList)


/// Lexes an entire string. It returns a lot of information useful for backtracing and is error robust.
let lexString fileName string =
    lexStringRef fileName (string |> StringRef.ofString) 0 0
