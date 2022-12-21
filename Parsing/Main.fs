module Loewe.Parsing.Main

open Loewe.Parsing.Types
open Loewe.Parsing.Helpers

/// Have a method for each token: If a given string starts with this kind of token return Some token
/// if the method can not find a token at the beginning of the string, return None
/// If no token fits, return Error

type ParseResult =
    | Success of PositionedToken list
    | Failure of Position


/// Starts at the beginning of the given string and tries to match any kind of token
/// If a match could be made, returns the parsed token and the substring responsible for the match
/// None if no match could be made
let parseToken (str: string) : (Token * string) option =
    match parseKeyword str with
    | Some key -> Some key
    | None ->

    match parseLiteral str with
    | Some literal -> Some literal
    | None ->

    match parseSeparator str with
    | Some sep -> Some sep
    | None ->

    match parseOperator str with
    | Some op -> Some op
    | None ->

    match parseName str with
    | Some name -> Some name
    | None -> 

    None

let parse (str: string) : ParseResult =
    let mutable trimmed = str.TrimStart ()

    let mutable tokenList = []

    let mutable hasError = false
    let mutable col = 1
    let mutable line = 1

    let addTokenAtCurrentPosition tok len = 
        let position = {
            Line = line
            Coloumn = col
            Length = len
        }
        let positionedToken = {
            Token = tok
            Position = position
        }
        tokenList <- List.append tokenList [positionedToken]

    while trimmed.Length <> 0 && not hasError do
        let (rowAdd, ColAdd, substr) = skipComment trimmed
        trimmed <- substr
        line <- line + rowAdd
        if rowAdd = 0 then
            col <- col + ColAdd
        else
            col <- ColAdd
            
        match parseToken trimmed with
        | Some (tok, str) -> 

            addTokenAtCurrentPosition tok str.Length

            // advance position
            trimmed <- trimmed.Substring str.Length
            col <- col + str.Length
            let newTrimmed = trimmed.TrimStart ()
            let cutElements = trimmed.Substring (0, (trimmed.Length - newTrimmed.Length))
            let mutable newLineCount = 0
            col <- col + cutElements.Length
            for ch in cutElements do
                if ch = '\n' then
                    newLineCount <- newLineCount + 1
            if newLineCount > 0 then
                line <- line + newLineCount
                col <- 1
            trimmed <- newTrimmed

        | None -> hasError <- true

    
    if hasError then
        Failure {
            Line = line
            Coloumn = col
            Length = 0
        }
    else
        Success tokenList