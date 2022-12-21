module Loewe.Tokenizing.Main

open Loewe.Tokenizing.Types
open Loewe.Tokenizing.Helpers

/// Have a method for each token: If a given string starts with this kind of token return Some token
/// if the method can not find a token at the beginning of the string, return None
/// If no token fits, return Error

type TokenizingResult =
    | Success of PositionedToken list
    | Failure of Position


/// Starts at the beginning of the given string and tries to match any kind of token
/// If a match could be made, returns the parsed token and the substring responsible for the match
/// None if no match could be made
let singleTokenize (str: string) : (Token option * string) option =
    match tokenizeComment str with
    | Some str -> Some (None, str)
    | None ->

    match tokenizeKeyword str with
    | Some (tok, str) -> Some (Some tok, str)
    | None ->

    match tokenizeLiteral str with
    | Some (tok, str) -> Some (Some tok, str)
    | None ->

    match tokenizeSeparator str with
    | Some (tok, str) -> Some (Some tok, str)
    | None ->

    match tokenizeOperator str with
    | Some (tok, str) -> Some (Some tok, str)
    | None ->

    match tokenizeName str with
    | Some (tok, str) -> Some (Some tok, str)
    | None -> 

    None

let tokenize (str: string) : TokenizingResult =
    let mutable trimmed = str

    let mutable tokenList = []

    let mutable hasError = false
    let mutable col = 1
    let mutable line = 1

    let trimStart () =
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

    trimStart ()

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
        match singleTokenize trimmed with
        | Some (tok, str) -> 
            match tok with
            | None -> 
                ()
            | Some tokVal ->
                addTokenAtCurrentPosition tokVal str.Length

            // advance position
            trimmed <- trimmed.Substring str.Length
            col <- col + str.Length
            trimStart ()

        | None -> hasError <- true

    
    if hasError then
        Failure {
            Line = line
            Coloumn = col
            Length = 0
        }
    else
        Success tokenList