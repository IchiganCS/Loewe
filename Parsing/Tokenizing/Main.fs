module Loewe.Parsing.Tokenizing.Main

open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.Tokenizing.Helpers
open Loewe.Parsing.Types

/// Have a method for each token: If a given string starts with this kind of token return Some token
/// if the method can not find a token at the beginning of the string, return None
/// If no token fits, return Error

type TokenizingResult =
    | Success of PositionedToken list
    | Failure of Position


/// Starts at the beginning of the given string and tries to match any kind of token.
/// If a match could be made, returns the parsed token and the number of chars responsible for the match.
/// None if no match could be made.
let tryTokenize (str: string ref) : (Token option * int) option =
    match tryTokenizeComment str with
    | Some len -> Some (None, len)
    | None ->

    match tryTokenizeKeyword str with
    | Some (tok, len) -> Some (Some tok, len)
    | None ->

    match tokenizeLiteral str with
    | Some (tok, len) -> Some (Some tok, len)
    | None ->

    match tryTokenizeSeparator str with
    | Some (tok, len) -> Some (Some tok, len)
    | None ->

    match tryTokenizeOperator str with
    | Some (tok, len) -> Some (Some tok, len)
    | None ->

    match tryTokenizeName str with
    | Some (tok, len) -> Some (Some tok, len)
    | None -> 

    None

let tokenize (str: string) : TokenizingResult =
    let trimmed = ref str

    let mutable tokenList = []

    let mutable hasError = false
    let mutable col = 1
    let mutable line = 1

    
    let trimStart () =
        let newTrimmed = trimmed.Value.TrimStart ()
        let cutElements = trimmed.Value.Substring (0, (trimmed.Value.Length - newTrimmed.Length))
        let mutable newLineCount = 0
        col <- col + cutElements.Length
        for ch in cutElements do
            if ch = '\n' then
                newLineCount <- newLineCount + 1
        if newLineCount > 0 then
            line <- line + newLineCount
            col <- 1
        trimmed.Value <- newTrimmed

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
        tokenList <- tokenList @ [positionedToken]

    while trimmed.Value.Length <> 0 && not hasError do
        match tryTokenize trimmed with
        | Some (tok, len) -> 
            match tok with
            | None -> 
                ()
            | Some tokVal ->
                addTokenAtCurrentPosition tokVal len
                

            // advance position
            trimmed.Value <- trimmed.Value.Substring len
            col <- col + len
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