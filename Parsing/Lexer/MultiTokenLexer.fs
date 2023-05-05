module Loewe.Parser.Lexer.MultiTokenLexer

open Loewe.Parser.Lexer.TokenTypes
open Loewe.Parser.Types

/// Lexes a full string. An error is returned if any part of the string could not be lexed.
let fullString (str: string) =
    let trimmed = ref str

    let mutable tokenList = []

    let mutable hasError = false
    let mutable col = 1
    let mutable line = 1


    let trimStart () =
        let newTrimmed = trimmed.Value.TrimStart ()

        let cutElements =
            trimmed.Value.Substring (0, (trimmed.Value.Length - newTrimmed.Length))

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
        let position =
            { Line = line
              Coloumn = col
              Length = len }

        let positionedToken = { Token = tok; Position = position }
        tokenList <- tokenList @ [ positionedToken ]

    while trimmed.Value.Length <> 0 && not hasError do
        match SingleTokenLexer.any trimmed with
        | Some (tok, len) ->
            match tok with
            | None -> ()
            | Some tokVal -> addTokenAtCurrentPosition tokVal len


            // advance position
            trimmed.Value <- trimmed.Value.Substring len
            col <- col + len
            trimStart ()

        | None -> hasError <- true


    if hasError then
        Error
            { Line = line
              Coloumn = col
              Length = 0 }
    else
        Ok tokenList
