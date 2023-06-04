module Loewe.Shared.CodeAnalysis.Composition

open IR
open Position



/// Information how the given tokens could be corrected to match any given result.
type RequiredCompositionEditing = {
    /// The tokens which would match. Tokens are carried over, on missing tokens, a type is inserted.
    Corrected: Choice<Token, TokenType> list
    /// The tokens which need to be replaced by the corrected tokens. The first element is the offset in the list.
    /// The second element is the count of replaced tokens.
    Replacing: int * int
    /// The editing distance of the token list. E.g. a distance of 1 means that only one token in the list was needed to be replaced.
    Distance: int
}

type IntermediateCompositionResult<'a> = Result<'a, RequiredCompositionEditing list>


let tryIdentifier tokens =
    match tokens with
    | (_, Identifier str) :: _ -> [ Ok str ]
    | _ :: (_, Identifier str) :: _ -> [
        Error {
            Corrected = [ Identifier str |> Choice1Of2 ]
            Replacing = 0, 2
            Distance = 1
        }
      ]
    | _ -> [
        Error {
            Corrected = [ IdentifierType |> Choice2Of2 ]
            Replacing = 0, 0
            Distance = 1
        }
      ]
