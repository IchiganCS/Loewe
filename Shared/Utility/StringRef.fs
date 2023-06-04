/// A cover for ReadOnlySpan<Char> in C#.
module Loewe.Shared.Utility.StringRef

type StringRef = {
    String: string
    Offset: int
    Length: int
}

module StringRef =
    let length str =
        str.Length

    let index i str =
        if i < str.Length then
            str.String[str.Offset + i]
        else
            failwith "index out of bounds for string ref"

    /// Simply advances the string ref by a fixed amount.
    /// Can be used as a substring method
    let advance count str =
        if str.Length < count then
            failwith $"cannot advance string ref by {count} - only has length {str.Length}"
        else
            {
                String = str.String
                Offset = str.Offset + count
                Length = str.Length - count
            }

    let ofString str =
        {
            String = str
            Offset = 0
            Length = str |> String.length
        }

    let empty str =
        str.Length = 0

    let first str =
        if str.Length > 0 then
            str.String[str.Offset]
        else
            failwith "there is no element in the string ref"

    let difference ref1 ref2 =
        if ref1.String <> ref2.String then
            failwith "two refs don't point to the same string"
        else
            abs (ref1.Offset - ref2.Offset)

    let trimStart str =
        let mutable extraOffset = 0

        while extraOffset < str.Length && str |> index extraOffset = ' ' do
            extraOffset <- extraOffset + 1

        {
            String = str.String
            Length = str.Length - extraOffset
            Offset = str.Offset + extraOffset
        }

    let equal normalStr refStr =
        if normalStr |> String.length <> refStr.Length then
            false
        else
            let mutable i = 0
            while i < refStr.Length && refStr |> index i = normalStr[i] do
                i <- i + 1

            i = refStr.Length


    let stripUntil char refStr =
        let mutable i = 0
        while i < refStr.Length && refStr |> index i <> char do
            i <- i + 1

        if i < refStr.Length then
            Some {
                String = refStr.String
                Offset = refStr.Offset + i
                Length = refStr.Length - i
            }
        else
            None


    let startsWith startChars str =
        if startChars |> String.length > str.Length then
            false
        else
            // construct a new string ref with the same length. should then be equal
            {
                String = str.String
                Offset = str.Offset
                Length = startChars |> String.length
            }
            |> equal startChars

        
