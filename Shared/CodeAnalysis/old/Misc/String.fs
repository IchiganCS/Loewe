module Loewe.Misc.String
open System

/// The number of rows skipped in front, the column to jump to second
/// The overall number of skipped elements third
let trimStart (str: ReadOnlySpan<char>) : (int * int * int) =
    let mutable rows = 0
    let mutable columns = 0
    let mutable count = 0

    while str.Length > count && String.IsNullOrWhiteSpace (str[count].ToString ()) do
        if str[count] = '\n' then
            columns <- 1
            rows <- rows + 1
        else
            columns <- columns + 1
        count <- count + 1


    rows, columns, count
