module Loewe.Shared.CodeAnalysis.Position


type MultiLinePosition = {
    FileName: string
    StartLine: int
    StartColumn: int
    EndLine: int
    EndColumn: int
}

type Position = {
    FileName: string
    Line: int
    Column: int
    Length: int
}

module Position = 
    let fuse pos1 pos2 =
        if pos1.FileName <> pos2.FileName then
            failwith "cannot fuse two position from different files"
        elif abs (pos1.Line - pos2.Line) > 1 then
            failwith "cannot fuse two positions which are more than one line apart"

        let smaller, greater = 
            match pos1.Line, pos2.Line with
            | line1, line2 when line1 < line2 -> pos1, pos2
            | line1, line2 when line1 > line2 -> pos2, pos1
            | _ ->
                match pos1.Column, pos2.Column with
                | col1, col2 when col1 < col2 -> pos1, pos2
                | col1, col2 when col1 > col2 -> pos2, pos1
                | _ -> failwith "cannot fuse two positions at the same start"

        {
            FileName = pos1.FileName
            StartLine = smaller.Line
            StartColumn = smaller.Column
            EndLine = greater.Line
            EndColumn = greater.Column + greater.Length
        }



