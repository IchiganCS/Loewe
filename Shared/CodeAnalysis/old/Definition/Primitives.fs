module Loewe.Definition.Primitives




let sourceToPrimitiveMap =
    Map [
        "int", Int32
        "uint", UInt32
        "long", Int64
        "ulong", UInt64
        "float", Float32
        "double", Double64
        "bool", Bool
        "void", Void
    ]

let sourcePrimitiveTypes =
    sourceToPrimitiveMap.Keys |> Set.ofSeq