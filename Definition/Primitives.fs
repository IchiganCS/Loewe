module Loewe.Definition.Primitives


type Primitive =
    | Int32
    | Int64
    | Float32
    | Double64
    | UInt32
    | UInt64
    | String
    | Char
    | Void

let sourceToPrimitiveMap =
    Map [
        "int", Int32
        "uint", UInt32
        "long", Int64
        "ulong", UInt64
        "float", Float32
        "double", Double64
        "string", String
        "bool", Char
        "void", Void
    ]

let sourcePrimitiveTypes =
    sourceToPrimitiveMap.Keys |> Set.ofSeq