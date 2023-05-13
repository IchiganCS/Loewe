module Loewe.Definition.Literals

open TokenTypes
open Primitives

let literalPrimitiveMap = 
    function
    | Int _ -> Int32
    | UnsignedInt _ -> UInt32
    | Long _ -> Int64
    | UnsignedLong _ -> UInt64
    | Float _ -> Float32
    | Double _ -> Double64
    | Literal.Bool _ -> Primitive.Bool
    | _ -> failwith "not implemented"