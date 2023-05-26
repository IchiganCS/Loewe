module Loewe.Definition.Operators
open CodeConstructs
open Primitives

let binaryOperationPrimitives =
    Map [
        (Int32, Int32, Multiplication), Int32
        (Int32, Int32, Addition), Int32
        (Int32, Int32, Division), Int32
        (Int32, Int32, Subtraction), Int32
        (Int32, Int32, Modulo), Int32
        (Int32, Int32, Equal), Bool
        (Int32, Int32, NotEqual), Bool

        (Int64, Int64, Multiplication), Int64
        (Int64, Int64, Addition), Int64
        (Int64, Int64, Division), Int64
        (Int64, Int64, Subtraction), Int64
        (Int64, Int64, Modulo), Int64
        (Int64, Int64, Equal), Bool
        (Int64, Int64, NotEqual), Bool

        (UInt32, UInt32, Multiplication), UInt32
        (UInt32, UInt32, Addition), UInt32
        (UInt32, UInt32, Division), UInt32
        (UInt32, UInt32, Subtraction), UInt32
        (UInt32, UInt32, Modulo), UInt32
        (UInt32, UInt32, Equal), Bool
        (UInt32, UInt32, NotEqual), Bool

        (UInt64, UInt64, Multiplication), UInt64
        (UInt64, UInt64, Addition), UInt64
        (UInt64, UInt64, Division), UInt64
        (UInt64, UInt64, Subtraction), UInt64
        (UInt64, UInt64, Modulo), UInt64
        (UInt64, UInt64, Equal), Bool
        (UInt64, UInt64, NotEqual), Bool
        
        (Int32, Float32, Multiplication), Float32
        (Int32, Float32, Addition), Float32
        (Int32, Float32, Division), Float32
        (Int32, Float32, Subtraction), Float32
        (Int32, Float32, Equal), Bool
        (Int32, Float32, NotEqual), Bool

        (Int32, Double64, Multiplication), Double64
        (Int32, Double64, Addition), Double64
        (Int32, Double64, Division), Double64
        (Int32, Double64, Subtraction), Double64
        (Int32, Double64, Equal), Bool
        (Int32, Double64, NotEqual), Bool

        (Int64, Float32, Multiplication), Float32
        (Int64, Float32, Addition), Float32
        (Int64, Float32, Division), Float32
        (Int64, Float32, Subtraction), Float32
        (Int64, Float32, Equal), Bool
        (Int64, Float32, NotEqual), Bool

        (Int64, Double64, Multiplication), Double64
        (Int64, Double64, Addition), Double64
        (Int64, Double64, Division), Double64
        (Int64, Double64, Subtraction), Double64
        (Int64, Double64, Equal), Bool
        (Int64, Double64, NotEqual), Bool

        (UInt32, Float32, Multiplication), Float32
        (UInt32, Float32, Addition), Float32
        (UInt32, Float32, Division), Float32
        (UInt32, Float32, Subtraction), Float32
        (UInt32, Float32, Equal), Bool
        (UInt32, Float32, NotEqual), Bool
        
        (UInt32, Double64, Multiplication), Double64
        (UInt32, Double64, Addition), Double64
        (UInt32, Double64, Division), Double64
        (UInt32, Double64, Subtraction), Double64
        (UInt32, Double64, Equal), Bool
        (UInt32, Double64, NotEqual), Bool
        
        (UInt64, Float32, Multiplication), Float32
        (UInt64, Float32, Addition), Float32
        (UInt64, Float32, Division), Float32
        (UInt64, Float32, Subtraction), Float32
        (UInt64, Float32, Equal), Bool
        (UInt64, Float32, NotEqual), Bool
        
        (UInt64, Double64, Multiplication), Double64
        (UInt64, Double64, Addition), Double64
        (UInt64, Double64, Division), Double64
        (UInt64, Double64, Subtraction), Double64
        (UInt64, Double64, Equal), Bool
        (UInt64, Double64, NotEqual), Bool

        (Float32, Double64, Multiplication), Double64
        (Float32, Double64, Addition), Double64
        (Float32, Double64, Subtraction), Double64
        (Float32, Double64, Division), Double64
        (Float32, Double64, Equal), Bool
        (Float32, Double64, NotEqual), Bool

        (Float32, Int32, Multiplication), Float32
        (Float32, Int32, Addition), Float32
        (Float32, Int32, Division), Float32
        (Float32, Int32, Subtraction), Float32
        (Float32, Int32, Equal), Bool
        (Float32, Int32, NotEqual), Bool

        (Double64, Int32, Multiplication), Double64
        (Double64, Int32, Addition), Double64
        (Double64, Int32, Division), Double64
        (Double64, Int32, Subtraction), Double64
        (Double64, Int32, Equal), Bool
        (Double64, Int32, NotEqual), Bool

        (Float32, Int64, Multiplication), Float32
        (Float32, Int64, Addition), Float32
        (Float32, Int64, Division), Float32
        (Float32, Int64, Subtraction), Float32
        (Float32, Int64, Equal), Bool
        (Float32, Int64, NotEqual), Bool

        (Double64, Int64, Multiplication), Double64
        (Double64, Int64, Addition), Double64
        (Double64, Int64, Division), Double64
        (Double64, Int64, Subtraction), Double64
        (Double64, Int64, Equal), Bool
        (Double64, Int64, NotEqual), Bool

        (Float32, UInt32, Multiplication), Float32
        (Float32, UInt32, Addition), Float32
        (Float32, UInt32, Division), Float32
        (Float32, UInt32, Subtraction), Float32
        (Float32, UInt32, Equal), Bool
        (Float32, UInt32, NotEqual), Bool
        
        (Double64, UInt32, Multiplication), Double64
        (Double64, UInt32, Addition), Double64
        (Double64, UInt32, Division), Double64
        (Double64, UInt32, Subtraction), Double64
        (Double64, UInt32, Equal), Bool
        (Double64, UInt32, NotEqual), Bool
        
        (Float32, UInt64, Multiplication), Float32
        (Float32, UInt64, Addition), Float32
        (Float32, UInt64, Division), Float32
        (Float32, UInt64, Subtraction), Float32
        (Float32, UInt64, Equal), Bool
        (Float32, UInt64, NotEqual), Bool
        
        (Double64, UInt64, Multiplication), Double64
        (Double64, UInt64, Addition), Double64
        (Double64, UInt64, Division), Double64
        (Double64, UInt64, Subtraction), Double64
        (Double64, UInt64, Equal), Bool
        (Double64, UInt64, NotEqual), Bool

        (Double64, Float32, Multiplication), Double64
        (Double64, Float32, Addition), Double64
        (Double64, Float32, Subtraction), Double64
        (Double64, Float32, Division), Double64
        (Double64, Float32, Equal), Bool
        (Double64, Float32, NotEqual), Bool

        (Bool, Bool, And), Bool
        (Bool, Bool, Or), Bool
        (Bool, Bool, Equal), Bool
        (Bool, Bool, NotEqual), Bool
    ]

let unaryOperationPrimitives = 
    Map [
        (Int32, Negate), Int32
        (Int64, Negate), Int64
        (Float32, Negate), Float32
        (Double64, Negate), Double64
        (Int32, BitwiseNot), Int32
        (Int64, BitwiseNot), Int64
        (UInt32, BitwiseNot), UInt32
        (UInt64, BitwiseNot), UInt64
        (Float32, BitwiseNot), Float32
        (Double64, BitwiseNot), Double64
        (Bool, Not), Bool
    ]

let checkBinaryPrecedence binOp1 binOp2 =
    match binOp1 with
    | Multiplication
    | Division
    | Modulo
    | Equal
    | NotEqual
    | And
    | BitwiseAnd -> binOp1
    | Addition
    | Subtraction
    | Or
    | BitwiseOr ->
        match binOp2 with
        | Addition
        | Subtraction
        | Or
        | BitwiseOr -> binOp1
        | _ -> binOp2


/// Given an existing rightExpr and a binary operation, a new left expression is added to the existing expression,
/// with respect to all rules of precedence.
let buildBinaryOperation (leftExpr, binOp, rightExpr) =
    // we need to check operator precedence
    // The battled term is initially the left part of the lowest binary operation in the right expression
    // If the operand in that lowest expression has a lower precedence than our given operator, we need to swap precedence and are finished
    // if the operand has higher precedence we need to recursively check the operator above in the rightExpr tree

    let rec replaceLowestBattledTerm rightExpr =
        match rightExpr with
        | BinaryOperation (battled, rop, rrval) ->
            match replaceLowestBattledTerm battled with
            | Some expr ->
                // a term has already been replaced, just carry
                Some (BinaryOperation (expr, rop, rrval))
            | None ->
                // no term has been replaced, we need to check if now replacing is required
                if checkBinaryPrecedence binOp rop = binOp then
                    // binOp should be executed first - build an inner operation with the left expression and the battled term
                    // and build the previous operation around it
                    Some (
                        BinaryOperation ((BinaryOperation (leftExpr, binOp, battled)), rop, rrval)
                    )
                else
                    None
        | _ -> None

    match replaceLowestBattledTerm rightExpr with
    | Some expr ->
        // if Some is returned, the left expression is already built int
        expr
    | None ->
        // if None is returned, no replacing was done - thus just build a new operation around it.
        BinaryOperation (leftExpr, binOp, rightExpr)