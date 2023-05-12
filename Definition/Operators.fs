module Loewe.Definition.Operators
open CodeConstructs

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
        | Expression.BinaryOperation (battled, rop, rrval) ->
            match replaceLowestBattledTerm battled with
            | Some expr ->
                // a term has already been replaced, just carry
                Some (Expression.BinaryOperation (expr, rop, rrval))
            | None ->
                // no term has been replaced, we need to check if now replacing is required
                if checkBinaryPrecedence binOp rop = binOp then
                    // binOp should be executed first - build an inner operation with the left expression and the battled term
                    // and build the previous operation around it
                    Some (
                        Expression.BinaryOperation ((Expression.BinaryOperation (leftExpr, binOp, battled)), rop, rrval)
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
        Expression.BinaryOperation (leftExpr, binOp, rightExpr)