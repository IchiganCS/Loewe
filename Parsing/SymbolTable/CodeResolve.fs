module Loewe.Parsing.SymbolTable.CodeResolve

open ResolveError
open Loewe.Definition.CodeConstructs
open Loewe.Definition.Primitives
open Loewe.Definition.Operators
open Loewe.Definition.Literals

type ResolvingState = {
    Namespaces: Namespace list
    GlobalSymbols: PartialResolvedSymbol list
    ScopedSymbols: Variable<Type> list
}

let combineResults2 res1 res2 fn =
    match res1, res2 with
    | Error err1, Error err2 -> Error (err1 |> List.append err2)
    | Error err, Ok _
    | Ok _, Error err -> Error err
    | Ok val1, Ok val2 -> Ok (fn (val1, val2))

let combineResults3 res1 res2 res3 fn =
    match res1, res2, res3 with
    | Error err1, Error err2, Error err3 -> List.concat [ err1; err2; err3 ] |> Error
    | Error err1, Error err2, Ok _
    | Error err1, Ok _, Error err2
    | Ok _, Error err1, Error err2 -> err1 |> List.append err2 |> Error
    | Ok _, Ok _, Error err
    | Ok _, Error err, Ok _
    | Error err, Ok _, Ok _ -> Error err
    | Ok val1, Ok val2, Ok val3 -> Ok (fn (val1, val2, val3))

let resolveType state searchNamespace typeName : ResolveResult<Type> =
    match sourceToPrimitiveMap |> Map.tryFind typeName with
    | Some x -> Ok (PrimitiveType x)
    | None ->
        let searchNamespaces =
            match searchNamespace with
            | Some n -> [ n ]
            | None -> state.Namespaces

        let classDeclarations =
            state.GlobalSymbols
            |> List.choose (function
                | ClassSymbol c -> Some c
                | _ -> None)

        let matches =
            classDeclarations
            |> List.filter (fun decl -> searchNamespaces |> List.contains decl.Namespace)
            |> List.filter (fun decl -> decl.Name = typeName)
            |> List.map ClassType

        match matches with
        | [ one ] -> Ok one
        | [] -> Error [ UnknownType (searchNamespaces, typeName) ]
        | _ -> Error [ DuplicateSymbol (searchNamespaces, typeName) ]

let resolveVariable (allVariables: Variable<Type> list) variable =
    let matches = allVariables |> List.filter (fun x -> x.Name = variable)

    match matches with
    | [ one ] -> Ok one
    | [] -> Error [ UnknownVariable variable ]
    | _ -> Error [ DuplicateVariable variable ]

let rec resolveExpressionType (expr: ResolvedExpression) state =
    match expr with
    | Bracketed newExpr -> resolveExpressionType newExpr state
    | Attribute (_, a) -> Ok a.Type
    | Variable v -> Ok v.Type
    | FunctionCall (f, _) -> Ok f.Return
    | BinaryOperation (left, op, right) ->
        let resolvedLeft = resolveExpressionType left state
        let resolvedRight = resolveExpressionType right state

        match resolvedLeft, resolvedRight with
        | Ok (PrimitiveType l), Ok (PrimitiveType r) ->
            match binaryOperationPrimitives |> Map.tryFind (l, r, op) with
            | Some t -> Ok (PrimitiveType t)
            | None -> Error [ UndefinedBinaryPrimitiveOperation (l, op, r) ]
        | Ok t1, Ok t2 -> Error [ UndefinedBinaryNonPrimitiveOperation (t1, op, t2) ]
        | Error e1, Error e2 -> Error (e1 |> List.append e2)
        | Error e, Ok _
        | Ok _, Error e -> Error e
    | Literal l -> literalPrimitiveMap l |> PrimitiveType |> Ok
    | MethodCall (_, m, _) -> Ok m.Return
    | UnaryOperation (op, expr) ->
        let resolvedExpr = resolveExpressionType expr state

        match resolvedExpr with
        | Ok (PrimitiveType p) ->
            match unaryOperationPrimitives |> Map.tryFind (p, op) with
            | Some t -> Ok (PrimitiveType t)
            | None -> Error [ UndefinedUnaryPrimitiveOperation (p, op) ]
        | Ok t -> Error [ UndefinedUnaryNonPrimitiveOperation (t, op) ]
        | Error e -> Error e

let resolveAttributeOfExpression state (expr: ResolvedExpression) attrName =
    match resolveExpressionType expr state with
    | Error e -> Error e
    | Ok (PrimitiveType p) -> Error [ AttributeOfPrimitive (p, attrName) ]
    | Ok (ClassType exprType) ->
        let attrMatches =
            state.GlobalSymbols
            |> List.choose (function
                | AttributeSymbol a ->
                    if a.Owner = exprType && a.Name = attrName then
                        Some a
                    else
                        None
                | _ -> None)


        match attrMatches with
        | [ one ] -> Ok one
        | [] -> Error [ UnknownAttribute (exprType, attrName) ]
        | _ -> Error [ DuplicateAttribute (exprType, attrName) ]

let resolveMethodOfExpression state expr methodName =
    match resolveExpressionType expr state with
    | Error e -> Error e
    | Ok (PrimitiveType p) -> Error [ MethodOfPrimitive (p, methodName) ]
    | Ok (ClassType exprType) ->
        let matches =
            state.GlobalSymbols
            |> List.choose (function
                | MethodSymbol (a, _) ->
                    if a.Owner = exprType && a.Name = methodName then
                        Some a
                    else
                        None
                | _ -> None)


        match matches with
        | [ one ] -> Ok one
        | [] -> Error [ UnknownAttribute (exprType, methodName) ]
        | _ -> Error [ DuplicateAttribute (exprType, methodName) ]

let resolveFunctionSymbol state funcSymbol =
    let (name, namespaceOption) = funcSymbol

    let matchingSymbols =
        state.GlobalSymbols
        |> List.filter (function
            | FunctionSymbol (f, _) -> f.Name = name && namespaceOption |> Option.forall ((=) f.Namespace)
            | _ -> false)
        |> List.map (function
            | FunctionSymbol (f, _) -> f
            | _ -> failwith "not be reached")

    match matchingSymbols with
    | [ one ] -> Ok one
    | [] -> Error [ UnknownFunction name ]
    | _ -> Error [ DuplicateFunction name ]


let rec resolveStatement state statement : ResolveResult<ResolvedStatement * Variable<Type> list> =
    let resolveLocalExpression = resolveExpression state
    let resolveLocalCode = resolveCode state

    match statement with
    | If (expr, block) -> combineResults2 (resolveLocalExpression expr) (resolveLocalCode block) (fun x -> If x, [])
    | KnownAssignment (left, right) ->
        combineResults2 (resolveLocalExpression left) (resolveLocalExpression right) (fun x -> KnownAssignment x, [])
    | NewAssignment ((leftType, leftName), right) ->
        match leftType with
        | None -> failwith "The type of a newly assigned variable is not given"
        | Some (typeName, typeNamespace) ->
            let leftResolvedType = resolveType state typeNamespace typeName
            let rightResolvedExpression = resolveLocalExpression right

            combineResults2 leftResolvedType rightResolvedExpression (fun (t, e) ->
                let variable = { Name = leftName; Type = t }
                NewAssignment (variable, e), [ variable ])
    | Expression e ->
        resolveLocalExpression e
        |> Result.map (fun resolvedExpr -> Expression resolvedExpr, [])
    | While (condition, block) ->
        combineResults2 (resolveLocalExpression condition) (resolveLocalCode block) (fun (cond, b) ->
            While (cond, b), [])
    | Return expr -> resolveLocalExpression expr |> Result.map (fun expr -> Return expr, [])
    | IfElse (condition, ifblock, elseblock) ->
        let resolvedCondition = resolveLocalExpression condition
        let resolvedIfBlock = resolveLocalCode ifblock
        let resolvedElseBlock = resolveLocalCode elseblock

        combineResults3 resolvedCondition resolvedIfBlock resolvedElseBlock (fun (c, i, e) -> IfElse (c, i, e), [])





and resolveExpression state expr =
    let resolveLocalExpression = resolveExpression state

    match expr with
    | BinaryOperation (expr1, op, expr2) ->
        combineResults2 (resolveLocalExpression expr1) (resolveLocalExpression expr2) (fun (x, y) ->
            BinaryOperation (x, op, y))
    | UnaryOperation (op, expr) -> resolveLocalExpression expr |> Result.map (fun x -> UnaryOperation (op, x))
    | Literal l -> Ok (Literal l)
    | Variable (None, name) -> resolveVariable state.ScopedSymbols name |> Result.map Variable
    | Variable (Some _, _) -> failwith "type qualified although the variable should be an expression"
    | Attribute (expr, attr) ->
        match resolveLocalExpression expr with
        | Error e -> Error e
        | Ok resolvedExpr ->
            resolveAttributeOfExpression state resolvedExpr attr
            |> Result.map (fun attr -> Attribute (resolvedExpr, attr))
    | FunctionCall (f, expr) ->
        let functionSymbol = resolveFunctionSymbol state f

        let resolvedParameters =
            expr |> List.map resolveLocalExpression |> ResolveError.mergeResults

        combineResults2 functionSymbol resolvedParameters FunctionCall
    | MethodCall (expression, method, parameters) ->
        match resolveLocalExpression expression with
        | Error e -> Error e
        | Ok resolvedExpression ->
            let methodSymbol = resolveMethodOfExpression state resolvedExpression method

            let resolvedParameters =
                parameters |> List.map resolveLocalExpression |> ResolveError.mergeResults

            combineResults2 methodSymbol resolvedParameters (fun (resolvedMethodSymbol, resolvedParameters) ->
                MethodCall (resolvedExpression, resolvedMethodSymbol, resolvedParameters))


    | _ -> failwith "notimplemented"

and resolveCode state (code: UnresolvedCode) : ResolveResult<ResolvedCode> =
    match code with
    | [] -> Ok []
    | head :: tail ->
        let headResult = head |> resolveStatement state

        match headResult with
        | Ok (resolvedStat, newScoped) ->
            let newState = {
                GlobalSymbols = state.GlobalSymbols
                ScopedSymbols = newScoped |> List.append state.ScopedSymbols
                Namespaces = state.Namespaces
            }

            resolveCode newState tail |> Result.map (fun ls -> resolvedStat :: ls)
        | Error errors ->
            // if there was an error, continue to search for more errors
            match tail |> resolveCode state with
            | Ok _ -> Error errors
            | Error innerErrors -> Error (innerErrors |> List.append errors)

let resolveFunction state (signature, code) =
    // add signature parameters to the scoped symbols
    let newState = {
        GlobalSymbols = state.GlobalSymbols
        Namespaces = state.Namespaces
        ScopedSymbols =
            state.ScopedSymbols
            |> List.append (signature.Parameters |> List.map (fun (t, name) -> { Name = name; Type = t }))
    }

    match code |> resolveCode newState with
    | Ok resolvedCode -> FunctionSymbol (signature, resolvedCode) |> Ok
    | Error error -> Error error


let resolveMethod state (signature: MethodSignature<_>, code) =
    let thisVariable = {
        Name = "this"
        Type = ClassType signature.Owner
    }

    let newState = {
        GlobalSymbols = state.GlobalSymbols
        Namespaces = state.Namespaces
        ScopedSymbols =
            thisVariable :: state.ScopedSymbols
            |> List.append (signature.Parameters |> List.map (fun (t, name) -> { Name = name; Type = t }))
    }

    match code |> resolveCode newState with
    | Ok resolvedCode -> MethodSymbol (signature, resolvedCode) |> Ok
    | Error error -> Error error



let fullResolve (symbols: PartialResolvedSymbol list) namespaces =
    let state = {
        GlobalSymbols = symbols
        ScopedSymbols = []
        Namespaces = namespaces
    }

    symbols
    |> List.map (function
        | MethodSymbol (signature, code) -> resolveMethod state (signature, code)
        | FunctionSymbol (signature, code) -> resolveFunction state (signature, code)
        | ClassSymbol c -> ClassSymbol c |> Ok
        | AttributeSymbol a -> AttributeSymbol a |> Ok)
    |> ResolveError.mergeResults
