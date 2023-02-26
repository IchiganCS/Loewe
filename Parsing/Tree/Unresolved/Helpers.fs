module Loewe.Parsing.Tree.Unresolved.Helpers

open Loewe.Parsing.Tree.Unresolved.Types
open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.Types


let private expectLogger writer requiredStr (acceptor : Token list -> (Token list * 'a) option) (tokens : Token list option) =
    match tokens with
    | None -> None
    | Some [] -> None
    | Some l -> 

    match acceptor l with
    | Some (rest, arg) -> Some (rest, arg)
    | None ->
        writer (sprintf "Exepected \"%s\", but received %s." requiredStr (string l.Head))
        None

let private expect requiredStr acceptor tokens = 
    expectLogger System.Console.WriteLine requiredStr acceptor tokens

let rec private expectMany str accepter tokens = 
    match expect str accepter tokens with
    | None -> None
    | Some (toks, args) ->

    // weird recursion to always return Some after first expect
    // None would be wrong
    match expectMany str accepter (Some toks) with
    | Some (endToks, newArgs) -> Some (endToks, args::newArgs)
    | None -> Some (toks, [args])

let private expectSingle single tokens : Token list option = 
    match expect 
        (string single)
        (function 
        | head::tail -> if head = single then Some (tail, 0) else None
        | _ -> None)
        tokens with
        
    | Some (toks, _) -> Some toks
    | _ -> None

let private expectSingleAndCarry single magicTokens : (Token list * 'a) option =
    match magicTokens with
    | None -> None
    | Some (tokens, magicValue) -> 
        expect 
            (string single)
            (function 
            | head::tail -> if head = single then Some (tail, magicValue) else None
            | _ -> None)
            (Some tokens)

let private insert (value : 'a) tokens : (Token list * 'a) option =
    match tokens with
    | Some toks -> Some (toks, value)
    | None -> None

let private combine (getNext : Token list option -> (Token list * 'b) option) (previousResult : (Token list * 'a) option) : (Token list * ('a * 'b)) option =
    match previousResult with
    | None -> None
    | Some (toks, res1) ->
        match getNext (Some toks) with
        | Some (endToks, res2) -> Some (endToks, (res1, res2))
        | _ -> None 

let private cast (caster : 'a -> 'b) (previous : (Token list * 'a) option) =
    match previous with
    | None -> None
    | Some (toks, value) -> Some (toks, (caster value))

let private castResult caster previousGetter =
    fun x -> cast caster (previousGetter x)

let private flatten2 func (value : (Token list * ('a * 'b)) option) =
    match value with
    | Some (toks, (a, b)) -> Some (toks, func (a, b))
    | None -> None

let private flatten3 func (value : (Token list * (('a * 'b) * 'c)) option) =
    match value with
    | Some (toks, ((a, b), c)) -> Some (toks, func (a, b, c))
    | None -> None

let private flatten4 func value =
    match value with
    | Some (toks, (((a, b), c), d)) -> Some (toks, func (a, b, c, d))
    | None -> None

let private flatten5 func value =
    match value with
    | Some (toks, ((((a, b), c), d), e)) -> Some (toks, func (a, b, c, d, e))
    | None -> None


/// Builds a namespace from a given list of strings.
let private buildNamespace strList = 
    let rec buildNamespaceRev strList = 
        match strList with
        | head::tail -> Namespace.Child (buildNamespaceRev tail, head)
        | _ -> Namespace.Global

    if strList = List.empty then
        None
    else
        Some (buildNamespaceRev (List.rev strList))


let private optionFilterSet (filter : 'a -> 'b option) (set : 'a Set) : 'b Set =
    set |> 
    Set.filter (fun elem -> match filter elem with | Some _ -> true | _ -> false) |>
    Set.map (fun elem -> match filter elem with | Some b -> b | _ -> raise (System.Exception "will not be reached"))



let acceptIdentifier tokens =
    match tokens with
    | (Token.Identifier str)::tail ->
        Some (tail, str)
    | _ -> None


/// Builds a string list from dot separated identifiers
let acceptStringList tokens =
    let rec helper tokens = 
        match expect "identifier" acceptIdentifier tokens with
        | None -> None
        | Some (rest, str) ->
            match
                helper 
                (Some rest |>
                expectSingle (Token.Separator Separator.Dot)) with
            | Some (restOfRest, strList) -> Some (restOfRest, str::strList)
            | None -> Some (rest, [str])        


    helper (Some tokens)

/// Builds a namespace following proper separated sub-namespaces
let acceptNamespace tokens =
    match acceptStringList tokens with
    | Some (toks, strList) -> 
        match buildNamespace strList with
        | Some ns -> Some (toks, ns)
        | None -> None
    | None -> None

let acceptQualifiedIdentifier tokens : (Token list * UnresolvedQualifiedIdentifier) option =
    cast 
        (fun strList ->
            match List.rev strList with
            | last::front -> (last, (buildNamespace (List.rev front)))
            | [] -> ("", None) // shouldn't be reached
        )
        (acceptStringList tokens)

/// Reads open, a namespace and the semicolon
let acceptOpenDirective tokens = 
    Some tokens |>
    expectSingle (Token.Keyword Keyword.Open) |>
    expect "Namespace identifier" acceptNamespace |>
    expectSingleAndCarry (Token.Separator Separator.Semicolon)

/// Reads namespace, a namespace, and the semicolon
let acceptNamespaceDirective tokens =
    Some tokens |>
    expectSingle (Token.Keyword Keyword.Namespace) |>
    expect "Namespace identifier" acceptNamespace |>
    expectSingleAndCarry (Token.Separator Separator.Semicolon)

/// Reads the namespace declaration and the open directives
let acceptFileHeader tokens = 
    Some tokens |>
    expect "Namespace declaration" acceptNamespaceDirective |>
    combine (castResult Set.ofList (expectMany "Namespace openings" acceptOpenDirective))

let acceptBinaryOperationToken tokens =
    match tokens with
    | (Token.Operator op)::tail ->
        match op with
        | Operator.And -> Some (tail, BinaryOperation.And)
        | Operator.Or -> Some (tail, BinaryOperation.Or)
        | Operator.Plus -> Some (tail, BinaryOperation.Addition)
        | Operator.Minus -> Some (tail, BinaryOperation.Subtraction)
        | Operator.Divide -> Some (tail, BinaryOperation.Division)
        | Operator.Star -> Some (tail, BinaryOperation.Multiplication)
        | Operator.Percent -> Some (tail, BinaryOperation.Modulo)
        | Operator.Equal -> Some (tail, BinaryOperation.Equal)
        | Operator.EqualNot -> Some (tail, BinaryOperation.NotEqual)
        | Operator.Not -> None


    | _ -> None

let acceptUnaryOperationToken tokens =
    match tokens with
    | (Token.Operator op)::tail ->
        match op with
        | Operator.Not -> Some (tail, UnaryOperation.Not)
        | _ -> None

    | _ -> None

let acceptLiteral tokens =
    match tokens with
    | (Token.Literal l)::tail -> Some (tail, UnresolvedExpression.Literal l)
    | _ -> None

let acceptVariable tokens =
    match acceptQualifiedIdentifier tokens with
    | Some (toks, id) -> Some (toks, UnresolvedExpression.Variable id)
    | None -> None

let private appendToOperationChain leftExpr binOp rightExpr : UnresolvedExpression =
    // we need to check operator precedence
    // The battled term is initially the left part of the lowest binary operation in the right expression
    // If the operand in that lowest expression has a lower precedence than our given operator, we need to swap precedence and are finished
    // if the operand has higher precedence we need to recursively check the operator above in the rightExpr tree

    let rec replaceLowestBattledTerm rightExpr : UnresolvedExpression option =
        match rightExpr with
        | BinaryOperation (battled, rop, rrval) ->
            match replaceLowestBattledTerm battled with
            | Some expr -> 
                // a term has already been replaced, just carry 
                Some (UnresolvedExpression.BinaryOperation (expr, rop, rrval))
            | None ->
                // no term has been replaced, we need to check if now replacing is required
                if checkOperatorPrecedence binOp rop = binOp then
                    // binOp should be executed first - build an inner operation with the left expression and the battled term
                    // and build the previous operation around it
                    Some (UnresolvedExpression.BinaryOperation ((UnresolvedExpression.BinaryOperation (leftExpr, binOp, battled)), rop, rrval))
                else
                    None
        | _ -> None

    match replaceLowestBattledTerm rightExpr with
    | Some expr -> 
        // if Some is returned, the left expression is already built int
        expr
    | None -> 
        // if None is returned, no replacing was done - thus just build a new operation around it.
        UnresolvedExpression.BinaryOperation (leftExpr, binOp, rightExpr)


let rec acceptBinaryOperation leftExpr tokens =
    (Some tokens |>
    insert leftExpr |>
    combine (expect "binary operation" acceptBinaryOperationToken) |>
    combine (expect "second operand" acceptExpression)) |>
    
    flatten3    
        (fun (leftExpr, op1, rightExpr) ->
            appendToOperationChain leftExpr op1 rightExpr)

and acceptNewAssignment tokens =
    (Some tokens |>
    expect "qualified type" acceptQualifiedIdentifier |>
    combine (expect "variable name" acceptIdentifier) |>
    expectSingleAndCarry (Token.Separator Separator.EqualSign) |>
    combine (expect "expression" acceptExpression) |>
    expectSingleAndCarry (Token.Separator Separator.Semicolon)) |>

    flatten3 UnresolvedStatement.NewAssignment

and acceptKnownAssignment tokens =
    (Some tokens |>
    expect "variable value" acceptExpression |>
    expectSingleAndCarry (Token.Separator Separator.EqualSign) |>
    combine (expect "assignment value" acceptExpression) |>
    expectSingleAndCarry (Token.Separator Separator.Semicolon)) |>

    flatten2 UnresolvedStatement.KnownAssignment

and acceptParameterList tokens =
    // reads the parameter list, but not parantheses
    let rec helper helperTokens =
        match helperTokens with
        | (Token.Separator Separator.BracketClose)::_ -> Some (helperTokens, [])
        | _ ->
        
        match
            (Some helperTokens |>
            expect "parameter value" acceptExpression) with
        | None -> None
        | Some (toks, expr) ->
        
        match toks with
        | (Token.Separator Separator.Comma)::tail ->
            match helper tail with
            | Some (endToks, nextParams) -> Some (endToks, expr::nextParams)
            | None -> None
        | (Token.Separator Separator.BracketClose)::_ ->
            Some (toks, [expr])
        | _ -> None

    Some tokens |>
    expectSingle (Token.Separator Separator.BracketOpen) |>
    expect "parameter list content" helper |>
    expectSingleAndCarry (Token.Separator Separator.BracketClose)
        
and acceptUnaryOperation tokens =
    (Some tokens |>
    expect "unary operation" acceptUnaryOperationToken |>
    combine (expect "operand" acceptExpression)) |>

    flatten2 UnresolvedExpression.UnaryOperation

and acceptBracketedExpression tokens =
    (Some tokens |>
    expectSingle (Token.Separator Separator.BracketOpen) |>
    expect "expression" acceptExpression |>
    expectSingleAndCarry (Token.Separator Separator.BracketClose)) |>

    cast UnresolvedExpression.Bracketed

and acceptFunctionCall tokens =
    (Some tokens |>
    expect "functionName" acceptQualifiedIdentifier |>
    combine (expect "function parameters" acceptParameterList)) |>
    
    flatten2 UnresolvedExpression.FunctionCall

and acceptMethodCall expr tokens =
    (Some tokens |>
    insert expr |>
    expectSingleAndCarry (Token.Separator Separator.Dot) |>
    combine (expect "method name" acceptIdentifier) |>
    combine (expect "call parameters" acceptParameterList)) |>

    flatten3 UnresolvedExpression.MethodCall

and acceptExpression tokens : (Token list * UnresolvedExpression) option =
    let rec tryExpand = function
        | Some (toks, expr) -> 
            match tryExpand (acceptBinaryOperation expr toks) with
            | Some (restToks, expr) -> Some (restToks, expr)
            | None -> 

            match tryExpand (acceptMethodCall expr toks) with
            | Some (restToks, expr) -> Some (restToks, expr)
            | None -> 

            Some (toks, expr)
        | None -> None

    match tryExpand (acceptLiteral tokens) with
    | Some (toks, expr) -> Some (toks, expr)
    | None ->

    match tryExpand (acceptFunctionCall tokens) with
    | Some (toks, expr) -> Some (toks, expr)
    | None ->

    match tryExpand (acceptVariable tokens) with
    | Some (toks, expr) -> Some (toks, expr)
    | None ->

    match tryExpand (acceptUnaryOperation tokens) with
    | Some (toks, expr) -> Some (toks, expr)
    | None ->

    match tryExpand (acceptBracketedExpression tokens) with
    | Some (toks, expr) -> Some (toks, expr)
    | None ->

    None

and acceptIf tokens =
    (Some tokens |>
    expectSingle (Token.Keyword Keyword.If) |>
    expectSingle (Token.Separator Separator.BracketOpen) |>
    expect "condition" acceptExpression |>
    expectSingleAndCarry (Token.Separator Separator.BracketClose) |>
    combine (expect "if body" acceptCodeblock)) |>
    
    flatten2 UnresolvedStatement.If

and acceptWhile tokens =
    (Some tokens |>
    expectSingle (Token.Keyword Keyword.While) |>
    expectSingle (Token.Separator Separator.BracketOpen) |>
    expect "condition" acceptExpression |>
    expectSingleAndCarry (Token.Separator Separator.BracketClose) |>
    combine (expect "while body" acceptCodeblock)) |>

    flatten2 UnresolvedStatement.While


and acceptStatementExpression tokens =
    (Some tokens |>
    expect "expression" acceptExpression |>
    expectSingleAndCarry (Token.Separator Separator.Semicolon)) |>

    cast UnresolvedStatement.Expression

        

and acceptStatement tokens =
    match acceptNewAssignment tokens with
    | Some stat -> Some stat
    | None ->

    match acceptKnownAssignment tokens with
    | Some stat -> Some stat
    | None ->

    match acceptIf tokens with
    | Some stat -> Some stat
    | None ->

    match acceptWhile tokens with
    | Some stat -> Some stat
    | None ->

    match acceptStatementExpression tokens with
    | Some stat -> Some stat
    | None -> 

    None

and acceptCodeblock tokens =
    match tokens with
    | (Token.Separator Separator.CurvedBracketOpen)::(Token.Separator Separator.CurvedBracketClose)::tail -> Some (tail, [])
    | _ ->

    Some tokens |>
    expectSingle (Token.Separator Separator.CurvedBracketOpen) |>
    expectMany "statements" acceptStatement |>
    expectSingleAndCarry (Token.Separator Separator.CurvedBracketClose)

let acceptDeclarationParameterList tokens =
    // reads the parameter list, but not parantheses
    let rec helper helperTokens =
        match helperTokens with
        | (Token.Separator Separator.BracketClose)::_ -> Some (helperTokens, [])
        | _ ->

        match
            (Some helperTokens |>
            expect "qualified parameter type" acceptQualifiedIdentifier |>
            combine (expect "parameter name" acceptIdentifier)) with
        | None -> None
        | Some (toks, (qtype, name)) ->
        
        match toks with
        | (Token.Separator Separator.Comma)::tail ->
            match helper tail with
            | Some (endToks, nextParams) -> Some (endToks, (qtype, name)::nextParams)
            | None -> None
        | (Token.Separator Separator.BracketClose)::_ ->
            Some (toks, [(qtype, name)])
        | _ -> None

    Some tokens |>
    expectSingle (Token.Separator Separator.BracketOpen) |>
    expect "parameter list content" helper |>
    expectSingleAndCarry (Token.Separator Separator.BracketClose)
      
type UnresolvedClassMember =
    | Attribute of UnresolvedAttribute
    | Method of AccessModifier * UnresolvedFunction

let acceptClassAccessModifier tokens =
    match tokens with
    | (Token.Keyword k)::tail ->
        match k with
        | Keyword.Public -> Some (tail, AccessModifier.Public)
        | Keyword.Private -> Some (tail, AccessModifier.Private)
        | _ -> None
    | _ -> None

let acceptClassAttribute tokens =
    (Some tokens |>
    expect "access modifier" acceptClassAccessModifier |>
    combine (expect "qualified type" acceptQualifiedIdentifier) |>
    combine (expect "attribute name" acceptIdentifier) |>
    expectSingleAndCarry (Token.Separator Separator.Semicolon)) |>
    
    flatten3
        (fun (acc, qtype, name) -> UnresolvedClassMember.Attribute {
            Name = name
            Type = qtype
            AccessModifier = acc
        })

let acceptClassMethod tokens =
    (Some tokens |>
    expect "access modifier" acceptClassAccessModifier |>
    combine (expect "return type" acceptQualifiedIdentifier) |>
    combine (expect "method name" acceptIdentifier) |>
    combine (expect "parameter list" acceptDeclarationParameterList) |>
    combine (expect "method code" acceptCodeblock)) |>
    
    flatten5
        (fun (acc, ret, name, paramList, code) -> UnresolvedClassMember.Method (acc, {
            Name = name
            Return = ret
            Parameters = paramList
            Code = code
        }))

let acceptClassMember tokens =
    match acceptClassAttribute tokens with
    | Some cm -> Some cm
    | None ->

    match acceptClassMethod tokens with
    | Some cm -> Some cm
    | None -> None


let acceptClassDefinition tokens =
    (Some tokens |>
    expectSingle (Token.Keyword Keyword.Class) |>
    expect "class name" acceptIdentifier |>
    expectSingleAndCarry (Token.Separator Separator.CurvedBracketOpen) |>
    combine (castResult Set.ofList (expectMany "class members" acceptClassMember)) |>
    expectSingleAndCarry (Token.Separator Separator.CurvedBracketClose)) |>
    
    flatten2
        (fun (name, members) -> {
            Name = name
            Methods = 
                members |> 
                optionFilterSet (fun m -> match m with | Method (a, m) -> Some (a, m) | _ -> None)
            Attributes = 
                members |> 
                optionFilterSet (fun m -> match m with | Attribute a -> Some a | _ -> None)
        })

let acceptFunctionDefinition tokens =
    (Some tokens |>
    expect "return type" acceptQualifiedIdentifier |>
    combine (expect "function name" acceptIdentifier) |>
    combine (expect "parameter list" acceptDeclarationParameterList) |>
    combine (expect "function code block" acceptCodeblock)) |>
    
    flatten4
        (fun (retType, name, paramList, code) -> {
            Name = name
            Return = retType
            Parameters = paramList
            Code = code
        })

let acceptFileBodyMember namesp opened tokens =
    match acceptFunctionDefinition tokens with
    | Some (toks, fn) -> Some (toks, (UnresolvedTopLevelEntry.Function (namesp, fn, opened)))
    | None ->

    match acceptClassDefinition tokens with
    | Some (toks, cl) -> Some (toks, (UnresolvedTopLevelEntry.Class (namesp, cl, opened)))
    | None -> None
