module Loewe.Parsing.Composing.Helpers

open Loewe.Parsing.Composing.Error
open Loewe.Parsing.Composing.Types
open Loewe.Parsing.Tokenizing
open Loewe.Parsing.Types
open Loewe.Parsing.Composing.Result




/// This function will always return Success. If no match could be made, an empty list will be returned
let rec private many acceptor tokens : Result<'a list> = 
    match acceptor tokens with
    | Failure _ -> Success (tokens, [])
    | Success (toks, args) -> 

    match many acceptor toks with
    | Success (endToks, newArgs) -> Success (endToks, args::newArgs)
    | Failure _ -> Success (toks, [args])

/// Like many but matches at least one.
let private oneOrMore acceptor tokens =
    match acceptor tokens with
    | Failure s -> Failure s
    | Success (toks, args) ->

    match many acceptor tokens with
    | Failure s -> Failure s // should not be reached
    | Success (toks, listMatches) -> Success (toks, args::listMatches)

/// Matches recursively until tokens is empty - if no match could be made, but tokens still
/// contains elements, failure is returned for the last error
let rec private all accepter tokens : Result<'a list> =
    match accepter tokens with
    | Failure error -> 
        match tokens with
        | [] -> Success (tokens, [])
        | _ -> Failure error
    | Success (toks, args) -> 

    match all accepter toks with
    | Success (endToks, newArgs) -> Success (endToks, args::newArgs)
    | Failure error -> Failure error

/// Tries to match with tokens - if success, Some value is returned
/// If the match could not be made, None is returned.
let private optional accepter tokens =
    match accepter tokens with
    | Success (toks, value) -> Success (toks, Some value)
    | Failure _ -> Success (tokens, None)

/// Tries to match a single token
let private token single tokens : Result<Token> = 
    match tokens with
    | head::tail -> 
        if head = single then 
            Success (tail, single) 
        else 
            Failure (Linear (tokens, (ErrorCause.Separator single), End))
    | [] -> Failure (Linear (tokens, (ErrorCause.Separator single), End))

/// Wraps a value into a composing result
let private insert (value : 'a) tokens : Result<'a> =
    Success (tokens, value)

/// Tries to match with a given accepter method - together with a previous result
/// If the previous result is a failure, the error is returned for currying, if previous
/// was a success, the accepter is tried to make a match and the result is returned tupled with
/// the previous result.
let private take (acceptor : Token list -> Result<'b>) (previousResult : Result<'a>) : Result<'a * 'b> =
    match previousResult with
    | Failure errors -> Failure errors
    | Success (tokens, res1) ->
        match acceptor tokens with
        | Success (endToks, res2) -> Success (endToks, (res1, res2))
        | Failure errors -> Failure errors

/// Matches the recurring acceptor as long as possible - as soon as no match could be made
/// the end acceptor is tried. If succeded, the list of matches of previous matches is returned.
/// If the end acceptor fails, the method fails and returns the failures of both, 
/// the recurring and end acceptor in that order.
let rec private until errorCause endAcceptor recurringAcceptor tokens =
    match recurringAcceptor tokens with
    | Failure cet -> 
        match endAcceptor tokens with
        | Success (tokens, _) -> Success (tokens, [])
        | Failure cet2 -> Failure (Multiple (tokens, errorCause, [cet; cet2] |> Set.ofList))
         
    | Success (tokens, value) ->    
        match until errorCause endAcceptor recurringAcceptor tokens with
        | Success (tokens, list) -> Success (tokens, value::list)
        | Failure cet -> Failure cet

/// Takes a previous result - if that is a failure, the failure is forwarded. If the result
/// was a success, the acceptor is tried to make the next match. If operation results in a
/// success, the value of previous and tokens of acceptor are returned.
let private skip (acceptor : Token list -> Result<'b>) previousResult : Result<'a> =
    match previousResult with
    | Failure errors -> Failure errors
    | Success (tokens, value) ->
        match acceptor tokens with
        | Success (rest, _) -> Success (rest, value)
        | Failure errors -> Failure errors

let private prepare tokens =
    Success (tokens, ())

/// Takes a list of acceptors and tried to match in that order. The first success is returned.
/// If no match could be made, Multiple Failure is returned with each error message.
let rec private any errorCause (acceptors : (Token list -> Result<'a>) list) tokens =
    match acceptors with
    | head::tail -> 
        match head tokens with
        | Success (toks, ret) -> Success (toks, ret)
        | Failure error -> 
            match any errorCause tail tokens with
            | Success (toks, ret) -> Success (toks, ret)
            | Failure otherErrors -> 
                match otherErrors with
                | Multiple (tokens, errorCause, rr) ->
                    Failure (Multiple (tokens, errorCause, rr |> Set.add error))
                | _ -> raise (System.Exception "either didn't return correct failure type")
    | _ -> Failure (Multiple (tokens, errorCause, Set.empty))


let private operationChain (leftExpr, binOp, rightExpr) : Expression =
    // we need to check operator precedence
    // The battled term is initially the left part of the lowest binary operation in the right expression
    // If the operand in that lowest expression has a lower precedence than our given operator, we need to swap precedence and are finished
    // if the operand has higher precedence we need to recursively check the operator above in the rightExpr tree

    let rec replaceLowestBattledTerm rightExpr : Expression option =
        match rightExpr with
        | Expression.BinaryOperation (battled, rop, rrval) ->
            match replaceLowestBattledTerm battled with
            | Some expr -> 
                // a term has already been replaced, just carry 
                Some (Expression.BinaryOperation (expr, rop, rrval))
            | None ->
                // no term has been replaced, we need to check if now replacing is required
                if checkOperatorPrecedence binOp rop = binOp then
                    // binOp should be executed first - build an inner operation with the left expression and the battled term
                    // and build the previous operation around it
                    Some (Expression.BinaryOperation ((Expression.BinaryOperation (leftExpr, binOp, battled)), rop, rrval))
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


let collectTo1 tokens errorCause builderFn childResult =
    match childResult with
    | Success (toks, (_, val1)) -> Success (toks, builderFn val1)
    | Failure error -> Failure (Linear (tokens, errorCause, error))

let collectTo2 tokens errorCause builderFn childResult =
    match childResult with
    | Success (toks, ((_, val1), val2)) -> Success (toks, builderFn (val1, val2))
    | Failure error -> Failure (Linear (tokens, errorCause, error))

let collectTo3 tokens errorCause builderFn childResult =
    match childResult with
    | Success (toks, (((_, val1), val2), val3)) -> Success (toks, builderFn (val1, val2, val3))
    | Failure error -> Failure (Linear (tokens, errorCause, error))

let collectTo4 tokens errorCause builderFn childResult =
    match childResult with
    | Success (toks, ((((_, val1), val2), val3), val4)) -> Success (toks, builderFn (val1, val2, val3, val4))
    | Failure error -> Failure (Linear (tokens, errorCause, error))

let collectTo5 tokens errorCause builderFn childResult =
    match childResult with
    | Success (toks, (((((_, val1), val2), val3), val4), val5)) -> Success (toks, builderFn (val1, val2, val3, val4, val5))
    | Failure error -> Failure (Linear (tokens, errorCause, error))

let clean result =
    match result with
    | Success (toks, (_, val1)) -> Success (toks, val1)
    | Failure error -> Failure error


let binaryOperationToken tokens =
    match tokens with 
    | (Operator op)::tail ->
        match op with
        | AndSign -> Success (tail, BitwiseAnd)
        | OrSign -> Success (tail, BitwiseOr)
        | Plus -> Success (tail, Addition)
        | Minus -> Success (tail, Subtraction)
        | Divide -> Success (tail, Division)
        | Star -> Success (tail, Multiplication)
        | Percent -> Success (tail, Modulo)
        | EqualSign -> Success (tail, Equal)
        | NotEqualSign -> Success (tail, NotEqual)
        | DoubleAnd -> Success (tail, And)
        | DoubleOr -> Success (tail, Or)
        | NotSign -> Failure (Linear (tokens, BinaryOperator, End))
        | TildeSign -> Failure (Linear (tokens, BinaryOperator, End))

    | _ -> Failure (Linear (tokens, BinaryOperator, End))

let unaryOperationToken tokens =
    match tokens with 
    | (Operator op)::tail ->
        match op with
        | TildeSign -> Success (tail, BitwiseNot)
        | NotSign -> Success (tail, Not)
        | Minus -> Success (tail, Negate)
        | AndSign -> Failure (Linear (tokens, UnaryOperator, End))
        | OrSign -> Failure (Linear (tokens, UnaryOperator, End))
        | Plus -> Failure (Linear (tokens, UnaryOperator, End))
        | Divide -> Failure (Linear (tokens, UnaryOperator, End))
        | Star -> Failure (Linear (tokens, UnaryOperator, End))
        | Percent -> Failure (Linear (tokens, UnaryOperator, End))
        | EqualSign -> Failure (Linear (tokens, UnaryOperator, End))
        | NotEqualSign -> Failure (Linear (tokens, UnaryOperator, End))
        | DoubleAnd -> Failure (Linear (tokens, UnaryOperator, End))
        | DoubleOr -> Failure (Linear (tokens, UnaryOperator, End))

    | _ -> Failure (Linear (tokens, UnaryOperator, End))





let rec expression tokens =
    // generally, each part of the code should expand to the maximum it could possibly mean, not just the first occurence
    // usually, this is not a problem, it becomes a problem only for expressions, since some expressions start with expressions.
    // therefore, we need to handle those expressions separately

    let rec tryExpand result =
        match result with
        | Failure errors -> Failure errors
        | Success (rest, expr) ->
            match 
                // here we try to build new expressions
                // in this way, expressions are recursively built right to left (<- this is important!)
                prepare rest |>
                take (any ExpandedExpression [
                    (methodCall expr)
                    (attribute expr)
                    (binaryOperation expr)]) |>
                clean with
            | Success (a, b) -> tryExpand (Success (a, b))
            | Failure _ -> result

    // try those which don't start with expressions
    tryExpand (
        prepare tokens |>
        take (any ErrorCause.Expression [
            literalExpression
            bracketedExpression
            unaryOperation
            functionCall
            variable]) |>
        clean)

and typedParameterList tokens =
    let rec commadTypedParameters tokens =
        prepare tokens |>
        take qualifiedIdentifier |>
        take identifier |>
        skip (token (Separator Comma)) |>
        collectTo2 tokens CommadParameterList id

    let rec lastTypedParameter tokens =
        prepare tokens |>
        take qualifiedIdentifier |>
        take identifier |>
        collectTo2 tokens LastTypedParameter id

    let rec someTypeParameter tokens =
        prepare tokens |>
        take (many commadTypedParameters) |>
        take lastTypedParameter |>
        skip (token (Separator BracketClose)) |>
        collectTo2 tokens SomeTypedParameter (fun (aheadList, last) -> aheadList@[last])

    let rec emptyParameterList tokens =
        prepare tokens |>
        take (token (Separator BracketClose)) |>
        collectTo1 tokens EmptyParameterList (fun _ -> [])

    prepare tokens |>
    skip (token (Separator BracketOpen)) |>
    take (any FilledTypedParameterList [emptyParameterList; someTypeParameter]) |>
    clean


and literal tokens =
    match tokens with
    | (Literal l)::tail -> Success (tail, l)
    | _ -> Failure (Linear (tokens, ErrorCause.Literal, End))

and literalExpression tokens =
    prepare tokens |>
    take literal |>
    collectTo1 tokens LiteralExpression Expression.Literal
        

and binaryOperation expr tokens =
    prepare tokens |>
    take (insert expr) |>
    take binaryOperationToken |>
    take expression |>
    collectTo3 tokens ErrorCause.BinaryOperation operationChain

and methodCall expr tokens =
    prepare tokens |>
    take (insert expr) |>
    skip (token (Separator Dot)) |>
    take identifier |>
    take parameterList |>
    collectTo3 tokens ErrorCause.MethodCall Expression.MethodCall

and functionCall tokens =
    prepare tokens |>
    take qualifiedIdentifier |>
    take parameterList |>
    collectTo2 tokens ErrorCause.FunctionCall Expression.FunctionCall

and parameterList tokens =
    let rec someArgument tokens =
        prepare tokens |>
        take expression |>
        skip (token (Separator Comma)) |>
        collectTo1 tokens SomeArgument id

    let rec lastArgument tokens =
        prepare tokens |>
        take expression |>
        collectTo1 tokens LastArgument id

    let rec filledArgumentList tokens =
        prepare tokens |>
        take (many someArgument) |>
        take lastArgument |>
        skip (token (Separator BracketClose)) |>
        collectTo2 tokens FilledArgumentList (fun (aheadList, last) -> aheadList@[last])

    let rec emptyArgumentList tokens =
        prepare tokens |>
        take (token (Separator BracketClose)) |>
        collectTo1 tokens EmptyArgumentList (fun _ -> [])

    prepare tokens |>
    skip (token (Separator BracketOpen)) |>
    take (any ArgumentList [emptyArgumentList; filledArgumentList]) |>
    clean

and unaryOperation tokens =
    prepare tokens |>
    take unaryOperationToken |>
    take expression |>
    collectTo2 tokens ErrorCause.UnaryOperation Expression.UnaryOperation

and variable tokens =
    prepare tokens |>
    take qualifiedIdentifier |>
    collectTo1 tokens ErrorCause.Variable Expression.Variable

and attribute expr tokens =
    prepare tokens |>
    take (insert expr) |>
    skip (token (Separator Dot)) |>
    take identifier |>
    collectTo2 tokens ErrorCause.Attribute Expression.Attribute

and qualifiedIdentifier tokens : Result<QualifiedIdentifier> =
    // This method is a little hacky. It simply reads a namespace and cuts the last namespace off
    // and treats it as an identifier. The rest of the naemspace is treated as a prefix.

    prepare tokens |>
    take (optional namespacePrefix) |>
    take identifier |>
    collectTo2 tokens ErrorCause.QualifiedIdentifier (fun (nsp, id) -> id, nsp)

and identifier tokens =
    match tokens with
    | (Identifier str)::tail -> Success (tail, str)
    | _ -> Failure (Linear (tokens, ErrorCause.Identifier, End))

/// If no prefix is given, the method fails. Global is only returned on a specific match.
and namespacePrefix tokens =
    let rec buildNamespaceRev strList =
        match strList with
        | [] -> Global
        | head::tail -> Child ((buildNamespaceRev tail), head)

    let buildNamespace strList =
        buildNamespaceRev (strList |> List.rev)

    let namespaceAndDot toks =
        prepare toks |>
        take identifier |>
        skip (token (Separator DoubleDot)) |>
        collectTo1 tokens NamespaceAndDot id

    
    prepare tokens |>
    take (oneOrMore namespaceAndDot) |>
    collectTo1 tokens ErrorCause.Namespace (fun strList -> buildNamespace strList)

and ``namespace`` tokens =
    let rec buildNamespaceRev strList =
        match strList with
        | [] -> Global
        | head::tail -> Child ((buildNamespaceRev tail), head)

    let buildNamespace strList =
        buildNamespaceRev (strList |> List.rev)

    let namespaceAndDot toks =
        prepare toks |>
        take identifier |>
        skip (token (Separator DoubleDot)) |>
        collectTo1 tokens NamespaceAndDot id

    prepare tokens |>
    take (many namespaceAndDot) |>
    take identifier |>
    collectTo2 tokens ErrorCause.Namespace (fun (strList, last) -> Child ((buildNamespace strList), last))


and bracketedExpression tokens =
    prepare tokens |>
    skip (token (Separator BracketOpen)) |>
    take expression |>
    skip (token (Separator BracketClose)) |>
    collectTo1 tokens BracketedExpression Bracketed

and statementExpression tokens =
    prepare tokens |>
    take expression |>
    skip (token (Separator Semicolon)) |>
    collectTo1 tokens StatementedExpression Statement.Expression

and knownAssignment tokens =
    prepare tokens |>
    take expression |>
    skip (token (Separator Assignment)) |>
    take expression |>
    skip (token (Separator Semicolon)) |>
    collectTo2 tokens ErrorCause.KnownAssignment Statement.KnownAssignment

and codeBlock tokens =
    prepare tokens |>
    skip (token (Separator CurvedBracketOpen)) |>
    take (statement |> until CodeBlock (token (Separator CurvedBracketClose))) |>
    clean

and newAssignment tokens =
    prepare tokens |>
    take qualifiedIdentifier |>
    take identifier |>
    skip (token (Separator Assignment)) |>
    take expression |>
    skip (token (Separator Semicolon)) |>
    collectTo3 tokens ErrorCause.NewAssignment Statement.NewAssignment

     
and statement tokens =
    prepare tokens |>
    take (any ErrorCause.Statement [
        knownAssignment;
        newAssignment;
        ``return``;
        ``if``;
        ``while``;
        statementExpression;]) |>
    clean

and ``while`` tokens =
    prepare tokens |>
    skip (token (Keyword While)) |>
    skip (token (Separator BracketOpen)) |>
    take expression |>
    skip (token (Separator BracketClose)) |>
    take codeBlock |>
    collectTo2 tokens WhileStatement Statement.While

and ``if`` tokens =
    prepare tokens |>
    skip (token (Keyword If)) |>
    skip (token (Separator BracketOpen)) |>
    take expression |>
    skip (token (Separator BracketClose)) |>
    take codeBlock |>
    collectTo2 tokens IfStatement Statement.If

and ``return`` tokens =
    prepare tokens |>
    skip (token (Keyword Return)) |>
    take expression |>
    skip (token (Separator Semicolon)) |>
    collectTo1 tokens ReturnStatement Statement.Return

and classDefinition tokens =
    prepare tokens |>
    skip (token (Keyword Class)) |>
    take identifier |>
    skip (token (Separator CurvedBracketOpen)) |>
    take (classMember |> until ClassDefinition (token (Separator CurvedBracketClose))) |>
    collectTo2 tokens ClassDefinition (fun (name, members) -> 
            TopLevelEntry.Class {
                Name = name
                Members = members |> Set.ofList
            }
        )

and classMember tokens =
    prepare tokens |>
    take (any MemberDefinition [
        methodDefinition;
        attributeDefinition;
    ]) |>
    clean

and methodDefinition tokens =
    prepare tokens |>
    take accessModifier |>
    take qualifiedIdentifier |>
    take identifier |>
    take typedParameterList |>
    take codeBlock |>
    collectTo5 tokens MethodDefinition (fun (am, qi, i, tpl, cb) -> 
        Method {
            Name = i
            AccessModifier = am
            Parameters = tpl
            Code = cb
            Return = qi
        })

and attributeDefinition tokens =
    prepare tokens |>
    take accessModifier |>
    take qualifiedIdentifier |>
    take identifier |>
    skip (token (Separator Semicolon)) |>
    collectTo3 tokens AttributeDefinition (fun (am, qi, i) ->
        ClassMember.Attribute {
            Name = i
            AccessModifier = am
            Type = qi
        })

and functionDefinition  tokens =
    prepare tokens |>
    take qualifiedIdentifier |>
    take identifier |>
    take typedParameterList |>
    take codeBlock |>
    collectTo4 tokens FunctionDefinition (fun (qi, i, tpl, cb) ->
        TopLevelEntry.Function {
            Name = i
            Return = qi
            Parameters = tpl
            Code = cb
        })

and ``topLevelSymbol`` tokens =
    prepare tokens |>
    take (any TopLevelSymbol [classDefinition; functionDefinition]) |>
    clean

and namespaceDeclaration tokens =
    prepare tokens |>
    skip (token (Keyword Namespace)) |>
    take ``namespace`` |>
    skip (token (Separator Semicolon)) |>
    collectTo1 tokens NamespaceDeclaration id

and openNamespace tokens =
    prepare tokens |>
    skip (token (Keyword Open)) |>
    take ``namespace`` |>
    skip (token (Separator Semicolon)) |>
    collectTo1 tokens OpenNamespace id

and file tokens : Result<FileContent> =
    prepare tokens |>
    take namespaceDeclaration |>
    take (many openNamespace) |>
    take (all topLevelSymbol) |>
    collectTo3 tokens File
        (fun (nsp, onsp, tls) -> 
        (nsp, onsp |> Set.ofList, tls |> Set.ofList))

and accessModifier tokens =
    match tokens with
    | (Keyword k)::tail ->
        match k with
        | Keyword.Public -> Success (tail, AccessModifier.Public)
        | Keyword.Private -> Success (tail, AccessModifier.Private)
        | _ -> Failure (Linear (tokens, ErrorCause.AccessModifier, End))
    | _ -> Failure (Linear (tokens, ErrorCause.AccessModifier, End))