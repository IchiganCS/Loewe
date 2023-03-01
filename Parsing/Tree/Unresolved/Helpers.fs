module Loewe.Parsing.Tree.Unresolved.Helpers

open Loewe.Parsing.Tree.Unresolved.Types
open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.Types

let private appendError failure str =
    match failure with
    | End -> Failure (Linear ([], str, End))
    | Multiple (ls, s, _) -> 
        if s = str then 
            Failure failure
        else
            Failure (Linear (ls, str, failure))
    | Linear (ls, _, _) ->
        Failure (Linear (ls, str, failure))

let private buildEndError tokens str =
    Failure (Linear (tokens, str, End))


/// This function will always return Success. If no match could be made, an empty list will be returned
let rec private many accepter tokens : ComposingResult<'a list> = 
    match accepter tokens with
    | Failure _ -> Success (tokens, [])
    | Success (toks, args) -> 

    match many accepter toks with
    | Success (endToks, newArgs) -> Success (endToks, args::newArgs)
    | Failure _ -> Success (toks, [args])

/// Matches recursively until tokens is empty - if no match could be made, but tokens still
/// contains elements, failure is returned for the last error
let rec private all accepter tokens : ComposingResult<'a list> =
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
let private token single tokens : ComposingResult<Token> = 
    match tokens with
    | head::tail -> 
        if head = single then 
            Success (tail, single) 
        else 
            Failure (Linear (tokens, (sprintf "Expected token %s, but found %s" (string single) (string head)), End))
    | [] -> Failure (Linear (tokens, (sprintf "Expected token %s, but found none" (string single)), End))

/// Wraps a value into a composing result
let private insert (value : 'a) tokens : ComposingResult<'a> =
    Success (tokens, value)

/// Tries to match with a given accepter method - together with a previous result
/// If the previous result is a failure, the error is returned for currying, if previous
/// was a success, the accepter is tried to make a match and the result is returned tupled with
/// the previous result.
let private take (accepter : Token list -> ComposingResult<'b>) (previousResult : ComposingResult<'a>) : ComposingResult<'a * 'b> =
    match previousResult with
    | Failure errors -> Failure errors
    | Success (tokens, res1) ->
        match accepter tokens with
        | Success (endToks, res2) -> Success (endToks, (res1, res2))
        | Failure errors -> Failure errors

/// Matches the recurring acceptor as long as possible - as soon as no match could be made
/// the end acceptor is tried. If succeded, the list of matches of previous matches is returned.
/// If the end acceptor fails, the method fails and returns the failures of both, 
/// the recurring and end acceptor in that order.
let rec private until endAcceptor recurringAcceptor tokens =
    match recurringAcceptor tokens with
    | Failure cet -> 
        match endAcceptor tokens with
        | Success (tokens, _) -> Success (tokens, [])
        | Failure cet2 -> Failure (Multiple (tokens, "until", [cet; cet2] |> Set.ofList))
         
    | Success (tokens, value) ->    
        match until endAcceptor recurringAcceptor tokens with
        | Success (tokens, list) -> Success (tokens, value::list)
        | Failure cet -> Failure cet

/// Takes a previous result - if that is a failure, the failure is forwarded. If the result
/// was a success, the acceptor is tried to make the next match. If operation results in a
/// success, the value of previous and tokens of acceptor are returned.
let private skip (acceptor : Token list -> ComposingResult<'b>) previousResult : ComposingResult<'a> =
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
let rec private either (acceptors : (Token list -> ComposingResult<'a>) list) str tokens =
    match acceptors with
    | head::tail -> 
        match head tokens with
        | Success (toks, ret) -> Success (toks, ret)
        | Failure error -> 
            match either tail str tokens with
            | Success (toks, ret) -> Success (toks, ret)
            | Failure otherErrors -> 
                match otherErrors with
                | Multiple (t, s, rr) ->
                    Failure (Multiple (t, s, rr |> Set.add error))
                | _ -> raise (System.Exception "either didn't return correct failure type")
    | _ -> Failure (Multiple (tokens, str, Set.empty))


let private collectTo func str (result: ComposingResult<unit * 'a>) =
    match result with
    | Success (toks, (_, a)) -> Success (toks, func a)
    | Failure errors -> appendError errors str

let private collectTo2 func str (result: ComposingResult<(unit * 'a) * 'b>) =
    match result with
    | Success (toks, ((_, a), b)) -> Success (toks, func (a, b))
    | Failure errors -> appendError errors str

let private collectTo3 func str (result: ComposingResult<((unit * 'a) * 'b) * 'c>) =
    match result with
    | Success (toks, (((_, a), b), c)) -> Success (toks, func (a, b, c))
    | Failure errors -> appendError errors str

let private collectTo4 func str (result: ComposingResult<(((unit * 'a) * 'b) * 'c) * 'd>) =
    match result with
    | Success (toks, ((((_, a), b), c), d)) -> Success (toks, func (a, b, c, d))
    | Failure errors -> appendError errors str

let private collectTo5 func str (result: ComposingResult<((((unit * 'a) * 'b) * 'c) * 'd) * 'e>) =
    match result with
    | Success (toks, (((((_, a), b), c), d), e)) -> Success (toks, func (a, b, c, d, e))
    | Failure errors -> appendError errors str





let private operationChain (leftExpr, binOp, rightExpr) : UnresolvedExpression =
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

let binaryOperationToken tokens =
    match tokens with 
    | (Token.Operator op)::tail ->
        match op with
        | Operator.And -> Success (tail, BinaryOperation.And)
        | Operator.Or -> Success (tail, BinaryOperation.Or)
        | Operator.Plus -> Success (tail, BinaryOperation.Addition)
        | Operator.Minus -> Success (tail, BinaryOperation.Subtraction)
        | Operator.Divide -> Success (tail, BinaryOperation.Division)
        | Operator.Star -> Success (tail, BinaryOperation.Multiplication)
        | Operator.Percent -> Success (tail, BinaryOperation.Modulo)
        | Operator.Equal -> Success (tail, BinaryOperation.Equal)
        | Operator.EqualNot -> Success (tail, BinaryOperation.NotEqual)
        | Operator.Not -> buildEndError tokens "Tried to read binary operation token"

    | _ -> buildEndError tokens "Tried to read binary operation token"

let unaryOperationToken tokens =
    match tokens with 
    | (Token.Operator op)::tail ->
        match op with
        | Operator.Not -> Success (tail, UnaryOperation.Not)
        | Operator.Minus -> Success (tail, UnaryOperation.Negate)
        | Operator.And -> buildEndError tokens "Tried to read unary operation token"
        | Operator.Or -> buildEndError tokens "Tried to read unary operation token"
        | Operator.Plus -> buildEndError tokens "Tried to read unary operation token"
        | Operator.Divide -> buildEndError tokens "Tried to read unary operation token"
        | Operator.Star -> buildEndError tokens "Tried to read unary operation token"
        | Operator.Percent -> buildEndError tokens "Tried to read unary operation token"
        | Operator.Equal -> buildEndError tokens "Tried to read unary operation token"
        | Operator.EqualNot -> buildEndError tokens "Tried to read unary operation token"

    | _ -> buildEndError tokens "Tried to read unary operation token"





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
                either [
                    (methodCall expr)
                    (attribute expr);
                    (binaryOperation expr)
                    ] "expand to greater expression" rest with
            | Success (a, b) -> tryExpand (Success (a, b))
            | Failure _ -> result

    // try those which don't start with expressions
    tryExpand (either [
        literalExpression;
        bracketedExpression;
        unaryOperation;
        functionCall;
        variable;
    ] "expression" tokens)

and typedParameterList tokens =
    let rec commadTypedParameters tokens =
        prepare tokens |>
        take qualifiedIdentifier |>
        take identifier |>
        skip (token (Separator Comma)) |>
        collectTo2 id "typed parameter list"

    let rec lastTypedParameter tokens =
        prepare tokens |>
        take qualifiedIdentifier |>
        take identifier |>
        collectTo2 id "last parameter"

    let rec someTypeParameter tokens =
        prepare tokens |>
        take (many commadTypedParameters) |>
        take lastTypedParameter |>
        skip (token (Separator BracketClose)) |>
        collectTo2 (fun (aheadList, last) -> aheadList@[last]) "parameter list with entires"

    let rec noTypeParameter tokens =
        prepare tokens |>
        take (token (Separator BracketClose)) |>
        collectTo (fun _ -> []) "empty parameter list"

    prepare tokens |>
    skip (token (Separator BracketOpen)) |>
    take (either [noTypeParameter; someTypeParameter] "parameter list") |>
    collectTo id "typed parameter list"


and literal tokens =
    match tokens with
    | (Literal l)::tail -> Success (tail, l)
    | _ -> Failure (Linear (tokens, "literal", End))

and literalExpression tokens =
    prepare tokens |>
    take literal |>
    collectTo UnresolvedExpression.Literal "literal expression"

and binaryOperation expr tokens =
    prepare tokens |>
    take (insert expr) |>
    take binaryOperationToken |>
    take expression |>
    collectTo3 operationChain "binary operation"

and methodCall expr tokens =
    prepare tokens |>
    take (insert expr) |>
    skip (token (Separator Dot)) |>
    take identifier |>
    take parameterList |>
    collectTo3 UnresolvedExpression.MethodCall "method call"

and functionCall tokens =
    prepare tokens |>
    take qualifiedIdentifier |>
    take parameterList |>
    collectTo2 UnresolvedExpression.FunctionCall "function call"

and parameterList tokens =
    let rec commadParameters tokens =
        prepare tokens |>
        take expression |>
        skip (token (Separator Comma)) |>
        collectTo id "commad parameter list"

    let rec lastTypedParameter tokens =
        prepare tokens |>
        take expression |>
        collectTo id "last parameter"

    let rec someTypeParameter tokens =
        prepare tokens |>
        take (many commadParameters) |>
        take lastTypedParameter |>
        skip (token (Separator BracketClose)) |>
        collectTo2 (fun (aheadList, last) -> aheadList@[last]) "parameter list with entires"

    let rec noTypeParameter tokens =
        prepare tokens |>
        take (token (Separator BracketClose)) |>
        collectTo (fun _ -> []) "empty parameter list"

    prepare tokens |>
    skip (token (Separator BracketOpen)) |>
    take (either [noTypeParameter; someTypeParameter] "parameter list") |>
    collectTo id "typed parameter list"

and unaryOperation tokens =
    prepare tokens |>
    take unaryOperationToken |>
    take expression |>
    collectTo2 UnaryOperation "unary operation"

and variable tokens =
    prepare tokens |>
    take qualifiedIdentifier |>
    collectTo Variable "variable"

and attribute expr tokens =
    prepare tokens |>
    take (insert expr) |>
    skip (token (Separator Dot)) |>
    take identifier |>
    collectTo2 UnresolvedExpression.Attribute "attribute"

and qualifiedIdentifier tokens : ComposingResult<UnresolvedQualifiedIdentifier> =
    prepare tokens |>
    take (optional namespacePrefix) |>
    take identifier |>
    collectTo2 (fun (nsp, i) -> i, nsp) "qualified identifier"

and identifier tokens =
    match tokens with
    | (Identifier str)::tail -> Success (tail, str)
    | _ -> Failure (Linear (tokens, "identifier", End))

and namespacePrefix tokens : ComposingResult<Namespace> =
    let rec buildNamespaceRev strList =
        match strList with
        | [] -> Global
        | head::tail -> Child ((buildNamespaceRev tail), head)

    let namespaceAndDot toks =
        prepare toks |>
        take identifier |>
        skip (token (Separator DoubleDot)) |>
        collectTo id "namespace and dot"

    prepare tokens |>
    // this is done to take at least one
    take namespaceAndDot |>
    take (many namespaceAndDot) |>
    collectTo2 (
        fun (first, last) -> buildNamespaceRev ((last |> List.rev)@[first])
    ) "namespace prefix"

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
        collectTo id "namespace and dot"

    prepare tokens |>
    take (many namespaceAndDot) |>
    take identifier |>
    collectTo2 (fun (strList, last) -> Child ((buildNamespace strList), last)) "namespace"


and bracketedExpression tokens =
    prepare tokens |>
    skip (token (Separator BracketOpen)) |>
    take expression |>
    skip (token (Separator BracketClose)) |>
    collectTo Bracketed "bracketed expression"

and statementExpression tokens =
    prepare tokens |>
    take expression |>
    skip (token (Separator Semicolon)) |>
    collectTo Expression "statemented expression"

and knownAssignment tokens =
    prepare tokens |>
    take expression |>
    skip (token (Separator EqualSign)) |>
    take expression |>
    skip (token (Separator Semicolon)) |>
    collectTo2 KnownAssignment "known assignment"

and codeBlock tokens =
    prepare tokens |>
    skip (token (Separator CurvedBracketOpen)) |>
    take (statement |> until (token (Separator CurvedBracketClose))) |>
    collectTo id "code block"

and newAssignment tokens =
    prepare tokens |>
    take qualifiedIdentifier |>
    take identifier |>
    skip (token (Separator EqualSign)) |>
    take expression |>
    skip (token (Separator Semicolon)) |>
    collectTo3 NewAssignment "new assignment"

     
and statement tokens =
    either [
        knownAssignment;
        newAssignment;
        ``return``;
        ``if``;
        ``while``;
        statementExpression;
    ] "statement" tokens 

and ``while`` tokens =
    prepare tokens |>
    skip (token (Keyword While)) |>
    skip (token (Separator BracketOpen)) |>
    take expression |>
    skip (token (Separator BracketClose)) |>
    take codeBlock |>
    collectTo2 UnresolvedStatement.While "while"

and ``if`` tokens =
    prepare tokens |>
    skip (token (Keyword If)) |>
    skip (token (Separator BracketOpen)) |>
    take expression |>
    skip (token (Separator BracketClose)) |>
    take codeBlock |>
    collectTo2 UnresolvedStatement.If "while"

and ``return`` tokens =
    prepare tokens |>
    skip (token (Keyword Return)) |>
    take expression |>
    skip (token (Separator Semicolon)) |>
    collectTo UnresolvedStatement.Return "return"

and classDefinition tokens =
    prepare tokens |>
    skip (token (Keyword Class)) |>
    take identifier |>
    skip (token (Separator CurvedBracketOpen)) |>
    take (classMember |> until (token (Separator CurvedBracketClose))) |>
    collectTo2 (fun (name, members) -> 
            UnresolvedTopLevelEntry.Class {
                Name = name
                Members = members |> Set.ofList
            }
        ) "class"

and classMember tokens =
    prepare tokens |>
    take (either [
        methodDefinition;
        attributeDefinition;
    ] "class member") |>
    collectTo id "class member"

and methodDefinition tokens =
    prepare tokens |>
    take accessModifier |>
    take qualifiedIdentifier |>
    take identifier |>
    take typedParameterList |>
    take codeBlock |>
    collectTo5 (fun (am, qi, i, tpl, cb) -> 
        Method {
            Name = i
            AccessModifier = am
            Parameters = tpl
            Code = cb
            Return = qi
        }) "class method"

and attributeDefinition tokens =
    prepare tokens |>
    take accessModifier |>
    take qualifiedIdentifier |>
    take identifier |>
    skip (token (Separator Semicolon)) |>
    collectTo3 (fun (am, qi, i) ->
        Attribute {
            Name = i
            AccessModifier = am
            Type = qi
        }) "class attribute"

and functionDefinition  tokens =
    prepare tokens |>
    take qualifiedIdentifier |>
    take identifier |>
    take typedParameterList |>
    take codeBlock |>
    collectTo4 (fun (qi, i, tpl, cb) ->
        UnresolvedTopLevelEntry.Function {
            Name = i
            Return = qi
            Parameters = tpl
            Code = cb
        }) "function"

and ``topLevelSymbol`` tokens =
    prepare tokens |>
    take (either [classDefinition; functionDefinition] "top level symbol") |>
    collectTo id "top level symbol"

and namespaceDeclaration tokens =
    prepare tokens |>
    skip (token (Keyword Namespace)) |>
    take ``namespace`` |>
    skip (token (Separator Semicolon)) |>
    collectTo id "namespace declaration"

and openNamespace tokens =
    prepare tokens |>
    skip (token (Keyword Open)) |>
    take ``namespace`` |>
    skip (token (Separator Semicolon)) |>
    collectTo id "namespace open"

and file tokens : ComposingResult<UnresolvedFile> =
    prepare tokens |>
    take namespaceDeclaration |>
    take (many openNamespace) |>
    take (all topLevelSymbol) |>
    collectTo3 
        (fun (nsp, onsp, tls) -> 
        (nsp, onsp |> Set.ofList, tls |> Set.ofList)) "file"

and accessModifier tokens =
    match tokens with
    | (Keyword k)::tail ->
        match k with
        | Keyword.Public -> Success (tail, AccessModifier.Public)
        | Keyword.Private -> Success (tail, AccessModifier.Private)
        | _ -> buildEndError tokens "access modifier"
    | _ -> buildEndError tokens "access modifier"