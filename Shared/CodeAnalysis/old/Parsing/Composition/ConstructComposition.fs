module Loewe.Parsing.Composition.ConstructComposition

open Loewe.Definition.CodeConstructs
open Loewe.Parsing.Composition.CompositionError
open Loewe.Parsing.Composition.CompositionTypes
open Loewe.Definition.TokenTypes
open Loewe.Definition.Operators

type private IntermediateResult<'a> = Result<Token list * 'a, ErrorTrace>


/// This function will always return Ok. If no match could be made, an empty list will be returned
let rec private many acceptor tokens : IntermediateResult<'a list> =
    match acceptor tokens with
    | Error _ -> Ok (tokens, [])
    | Ok (toks, args) ->

        match many acceptor toks with
        | Ok (endToks, newArgs) -> Ok (endToks, args :: newArgs)
        | Error _ -> Ok (toks, [ args ])

/// Like many but matches at least one.
let private oneOrMore acceptor startTokens =
    match acceptor startTokens with
    | Error s -> Error s
    | Ok (interTokens, args) ->

        match many acceptor interTokens with
        | Error s -> Error s // should not be reached
        | Ok (endTokens, listMatches) -> Ok (endTokens, args :: listMatches)

/// Matches recursively until tokens is empty - if no match could be made, but tokens still
/// contains elements, Error is returned for the last error
let rec private all accepter tokens =
    match accepter tokens with
    | Error error ->
        match tokens with
        | [] -> Ok (tokens, [])
        | _ -> Error error
    | Ok (toks, args) ->

        match all accepter toks with
        | Ok (endToks, newArgs) -> Ok (endToks, args :: newArgs)
        | Error error -> Error error

/// Tries to match with tokens - if Ok, Some value is returned
/// If the match could not be made, None is returned.
let private optional accepter tokens =
    match accepter tokens with
    | Ok (toks, value) -> Ok (toks, Some value)
    | Error _ -> Ok (tokens, None)

/// Tries to match a single token
let private token single tokens : IntermediateResult<Token> =
    match tokens with
    | head :: tail ->
        if head = single then
            Ok (tail, single)
        else
            Error (Linear (tokens, (ErrorCause.Separator single), End))
    | [] -> Error (Linear (tokens, (ErrorCause.Separator single), End))

/// Wraps a value into a composing result
let private insert (value: 'a) tokens = Ok (tokens, value)

/// Tries to match with a given accepter method - together with a previous result
/// If the previous result is a Error, the error is returned for currying, if previous
/// was a Ok, the accepter is tried to make a match and the result is returned tupled with
/// the previous result.
let private take acceptor previousResult =
    match previousResult with
    | Error errors -> Error errors
    | Ok (tokens, res1) ->

        match acceptor tokens with
        | Ok (endToks, res2) -> Ok (endToks, (res1, res2))
        | Error errors -> Error errors

/// Matches the recurring acceptor as long as possible - as soon as no match could be made
/// the end acceptor is tried. If succeded, the list of matches of previous matches is returned.
/// If the end acceptor fails, the method fails and returns the Errors of both,
/// the recurring and end acceptor in that order.
let rec private until errorCause endAcceptor recurringAcceptor startTokens : IntermediateResult<'a list> =
    match recurringAcceptor startTokens with
    | Error cet ->
        match endAcceptor startTokens with
        | Ok (endTokens, _) -> Ok (endTokens, [])
        | Error cet2 -> Error (Multiple (startTokens, errorCause, [ cet; cet2 ] |> Set.ofList))

    | Ok (interTokens, value) ->
        match until errorCause endAcceptor recurringAcceptor interTokens with
        | Ok (endTokens, list) -> Ok (endTokens, value :: list)
        | Error cet -> Error cet

/// Takes a previous result - if that is a Error, the Error is forwarded. If the result
/// was an Ok, the acceptor is tried to make the next match. If operation results in an
/// Ok, the value of previous and tokens of acceptor are returned.
let private skip acceptor previousResult =
    match previousResult with
    | Error errors -> Error errors
    | Ok (tokens, value) ->

        match acceptor tokens with
        | Ok (rest, _) -> Ok (rest, value)
        | Error errors -> Error errors

let private prepare tokens = Ok (tokens, ())

/// Takes a list of acceptors and tried to match in that order. The first Ok is returned.
/// If no match could be made, Multiple Error is returned with each error message.
let rec private any errorCause acceptors tokens =
    match acceptors with
    | head :: tail ->
        match head tokens with
        | Ok (toks, ret) -> Ok (toks, ret)
        | Error error ->
            match any errorCause tail tokens with
            | Ok (toks, ret) -> Ok (toks, ret)
            | Error otherErrors ->
                match otherErrors with
                | Multiple (tokens, errorCause, rr) -> Error (Multiple (tokens, errorCause, rr |> Set.add error))
                | _ -> raise (Failure "either didn't return correct Error type")
    | _ -> Error (Multiple (tokens, errorCause, Set.empty))


let private collectTo1 tokens errorCause builderFn childResult =
    match childResult with
    | Ok (toks, (_, val1)) -> Ok (toks, builderFn val1)
    | Error error -> Error (Linear (tokens, errorCause, error))

let private collectTo2 tokens errorCause builderFn childResult =
    match childResult with
    | Ok (toks, ((_, val1), val2)) -> Ok (toks, builderFn (val1, val2))
    | Error error -> Error (Linear (tokens, errorCause, error))

let private collectTo3 tokens errorCause builderFn childResult =
    match childResult with
    | Ok (toks, (((_, val1), val2), val3)) -> Ok (toks, builderFn (val1, val2, val3))
    | Error error -> Error (Linear (tokens, errorCause, error))

let private collectTo4 tokens errorCause builderFn childResult =
    match childResult with
    | Ok (toks, ((((_, val1), val2), val3), val4)) -> Ok (toks, builderFn (val1, val2, val3, val4))
    | Error error -> Error (Linear (tokens, errorCause, error))

let private collectTo5 tokens errorCause builderFn childResult =
    match childResult with
    | Ok (toks, (((((_, val1), val2), val3), val4), val5)) -> Ok (toks, builderFn (val1, val2, val3, val4, val5))
    | Error error -> Error (Linear (tokens, errorCause, error))

let private clean result =
    match result with
    | Ok (toks, (_, val1)) -> Ok (toks, val1)
    | Error error -> Error error


let binaryOperationToken tokens =
    match tokens with
    | (Operator op) :: tail ->
        match op with
        | AndSign -> Ok (tail, BitwiseAnd)
        | OrSign -> Ok (tail, BitwiseOr)
        | Plus -> Ok (tail, Addition)
        | Minus -> Ok (tail, Subtraction)
        | Divide -> Ok (tail, Division)
        | Star -> Ok (tail, Multiplication)
        | Percent -> Ok (tail, Modulo)
        | EqualSign -> Ok (tail, Equal)
        | NotEqualSign -> Ok (tail, NotEqual)
        | DoubleAnd -> Ok (tail, And)
        | DoubleOr -> Ok (tail, Or)
        | NotSign -> Error (Linear (tokens, BinaryOperator, End))
        | TildeSign -> Error (Linear (tokens, BinaryOperator, End))

    | _ -> Error (Linear (tokens, BinaryOperator, End))

let unaryOperationToken tokens =
    match tokens with
    | (Operator op) :: tail ->
        match op with
        | TildeSign -> Ok (tail, BitwiseNot)
        | NotSign -> Ok (tail, Not)
        | Minus -> Ok (tail, Negate)
        | AndSign -> Error (Linear (tokens, UnaryOperator, End))
        | OrSign -> Error (Linear (tokens, UnaryOperator, End))
        | Plus -> Error (Linear (tokens, UnaryOperator, End))
        | Divide -> Error (Linear (tokens, UnaryOperator, End))
        | Star -> Error (Linear (tokens, UnaryOperator, End))
        | Percent -> Error (Linear (tokens, UnaryOperator, End))
        | EqualSign -> Error (Linear (tokens, UnaryOperator, End))
        | NotEqualSign -> Error (Linear (tokens, UnaryOperator, End))
        | DoubleAnd -> Error (Linear (tokens, UnaryOperator, End))
        | DoubleOr -> Error (Linear (tokens, UnaryOperator, End))

    | _ -> Error (Linear (tokens, UnaryOperator, End))





let rec expression tokens  =
    // generally, each part of the code should expand to the maximum it could possibly mean, not just the first occurence
    // usually, this is not a problem, it becomes a problem only for expressions, since some expressions start with expressions.
    // therefore, we need to handle those expressions separately

    let rec tryExpand result =
        match result with
        | Error errors -> Error errors
        | Ok (rest, expr) ->
            match
                // here we try to build new expressions
                // in this way, expressions are recursively built right to left (<- this is important!)
                prepare rest
                |> take (any ExpandedExpression [ methodCall expr; attribute expr; binaryOperation expr ])
                |> clean
            with
            | Ok (a, b) -> tryExpand (Ok (a, b))
            | Error _ -> result

    // try those which don't start with expressions
    tryExpand (
        prepare tokens
        |> take (
            any ErrorCause.Expression [
                literalExpression
                bracketedExpression
                unaryOperation
                functionCall
                variable
            ]
        )
        |> clean
    )

and typedParameterList tokens =
    let rec commadTypedParameters tokens =
        prepare tokens
        |> take qualifiedIdentifier
        |> take identifier
        |> skip (token (Separator Comma))
        |> collectTo2 tokens CommadParameterList id

    let rec lastTypedParameter tokens =
        prepare tokens
        |> take qualifiedIdentifier
        |> take identifier
        |> collectTo2 tokens LastTypedParameter id

    let rec someTypeParameter tokens =
        prepare tokens
        |> take (many commadTypedParameters)
        |> take lastTypedParameter
        |> skip (token (Separator BracketClose))
        |> collectTo2 tokens SomeTypedParameter (fun (aheadList, last) -> aheadList @ [ last ])

    let rec emptyParameterList tokens =
        prepare tokens
        |> take (token (Separator BracketClose))
        |> collectTo1 tokens EmptyParameterList (fun _ -> [])

    prepare tokens
    |> skip (token (Separator BracketOpen))
    |> take (any FilledTypedParameterList [ emptyParameterList; someTypeParameter ])
    |> clean


and literal tokens =
    match tokens with
    | (Literal l) :: tail -> Ok (tail, l)
    | _ -> Error (Linear (tokens, ErrorCause.Literal, End))

and literalExpression tokens =
    prepare tokens
    |> take literal
    |> collectTo1 tokens LiteralExpression Expression.Literal


and binaryOperation expr tokens =
    prepare tokens
    |> take (insert expr)
    |> take binaryOperationToken
    |> take expression
    |> collectTo3 tokens ErrorCause.BinaryOperation buildBinaryOperation

and methodCall expr tokens =
    prepare tokens
    |> take (insert expr)
    |> skip (token (Separator Dot))
    |> take identifier
    |> take parameterList
    |> collectTo3 tokens ErrorCause.MethodCall Expression.MethodCall

and functionCall tokens =
    prepare tokens
    |> take qualifiedIdentifier
    |> take parameterList
    |> collectTo2 tokens ErrorCause.FunctionCall Expression.FunctionCall

and parameterList tokens =
    let rec someArgument tokens =
        prepare tokens
        |> take expression
        |> skip (token (Separator Comma))
        |> collectTo1 tokens SomeArgument id

    let rec lastArgument tokens =
        prepare tokens |> take expression |> collectTo1 tokens LastArgument id

    let rec filledArgumentList tokens =
        prepare tokens
        |> take (many someArgument)
        |> take lastArgument
        |> skip (token (Separator BracketClose))
        |> collectTo2 tokens FilledArgumentList (fun (aheadList, last) -> aheadList @ [ last ])

    let rec emptyArgumentList tokens =
        prepare tokens
        |> take (token (Separator BracketClose))
        |> collectTo1 tokens EmptyArgumentList (fun _ -> [])

    prepare tokens
    |> skip (token (Separator BracketOpen))
    |> take (any ArgumentList [ emptyArgumentList; filledArgumentList ])
    |> clean

and unaryOperation tokens =
    prepare tokens
    |> take unaryOperationToken
    |> take expression
    |> collectTo2 tokens ErrorCause.UnaryOperation Expression.UnaryOperation

and qualifiedVariable tokens =
    prepare tokens
    |> take qualifiedIdentifier
    |> take identifier
    |> collectTo2 tokens ErrorCause.Variable (fun (ident, name) -> Some ident, name)

and variable tokens =
    prepare tokens
    |> take identifier
    |> collectTo1 tokens ErrorCause.Variable (fun str -> Expression.Variable (None, str))

and attribute expr tokens =
    prepare tokens
    |> take (insert expr)
    |> skip (token (Separator Dot))
    |> take identifier
    |> collectTo2 tokens ErrorCause.Attribute Expression.Attribute

and qualifiedIdentifier tokens =
    prepare tokens
    |> take (optional namespacePrefix)
    |> take identifier
    |> collectTo2 tokens ErrorCause.QualifiedIdentifier (fun (nsp, id) -> id, nsp)

and identifier tokens =
    match tokens with
    | (Identifier str) :: tail -> Ok (tail, str)
    | _ -> Error (Linear (tokens, ErrorCause.Identifier, End))

/// If no prefix is given, the method fails. Global is only returned on a specific match.
and namespacePrefix tokens =
    let rec buildNamespaceRev strList =
        match strList with
        | [] -> Global
        | head :: tail -> Child ((buildNamespaceRev tail), head)

    let buildNamespace strList = buildNamespaceRev (strList |> List.rev)

    let namespaceAndDot toks =
        prepare toks
        |> take identifier
        |> skip (token (Separator DoubleDot))
        |> collectTo1 tokens NamespaceAndDot id


    prepare tokens
    |> take (oneOrMore namespaceAndDot)
    |> collectTo1 tokens ErrorCause.Namespace buildNamespace

and ``namespace`` tokens =
    let rec buildNamespaceRev strList =
        match strList with
        | [] -> Global
        | head :: tail -> Child ((buildNamespaceRev tail), head)

    let buildNamespace strList = buildNamespaceRev (strList |> List.rev)

    let namespaceAndDot toks =
        prepare toks
        |> take identifier
        |> skip (token (Separator DoubleDot))
        |> collectTo1 tokens NamespaceAndDot id

    prepare tokens
    |> take (many namespaceAndDot)
    |> take identifier
    |> collectTo2 tokens ErrorCause.Namespace (fun (strList, last) -> Child ((buildNamespace strList), last))


and bracketedExpression tokens =
    prepare tokens
    |> skip (token (Separator BracketOpen))
    |> take expression
    |> skip (token (Separator BracketClose))
    |> collectTo1 tokens BracketedExpression Bracketed

and statementExpression tokens =
    prepare tokens
    |> take expression
    |> skip (token (Separator Semicolon))
    |> collectTo1 tokens StatementedExpression Statement.Expression

and knownAssignment tokens =
    prepare tokens
    |> take expression
    |> skip (token (Separator Assignment))
    |> take expression
    |> skip (token (Separator Semicolon))
    |> collectTo2 tokens ErrorCause.KnownAssignment Statement.KnownAssignment

and codeBlock tokens =
    prepare tokens
    |> skip (token (Separator CurvedBracketOpen))
    |> take (statement |> until CodeBlock (token (Separator CurvedBracketClose)))
    |> clean

and newAssignment tokens =
    prepare tokens
    |> take qualifiedVariable
    |> skip (token (Separator Assignment))
    |> take expression
    |> skip (token (Separator Semicolon))
    |> collectTo2 tokens ErrorCause.NewAssignment Statement.NewAssignment

and statement tokens =
    prepare tokens
    |> take (
        any ErrorCause.Statement [
            knownAssignment
            newAssignment
            ``return``
            ``if``
            ``while``
            statementExpression
        ]
    )
    |> clean

and ``while`` tokens =
    prepare tokens
    |> skip (token (Keyword While))
    |> skip (token (Separator BracketOpen))
    |> take expression
    |> skip (token (Separator BracketClose))
    |> take codeBlock
    |> collectTo2 tokens WhileStatement Statement.While

and ``if`` tokens =
    prepare tokens
    |> skip (token (Keyword If))
    |> skip (token (Separator BracketOpen))
    |> take expression
    |> skip (token (Separator BracketClose))
    |> take codeBlock
    |> collectTo2 tokens IfStatement Statement.If

and ``return`` tokens =
    prepare tokens
    |> skip (token (Keyword Return))
    |> take expression
    |> skip (token (Separator Semicolon))
    |> collectTo1 tokens ReturnStatement Statement.Return

and classDefinition tokens =
    prepare tokens
    |> skip (token (Keyword Class))
    |> take identifier
    |> skip (token (Separator CurvedBracketOpen))
    |> take (classMember |> until ClassDefinition (token (Separator CurvedBracketClose)))
    |> collectTo2 tokens ClassDefinition (fun (name, members) -> TopLevelEntry.ClassEntry (name, members))

and classMember tokens =
    prepare tokens
    |> take (any MemberDefinition [ methodDefinition; attributeDefinition ])
    |> clean

and methodDefinition tokens =
    prepare tokens
    |> take accessModifier
    |> take qualifiedIdentifier
    |> take identifier
    |> take typedParameterList
    |> take codeBlock
    |> collectTo5 tokens MethodDefinition MethodMember

and attributeDefinition tokens =
    prepare tokens
    |> take accessModifier
    |> take qualifiedIdentifier
    |> take identifier
    |> skip (token (Separator Semicolon))
    |> collectTo3 tokens AttributeDefinition AttributeMember

and functionDefinition tokens =
    prepare tokens
    |> take qualifiedIdentifier
    |> take identifier
    |> take typedParameterList
    |> take codeBlock
    |> collectTo4 tokens FunctionDefinition FunctionEntry

and ``topLevelSymbol`` tokens =
    prepare tokens
    |> take (any TopLevelSymbol [ classDefinition; functionDefinition ])
    |> clean

and namespaceDeclaration tokens =
    prepare tokens
    |> skip (token (Keyword Namespace))
    |> take ``namespace``
    |> skip (token (Separator Semicolon))
    |> collectTo1 tokens NamespaceDeclaration id

and openNamespaceDeclaration tokens =
    prepare tokens
    |> skip (token (Keyword Open))
    |> take ``namespace``
    |> skip (token (Separator Semicolon))
    |> collectTo1 tokens OpenNamespace id


and accessModifier tokens =
    match tokens with
    | (Keyword k) :: tail ->
        match k with
        | Keyword.Public -> Ok (tail, AccessModifier.Public)
        | Keyword.Private -> Ok (tail, AccessModifier.Private)
        | _ -> Error (Linear (tokens, ErrorCause.AccessModifier, End))
    | _ -> Error (Linear (tokens, ErrorCause.AccessModifier, End))

and fileContent tokens : IntermediateResult<FileContent> =
    prepare tokens
    |> take namespaceDeclaration
    |> take (many openNamespaceDeclaration)
    |> take (all topLevelSymbol)
    |> collectTo3 tokens File (fun (nsp, onsp, tls) -> nsp, nsp::onsp, tls)
