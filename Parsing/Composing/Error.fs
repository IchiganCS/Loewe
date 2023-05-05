namespace Loewe.Parsing.Composing.Error
open Loewe.Parsing.Tokenizing

type ErrorCause =
    | Separator of Token
    | Statement
    | WhileStatement
    | IfStatement
    | ReturnStatement
    | ClassDefinition
    | MemberDefinition
    | MethodDefinition
    | AttributeDefinition
    | AccessModifier
    | FunctionDefinition
    | TopLevelSymbol
    | OpenNamespace
    | File
    | NamespaceDeclaration
    | Expression
    | CodeBlock
    | StatementedExpression
    | BracketedExpression
    | KnownAssignment
    | NewAssignment
    | Keyword
    | Identifier
    | QualifiedIdentifier
    | Literal
    | LiteralExpression
    | UnaryOperator
    | BinaryOperator
    | BinaryOperation
    | ExpandedExpression
    | UnaryOperation
    | Namespace
    | NamespaceAndDot
    | NamespacePrefix
    | MethodCall
    | FunctionCall
    | Attribute
    | CommadParameterList
    | LastTypedParameter
    | EmptyParameterList
    | FilledTypedParameterList
    | SomeTypedParameter
    | ParameterList
    | LastArgument
    | SomeArgument
    | FilledArgumentList
    | EmptyArgumentList
    | ArgumentList
    | Variable

/// A type for specifying a parsing error. The list of tokens starts with the tokens responsible for the error.
type ErrorTrace = 
    | End
    | Linear of Token list * ErrorCause * ErrorTrace
    | Multiple of Token list * ErrorCause * ErrorTrace Set


module Error =



    /// Calculates the longest token list given a set of error traces.
    let rec longestTokenList errorTraces =
        errorTraces
        |> List.map
            (function
            | End -> []
            | Linear (ls, _, _) -> ls
            | Multiple (ls, _, _) -> ls)
        |> List.maxBy List.length

    /// Formats an error trace as a string.
    let string errorTrace =
        let pad = 2
        let rec format depth =
            function
            | End -> ""
            | Linear (_, cause, et) -> 
                "\n".PadRight(pad * depth) + (string cause) + (format depth et)
            | Multiple (_, cause, set) ->
                "\n".PadRight(pad * depth) + (string cause) + 
                    (set
                    |> Set.fold (fun x y -> x + (format (depth + 1) y)) "\n")

        format 0 errorTrace




    /// Returns a trace to the path with least tokens (e.g. the path that could do most work). This trace is always linear. 
    /// If there are multiple traces which achieve the same count of tokens, the error before those different traces is taken
    /// and then ended, since this the actual cause for the error.
    let deepest error =

        // takes an error trace and returns a linear trace with the least tokens
        // in its paths. The second element is the length of the token.
        let rec helper =
            function
            | End -> End, 0
            | Linear (toks, cause, next) -> 
                let (trace, length) = helper next
                if length = 0 then
                    Linear (toks, cause, trace), toks |> List.length
                else
                    Linear (toks, cause, trace), length

            | Multiple (toks, cause, set) ->
                let traces = set |> Set.toList
                let cleanedTraces = 
                    traces 
                    |> List.map helper
                    |> List.filter (fun x -> snd x <> 0)
                    
                let minLength = 
                    cleanedTraces 
                    |> List.minBy snd 
                    |> snd

                let minTraces = 
                    cleanedTraces 
                    |> List.filter (fun x -> snd x = minLength)
                    |> List.map fst

                if minTraces |> List.length = 1 then
                    Linear (toks, cause, minTraces |> List.head), minLength
                else
                    // if there are multiple traces with the same minimal length, we don't care about their differences. 
                    // the cause given in this last error is the actual problem.
                    Linear (toks, cause, End), minLength

        fst (helper error)