module Loewe.Shared.CodeAnalysis.Parsing

open Position
open Token
open BuiltIn
open Loewe.Shared.Utility.SpikeList

module Tree =
    type PotentiallyModuledIdentifier = {
        Identifier: string WithToken
        Module: (Module WithTokens) option

        Tokens: PositionedToken list
    }

    type Type =
        | PrimitiveType of Primitive * PositionedToken
        | UserType of PotentiallyModuledIdentifier

    type Expression =
        | BinaryOperationExpr of BinaryOperationData
        | UnaryOperationExpr of UnaryOperationData
        | LiteralExpr of LiteralData
        | VariableExpr of VariableData
        | AttributeExpr of AttributeData
        | BracketedExpr of BracketedData
        | FunctionCallExpr of FunctionCallData
        | MethodCallExpr of MethodCallData


    and BinaryOperationData = {
        Left: Expression
        Operator: BinaryOperation
        OperatorToken: PositionedToken
        Right: Expression

        Tokens: PositionedToken list
    }

    and UnaryOperationData = {
        Operand: Expression
        Operator: UnaryOperation
        OperatorToken: PositionedToken

        Tokens: PositionedToken list
    }

    and LiteralData = {
        Value: Literal
        Token: PositionedToken
    }

    and VariableData = {
        Identifier: string
        Token: PositionedToken
    }

    and AttributeData = {
        Operand: Expression
        DotToken: PositionedToken
        Name: string
        NameToken: string

        Tokens: PositionedToken list
    }

    and BracketedData = {
        Inner: Expression
        LeftBracket: PositionedToken
        RightBracket: PositionedToken

        Tokens: PositionedToken list
    }

    and ParameterCallData = {
        LeftBracket: PositionedToken
        RightBracket: PositionedToken

        Members: SpikeList<Expression, PositionedToken>

        Tokens: PositionedToken list
    }

    
    and ParameterDefineData = {
        LeftBracket: PositionedToken
        RightBracket: PositionedToken

        Members: SpikeList<(Type WithTokens * string WithToken), PositionedToken>

        Tokens: PositionedToken list
    }

    and FunctionCallData = {
        Name: PotentiallyModuledIdentifier
        Parameters: ParameterCallData

        Tokens: PositionedToken list
    }

    and MethodCallData = {
        Expression: Expression
        DotToken: PositionedToken
        Name: string
        Parameters: ParameterCallData

        Tokens: PositionedToken list
    }

    let tokensOfExpression expr =
        match expr with
        | MethodCallExpr data -> data.Tokens
        | FunctionCallExpr data -> data.Tokens
        | BinaryOperationExpr data -> data.Tokens
        | UnaryOperationExpr data -> data.Tokens
        | AttributeExpr data -> data.Tokens
        | BracketedExpr data -> data.Tokens
        | LiteralExpr data -> [ data.Token ]
        | VariableExpr data -> [ data.Token ]

    type Statement =
        | KnownAssignmentStat of KnownAssignmentStatData
        | NewAssignmentStat of NewAssignmentStatData
        | ExpressionStat of ExpressionStatData
        | WhileStat of WhileStatData
        | ReturnStat of ReturnStatData

    and KnownAssignmentStatData = {
        Name: string
        NameToken: PositionedToken
        AssignmentToken: PositionedToken
        Value: Expression

        Tokens: PositionedToken list
    }

    and TypeHint = {
        OfToken: PositionedToken
        Type: Type
        TypeTokens: PositionedToken list

        Tokens: PositionedToken list
    }

    and NewAssignmentStatData = {
        NewToken: PositionedToken
        Name: string
        TypeHint: TypeHint option
        NameToken: string
        AssignmentToken: PositionedToken
        Value: Expression

        Tokens: PositionedToken list
    }

    and ExpressionStatData = { Expression: Expression }

    and LongCodeBlock = {
        LeftBracket: PositionedToken
        RightBracket: PositionedToken
        Statements: Statement list

        Tokens: PositionedToken list
    }

    and CodeBlock =
        | Single of Statement
        | Multiple of LongCodeBlock

    and WhileStatData = {
        WhileToken: PositionedToken
        Condition: Expression
        CodeBlock: CodeBlock

        Tokens: PositionedToken list
    }

    and ReturnStatData = {
        ReturnToken: PositionedToken
        Value: Expression

        Tokens: PositionedToken list
    }

    type ClassData = {
        ClassWordToken: PositionedToken
        Name: string
        NameToken: PositionedToken
        LeftBracket: PositionedToken
        RightBracket: PositionedToken
        Tokens: PositionedToken list
    }

    type FunctionData = {
        Name: string
        NameToken: PositionedToken
        Parameters: ParameterDefineData
        Code: CodeBlock
        Tokens: PositionedToken list
    }

    type MethodDefineData = {
        AccessModifier: AccessModifier
        AccessModifierToken: PositionedToken
        Name: string
        NameToken: PositionedToken
        Parameters: ParameterDefineData
        Code: CodeBlock
        Tokens: PositionedToken list
    }

    type AttributeDefineData = {
        AccessModifier: AccessModifier
        AccessModifierToken: PositionedToken
        Name: string
        NameToken: PositionedToken
        Type: Type
        TypeTokens: PositionedToken list

        Tokens: PositionedToken list
    }

    type ModuleDefineData = {
        Functions: FunctionData list
        Classes: ClassData list
        Name: string
        Parent: Module
    }



module Error =

    type Edit =
        | Missing of TokenType * Position
        | Unneeded of PositionedToken

    [<RequireQualifiedAccess>]
    type Step =
        | Identifier
        | Signature
        | Variable
        | BinaryOperation
        | Expression

    type Error = {
        Edits: Edit list

        /// The first element is the most outer cause. The last cause the most inner.
        Trace: Step list
    }

    let lessEditsThan greater smaller =
        smaller |> List.sumBy (fun x -> x.Edits |> List.length)
        <= (greater |> List.sumBy (fun x -> x.Edits |> List.length))







open Error
open Tree

type Context = {
    /// The current position of the context.
    Position: Position

    /// The tokens which yet need to be parsed.
    Tokens: PositionedToken list

    /// Already encountered errors in the context.
    Errors: Error list

    /// The current trace of the context.
    Trace: Step list
}

type IntermediateResult<'a> = Result<'a, Error list> * Context
type ParsingFun<'a> = Context -> IntermediateResult<'a>

type ParsingResult<'a> = 'a list * Error list

type FileParsingResult = ParsingResult<TopLevelSymbol>


let private appendStep (context : Context) step = 
    {
        context with
            Trace = step::context.Trace
    }



let private success context value pos tail : IntermediateResult<'a> =
    Ok value,
    {
        context with
            Position = pos
            Tokens = tail
    }

let private failure context edits restPosition restTokens : IntermediateResult<'a> =
    Error ([ { Edits = edits; Trace = context.Trace } ]),
    {
        context with
            Position = restPosition
            Tokens = restTokens
    }


let tryIdentifier (context: Context) =
    let context = Step.Identifier |> appendStep context

    match context.Tokens with
    | (pos, Identifier str) :: tail -> success context (str, (pos, Identifier str)) (pos |> Position.endOf) tail
    | tok :: (_, Identifier _) :: tail ->
        failure context [ Unneeded tok ] (tok |> fst |> Position.startOf) tail
    | tail -> failure context [ Missing (IdentifierType, context.Position) ] context.Position tail


let tryVariableExpression (context: Context) =
    let context = Step.Variable |> appendStep context

    match tryIdentifier context with
    | Ok (ident, tok), context -> Ok (VariableExpr { Identifier = ident; Token = tok }), context
    | Error e, c -> Error e, c

let tryBinaryExpressionCont (context: Context) = failwith "not implemented"

let private combine res1 (res2, context) func : IntermediateResult<'a> =
    match res1, res2 with
    | Error e1, Error e2 -> Error (e1 @ e2), context
    | Error e, Ok _
    | Ok _, Error e -> Error e, context
    | Ok left, Ok right -> Ok (func left right), context


let tryBinaryExpression leftRes context : IntermediateResult<Expression> =
    let context = Step.BinaryOperation |> appendStep context
    
    combine leftRes (tryBinaryExpressionCont context) (fun left ((binOp, binOpTok), right) ->
        BinaryOperationExpr {
            Left = left
            Right = right
            Operator = binOp
            OperatorToken = binOpTok

            Tokens = (left |> tokensOfExpression) @ [ binOpTok ] @ (right |> tokensOfExpression)
        })


let rec tryExpression (context: Context) =
    let starters = [ tryVariableExpression ]
    let expanders = [ tryBinaryExpression ]

    let startersResult =
        starters
        |> List.fold
            (fun prev current ->
                match prev with
                | None -> Some (current context)
                | Some (Ok value, c) -> Some (Ok value, c)
                | Some (Error e, c) ->
                    match current context with
                    | Ok value, c -> Some (Ok value, c)
                    | Error e2, c2 when e2 |> lessEditsThan e -> Some (Error e2, c2)
                    | _ -> Some (Error e, c))
            None
        |> Option.get


    match startersResult with
    | Ok expr, context ->
        let expandedResult =
            expanders
            |> List.fold
                (fun prev current ->
                    match prev with
                    | None -> Some (current expr context)
                    | Some (Ok value, c) -> Some (Ok value, c)
                    | Some (Error e, c) ->
                        match current expr context with
                        | Ok value, c -> Some (Ok value, c)
                        | Error e2, c2 when lessEditsThan e2 e -> Some (Error e2, c2)
                        | _ -> Some (Error e, c))
                None
            |> Option.get

        match expandedResult with
        | Ok expr, context -> Ok (expr), context
        | Error err, context -> failure context err.Edits err.Cause context.Position context.Tokens
