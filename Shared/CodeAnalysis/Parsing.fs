module Loewe.Shared.CodeAnalysis.Parsing


module Tree =
    open BuiltIn
    open Token
    open Position

    type FileContent = {
        ModuleDeclaration: ModuleDeclaration
        UsingStatements: UsingStatement list

        ClassDefinitions: ClassDefinition list
        FunctionDefinitions: FunctionDefinition list
    }

open Position
open Token
open Tree

type Part =
    | Identifier
    | Expression
    | Class
    | Function
    | Method
    | Attribute
    | ParameterList
    | SingleParameter


type EditAction =
    | Remove of Token
    | Insert of Part

type Error = {
    Edits: (EditAction * LinePosition) list
    Aim: Part list
}

type Context = {
    Errors: Error list
    Parts: Part list
    Tokens: PositionedToken list
}


type IntermediateResult<'a> = Result<'a, 'a> * Context
type IntermediateFun<'a> = Context -> IntermediateResult<'a>
type ParsingResult<'a> = 'a * Error list
type ParsingFun<'a> = PositionedToken list -> ParsingResult<'a>
type FileResult = ParsingResult<FileContent>




