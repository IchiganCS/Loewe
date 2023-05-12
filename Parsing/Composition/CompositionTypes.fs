namespace Loewe.Parsing.Composition.CompositionTypes

open Loewe.Definition.CodeConstructs


type ClassMember =
    | MethodMember of
        AccessModifier: AccessModifier *
        ReturnType: UnknownIdentifier *
        Name: string *
        Parameters: (UnknownIdentifier * string) list *
        CodeBlock: UnresolvedStatement list
    | AttributeMember of AccessModifier: AccessModifier * Type: UnknownIdentifier * Name: string

type TopLevelEntry =
    | ClassEntry of Name: string * Members: ClassMember list
    | FunctionEntry of
        ReturnType: UnknownIdentifier *
        Name: string *
        Parameters: (UnknownIdentifier * string) list *
        CodeBlock: UnresolvedStatement list

type FileContent = Namespace * Namespace list * TopLevelEntry list
