/// A module containing all necessary operations and definitions for a symbol table in Loewe.
/// Such a symbol table contains fully qualified type signatures containing all information for 
/// functions and classes (and their members), thus globally available symbols.
/// Local symbols are handled only locally.
namespace Loewe.Parsing.SymbolTable


/// This module provides types for a symbol table.
module SymbolTypes =
    open Loewe.Parsing.Types

    type UnresolvedCode = Loewe.Parsing.Composition.CompositionTypes.Statement list

    type Attribute = {
        AccessModifier: AccessModifier
        Name: string
        Type: Type
        Owner: ClassStump
    }

    and Method = {
        AccessModifier: AccessModifier
        Name: string
        Parameters: (Type * string) list
        Return: Type option
        Code: UnresolvedCode
        Owner: ClassStump
    }

    and Type =
        | PrimitiveType of string
        | ClassType of ClassStump

    /// Classes are only stumps since we would have to make their attribute and method lists mutable.
    /// They could not be created otherwise.
    and ClassStump = {
        Namespace: Namespace
        Name: string
    }

    and Function = {
        Namespace: Namespace
        Name: string
        Parameters: (Type * string) list
        Return: Type option
        Code: UnresolvedCode
    }

    type GlobalSymbol = 
        | ClassSymbol of ClassStump
        | FunctionSymbol of Function
        | AttributeSymbol of Attribute
        | MethodSymbol of Method

    type SymbolTable = GlobalSymbol Set