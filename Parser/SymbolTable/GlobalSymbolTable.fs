module Loewe.Parser.SymbolTable
open SymbolTypes

type NewSymbolHandlerResult =
    | SymbolFound
    | SymbolNotFound

type NewSymbolHandler = GlobalSymbol -> NewSymbolHandlerResult

/// Contains a set of defined symbols as well as a list of functions. Those functions symbolize a missing entry in the table, 
/// e.g. a symbol was required but not yet added. 
type GlobalSymbolTable = GlobalSymbol Set 

