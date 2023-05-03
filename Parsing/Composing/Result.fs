namespace Loewe.Parsing.Composing.Result
open Loewe.Parsing.Composing.Error
open Loewe.Parsing.Tokenizing

/// A result of composing. If the process was completed successfully, the list of remaining tokens and the identified object is returned.
/// In case of a failure, a backtrace is provided.
type Result<'a> =
    | Success of Token list * 'a
    | Failure of ErrorTrace