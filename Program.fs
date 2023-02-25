open Loewe.Parsing.Tokenizing.Main
open System.IO
open Loewe.Parsing.Tokenizing.Types
open Loewe.Parsing.Tree.Unresolved.Main

let testProgram = "

namespace Test;
open Test.Asgasg;

class Holter {
    public int diPolter;

}

asgpih Linchen(istToll name, sdg aegas) {
    int x = asd + asdgasd * etst + 124;
}
"

match tokenize testProgram with
| Failure _ -> printf "STOP"
| Success tokens ->


match tokens |>
List.map (fun pt -> pt.Token) |>
buildUnresolvedFromFile with
| Some members -> raise (System.Exception "Es hat geklppat")
| None -> printfn "ALLES BRENNT"