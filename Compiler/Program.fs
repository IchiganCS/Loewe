open Loewe.Shared.CodeAnalysis.Lexing


let commentProgram =
    "
int main$() // test
{ //// stop
    // this should work 
    // emulated multiline comment
    int x = \"test\" //  notice spaces here ->    
;
    int works = \"3\";//3//s
}"

match lexString commentProgram with
| Ok tokens -> ()
| Error (skipped, toks) -> ()