module Loewe.AST.Writer.Format

open Loewe.AST.Types

let formatParanthizedList (args : string list) =
    let mutable res = "("
    for arg in args do
        res <- res + (sprintf "%s, " arg)

    if args.Length = 0 then
        res + ")"
    else
        res[0..res.Length - 3] + ")"

let rec formatNamespace = function
    | Root -> ""
    | Child (parent, name) -> (formatNamespace parent) + name

let formatTopSymbol (symbol: TopSymbol) =
    (formatNamespace symbol.Namespace) + symbol.Name




let formatType (typ: Type) =
    match typ with
    | Primitive p -> p.TranslatedName
    | Class c -> (formatTopSymbol c.Symbol)

let formatQualifiedType (qTyp: QualifiedType) =
    match qTyp.Qualifier with
    | Owner -> (formatType qTyp.Type)
    | Ref | Ptr -> (formatType qTyp.Type) + "*"
    | ConstPtr | ConstRef -> "const " + (formatType qTyp.Type) + "*"

let formatFunctionSignature (func: Function) =
    sprintf "%s %s" (formatQualifiedType func.Signature.Return) (formatTopSymbol func.Symbol) +
    (func.Signature.Parameters
    |> List.map (fun arg -> sprintf "%s %s, " (formatQualifiedType arg.Type) arg.Name)
    |> formatParanthizedList)





let rec formatExpression (expression: Expression) =
    match expression with
    | Variable var -> var.Name
    | FunctionCall (func, exprList) -> 
        sprintf "%s" (formatTopSymbol func) +
        (exprList
        |> List.map formatExpression
        |> formatParanthizedList)
    | Constant str -> str

let formatVariableWithType (var: Variable) =
    (formatQualifiedType var.Type) + " " + var.Name




let rec formatBranches (checks: Expression list) (code: Statement list list) =
    if checks.Length < code.Length - 1 || checks.Length > code.Length then
        printfn "Don't understand this kind of branches"
        ""
    else

    let mutable res = sprintf "if (%s) %s" (formatExpression checks[0]) (formatCodeBlock code[0])

    for i in 1 .. checks.Length do
        res <- res + sprintf "elif (%s) %s" (formatExpression checks[i]) (formatCodeBlock code[i])

    // if there are not as many checks code statements, last branch is else
    if checks.Length = code.Length + 1 then
        res <- res + sprintf "else %s" (formatCodeBlock code[code.Length - 1])
    res

and formatWhile check code =
    sprintf "while (%s) %s" (formatExpression check) (formatCodeBlock code)

and formatFor init check incr code =
    sprintf "for (%s; %s; %s) %s" 
        (formatExpression init)
        (formatExpression check)
        (formatExpression incr)
        (formatCodeBlock code)

and formatStatement (st: Statement) : string =
    match st with
    | AssignValue (lhs, rhs) -> sprintf "%s = %s" (formatExpression lhs) (formatExpression rhs)
    | InitializeVariable (var, rhs) -> sprintf "%s = %s" (formatVariableWithType var) (formatExpression rhs)
    | Expression expr -> formatExpression expr
    | Branches (checkList, codesList) -> formatBranches checkList codesList
    | While (check, code) -> formatWhile check code
    | For (init, check, incr, code) -> formatFor init check incr code
    | Return (value) ->
        match value with
        | None -> "return"
        | Some expr -> "return " + (formatExpression expr)

    + ";"

and formatCodeBlock (code: Statement list) =
    "{\n" +

        (code 
        |> List.fold 
            (fun str -> fun stat -> str + (formatStatement stat) + "\n") 
            "")

    + "}"


let formatFunctionDeclaration func =
    (formatFunctionSignature func) + ";"

let formatFunctionDefinition (func: Function) =
    (formatFunctionSignature func) + " " +
    (formatCodeBlock func.Code)



let formatForwardClassDeclaration clas = 
    sprintf "typedef struct %s_s %s;" (formatTopSymbol clas.Symbol)

let formatInstanceMethodName (method: Method) =
    sprintf "CLASSMETHOD_%s_%s" (formatTopSymbol method.Symbol.Parent.Symbol) (method.Symbol.Name)
    
let formatStaticMethodName (method: Method) =
    sprintf "STATIC_CLASSMETHOD_%s_%s" (formatTopSymbol method.Symbol.Parent.Symbol) (method.Symbol.Name)

let formatStaticVariableName (field: Field) =
    sprintf "STATIC_VARIABLE_%s_%s" (formatTopSymbol field.Symbol.Parent.Symbol) (field.Name)



let getFullMethodParameters (method: Method) =
    {
        Name = "this"
        Type = {
            Type = Class method.Symbol.Parent
            Qualifier = Ptr
        }
    }::method.Signature.Parameters

let formatMethodSignature (method: Method) =
    sprintf "%s %s" (formatQualifiedType method.Signature.Return) (formatInstanceMethodName method) +
    ((getFullMethodParameters method)
    |> List.map (fun arg -> sprintf "%s %s, " (formatQualifiedType arg.Type) arg.Name)
    |> formatParanthizedList)

let formatInstanceMethodDeclaration (method: Method) =
    (formatMethodSignature method) + ";"

let formatInstanceMethodDefinition (method: Method) =
    (formatMethodSignature method) + (formatCodeBlock method.Code)

let formatClassDefinitionBody (clas: Class) =
    let mutable res = "{\n"

    for field in clas.InstanceFields do
        res <- res + (sprintf "%s %s;\n" (formatQualifiedType field.Type) field.Name)

    res + "}"

let formatClassDefinition clas =
    sprintf "typedef struct %s_s %s %s" (formatTopSymbol clas.Symbol) (formatClassDefinitionBody clas) (formatTopSymbol clas.Symbol)
