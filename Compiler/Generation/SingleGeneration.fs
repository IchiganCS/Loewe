module Loewe.Generation.SingleGeneration

open Loewe.Definition.TokenTypes
open Loewe.Definition.CodeConstructs
open LLVMSharp

type LLVMBuilderContext = {
    SymbolTable: ResolvedSymbol Set
    Context: LLVMContextRef
    Builder: LLVMBuilderRef
    Module: LLVMModuleRef
    NamedValues: Map<string, LLVMValueRef>
}

let generateLLVMLiteral literal =
    match literal with
    | Float f -> LLVM.ConstReal(LLVM.FloatType(), f)
    | Double d -> LLVM.ConstReal(LLVM.DoubleType(), d)
    | Int i -> LLVM.ConstInt(LLVM.Int32Type(), uint64 i, false)
    | Long i -> LLVM.ConstInt(LLVM.Int64Type(), uint64 i, false)
    | UnsignedInt i -> LLVM.ConstInt(LLVM.Int64Type(), uint64 i, false)
    | UnsignedLong i -> LLVM.ConstInt(LLVM.Int64Type(), uint64 i, false)
    | Bool true -> LLVM.ConstInt(LLVM.Int1Type(), uint64 -1, false)
    | Bool false -> LLVM.ConstInt(LLVM.Int1Type(), uint64 0, false)
    | String s -> LLVM.ConstString(s, s |> String.length |> uint32, false)
    

let rec generateLLVMExpression context expression =
    match expression with
    | BinaryOperation (expr1, op, expr2) ->
        let llvmExpr1 = generateLLVMExpression context expr1
        let llvmExpr2 = generateLLVMExpression context expr2

        match op with
        | Addition -> 
            LLVM.BuildAdd(context.Builder, llvmExpr1, llvmExpr2, "addint")
        | Subtraction ->
            LLVM.BuildSub(context.Builder, llvmExpr1, llvmExpr2, "subint")

        | _ -> failwith "not implmemented"
    | Literal l -> generateLLVMLiteral l
    | _ -> failwith "not implmemented"
    


let generateLLVMStatement symbolTable statement =
    failwith "not implemented"