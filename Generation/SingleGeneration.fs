module Loewe.Generation.SingleGeneration

open LLVMSharp
open Loewe.Definition.TokenTypes
open Loewe.Definition.CodeConstructs

type LLVMBuilderContext = {
    SymbolTable: ResolvedSymbol Set
    Context: LLVMContextRef
    Builder: LLVMBuilderRef
    Module: LLVMModuleRef
    NamedValues: Map<string, LLVMValueRef>
}

let generateLLVMLiteral context literal =
    match literal with
    | Float f ->LLVM.ConstReal(LLVM.FloatType(), f)
    | Double d -> LLVM.ConstReal(LLVM.DoubleType(), d)
    | Int i -> LLVM.ConstInt(LLVM.Int32Type(), uint64 i, LLVMBool(-1)) //todo
    | Long i -> LLVM.ConstInt(LLVM.Int32Type(), uint64 i, LLVMBool(-1)) //todo
    | UnsignedInt i -> LLVM.ConstInt(LLVM.Int32Type(), uint64 i, LLVMBool(0)) //todo
    | UnsignedLong i -> LLVM.ConstInt(LLVM.Int32Type(), i, LLVMBool(0)) //todo
    | _ -> failwith "not implmemented"
    

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
    | Literal l -> generateLLVMLiteral context l
    | _ -> failwith "not implmemented"
    


let generateLLVMStatement symbolTable statement =
    failwith "not implemented"