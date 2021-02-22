#include "Project.h"

// expansion
#define ASMCALL_DERIVE_0(name)                                    \
  SymVal *SEOpvAsm_##name::derive(SymExec &sym)

#define ASMCALL_DERIVE_1(name, arg0)                              \
  SymVal *SEOpvAsm_##name::derive(SymExec &sym,                   \
      SymVal *arg0)

#define ASMCALL_DERIVE_2(name, arg0, arg1)                        \
  SymVal *SEOpvAsm_##name::derive(SymExec &sym,                   \
      SymVal *arg0, SymVal *arg1)

#define ASMCALL_DERIVE_3(name, arg0, arg1, arg2)                  \
  SymVal *SEOpvAsm_##name::derive(SymExec &sym,                   \
      SymVal *arg0, SymVal *arg1, SymVal *arg2)

// helpers
#define CHECK_INST                                                \
  assert(isa<CallInst>(inst));                                    \
  CallInst *op = cast<CallInst>(inst);

#define GET_ORACLE                                                \
  ModuleOracle &oracle = sym.getOracle();

#define INIT_EXPR                                                 \
  SymExpr expr;                                                   \
  SymCond cond;

#define FINI_EXPR                                                 \
  return new SymVal(expr, cond);

#define IGNORED                                                   \
  INIT_EXPR;                                                      \
  FINI_EXPR;

// asm call symbolization routines

// fetch
ASMCALL_DERIVE_2(__get_user_n, src, size) {
    INIT_EXPR;

    CHECK_INST;

    GET_ORACLE;
    Type* ty = op->getType();
    if (StructType * stcTy = dyn_cast<StructType>(ty)) {
        //expr.expr = sym.createSymStruct(stcTy);
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(stcTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (IntegerType * intTy = dyn_cast<IntegerType>(ty)) {
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(intTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (isa<PointerType>(ty)) {
        assert(false && "ERR: pointer type variable returned by __get_user_\n");
    }
    else {
        expr.expr = sym.createConstNull();
        errs() << "Vital Err:unhandled return value of __get_user_:" << *op << '\n';
        DUMP.typedValue(op);
        llvm_unreachable("unhandled return value of __get_user_\n");
    }

    sym.addAssert(Z3_mk_bvugt(sym.getContext(),
                              size->getExpr().expr, sym.createConstPointer(0)));
    sym.addAssert(Z3_mk_bvult(sym.getContext(),
                              size->getExpr().expr, sym.createConstPointer(HEAP_SLOT_LEN)));

    FINI_EXPR;
}

ASMCALL_DERIVE_0(Unhandled0) {
    INIT_EXPR;

    CHECK_INST;

    GET_ORACLE;
    Type* ty = op->getType();
    if (StructType * stcTy = dyn_cast<StructType>(ty)) {
        //expr.expr = sym.createSymStruct(stcTy);
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(stcTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (IntegerType * intTy = dyn_cast<IntegerType>(ty)) {
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(intTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (isa<PointerType>(ty)) {
        expr.expr = sym.createPtrHeap();
    }
    else if(ty->isVoidTy()){
        IGNORED;
    }
    else {
        expr.expr = sym.createConstNull();
        errs() << "Vital Err:unhandled return value of unahandled asm 0:" << *op << '\n';
        DUMP.typedValue(op);
        llvm_unreachable("unhandled return value of unahandled asm 0\n");
    }
    FINI_EXPR;
}

ASMCALL_DERIVE_1(Unhandled1, arg0) {
    INIT_EXPR;

    CHECK_INST;

    GET_ORACLE;
    Type* ty = op->getType();
    if (StructType * stcTy = dyn_cast<StructType>(ty)) {
        //expr.expr = sym.createSymStruct(stcTy);
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(stcTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (IntegerType * intTy = dyn_cast<IntegerType>(ty)) {
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(intTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (isa<PointerType>(ty)) {
        expr.expr = sym.createPtrHeap();
    }
    else if(ty->isVoidTy()){
        IGNORED;
    }
    else {
        expr.expr = sym.createConstNull();
        errs() << "Vital Err:unhandled return value of unahandled asm 1:" << *op << '\n';
        DUMP.typedValue(op);
        llvm_unreachable("unhandled return value of unahandled asm 1\n");
    }
    FINI_EXPR;
}

ASMCALL_DERIVE_2(Unhandled2, arg0, arg1) {
    INIT_EXPR;

    CHECK_INST;

    GET_ORACLE;
    Type* ty = op->getType();
    if (StructType * stcTy = dyn_cast<StructType>(ty)) {
        //expr.expr = sym.createSymStruct(stcTy);
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(stcTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (IntegerType * intTy = dyn_cast<IntegerType>(ty)) {
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(intTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (isa<PointerType>(ty)) {
        expr.expr = sym.createPtrHeap();
    }
    else if(ty->isVoidTy()){
        IGNORED;
    }
    else {
        expr.expr = sym.createConstNull();
        errs() << "Vital Err:unhandled return value of unahandled asm 2:" << *op << '\n';
        DUMP.typedValue(op);
        llvm_unreachable("unhandled return value of unahandled asm 2\n");
    }
    FINI_EXPR;
}

ASMCALL_DERIVE_3(Unhandled3, arg0, arg1,arg2) {
    INIT_EXPR;

    CHECK_INST;

    GET_ORACLE;
    Type* ty = op->getType();
    if (StructType * stcTy = dyn_cast<StructType>(ty)) {
        //expr.expr = sym.createSymStruct(stcTy);
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(stcTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (IntegerType * intTy = dyn_cast<IntegerType>(ty)) {
        expr.expr = sym.createVarMachInt(oracle.getTypeWidth(intTy));
        //expr.expVoidPointerTy = individual;
    }
    else if (isa<PointerType>(ty)) {
        expr.expr = sym.createPtrHeap();
    }
    else if(ty->isVoidTy()){
        IGNORED;
    }
    else {
        expr.expr = sym.createConstNull();
        errs() << "Vital Err:unhandled return value of unahandled asm 3:" << *op << '\n';
        DUMP.typedValue(op);
        llvm_unreachable("unhandled return value of unahandled asm 3\n");
    }
    FINI_EXPR;
}