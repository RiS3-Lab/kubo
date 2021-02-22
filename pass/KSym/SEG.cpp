#include "Project.h"

/* [disabled], to be enabeld after handling page_address
#define KSYM_CONFIG_FORCE_ASSERT
*/

// build linkage in SEOpv
void SEOpv::setHost(SENode *h) {
  host = h;

  for(SENode *v : vals){
    v->addUsr(h);
    h->addDep(v);
  }
}
void SEOpv::setHost_host(SENode *h) {
  host = h;
}

// an SEOpv is ready when all its values are symbolized
bool SEOpv::ready(SymExec &sym) {
  for(SENode *v : vals) {
      // errs() << "before seopv get symbol\n";
      //errs()<<"opval:"<<*v->getVal()<<'\n';
      SymVar *svar = v->getSymbol(sym);
      if(svar == nullptr){
          return false;
      }
      // errs() << "after seopv get symbol\n";

      SymVal *sval = svar->getSingleVal();
      if(!sval->ready()){
          return false;
      }
  }
  // errs() << "seopv ready\n";

  return true;
}

SymVar *SENodeLeaf::getSymbol(SymExec &sym) {
  bool exist;
  SymVar *var = sym.getOrCreateVar(this, exist);

  if(!exist){
    symbolize(sym, var);
  }

  return var;
}

SymVar *SENodeVar::getSymbol(SymExec &sym) {
  bool exist;
  SymVar *var = sym.getOrCreateVar(this, exist);

  if(!exist){
    symbolize(sym, var);
  }

  return var;
}

SymVar *SENodeInst::getSymbol(SymExec &sym) {
    // TODO to be removed after symbolize for all subtypes are done
    // this also asserts on non-recursiveness between cur and preds
    if(isa<SENodeSpecialCall>(this))
        return nullptr;
    for(SEOpv *i : opval) {
        bool readyness = false;
        if(sym.readyCache.find(i) != sym.readyCache.end()){
            if(sym.readyCache[i] == false){
                readyness = i->ready(sym);
                sym.readyCache[i] = readyness;
            }else{
                readyness = true;
            }
        }else{
            readyness = i->ready(sym);
            sym.readyCache[i] = readyness;
        }
        if(!readyness){
            return nullptr;
        }
    }
  // end of TODO

  bool exist;
  SymVar *var = sym.getOrCreateVar(this, exist);

  if(!exist){
      // errs() << "before nodeinst symbolize\n";
    symbolize(sym, var);
    // errs() << "after nodeinst symbolize\n";
  }

  return var;
}

SymVar *SENodeUnknown::getSymbol(SymExec &sym) {
#if defined(KSYM_DEBUG) && defined(KSYM_DEBUG_CASE)
  if(CallInst *ci = dyn_cast<CallInst>(val)){
    if(ci->isInlineAsm()){
      // inline asm case, do nothing
    }

    else {
      Function *tar = ci->getCalledFunction();
      if(tar == nullptr){
        // indirect call case, do nothing
      } else {
        // direct call
        string fn = tar->getName().str();
        if(tar->isIntrinsic()){
          Helper::convertDotInName(fn);
        }

        errs()
          << "getSymbol unknown: "
          << ci->getParent()->getParent()->getName() << "::"
          << fn << "\n";
      }
    }
  }
#endif

  return nullptr;
}

// generic symbolize - opv
void SEOpv0::symbolize(SymExec &sym, SymVar *var) {
  var->add(derive(sym));
}

void SEOpv1::symbolize(SymExec &sym, SymVar *var) {
  SENode *v0 = at(0);
  SymVar *r0 = v0->getSymbol(sym);

  SymVar::iterator i0 = r0->begin(), i0e = r0->end();
  for(; i0 != i0e; ++i0){
    var->add(derive(sym, *i0));
  }
}

void SEOpv2::symbolize(SymExec &sym, SymVar *var) {
  SENode *v0 = at(0);
  SymVar *r0 = v0->getSymbol(sym);

  SENode *v1 = at(1);
  SymVar *r1 = v1->getSymbol(sym);

  SymVar::iterator i0 = r0->begin(), i0e = r0->end();
  for(; i0 != i0e; ++i0){
    SymVar::iterator i1 = r1->begin(), i1e = r1->end();
    for(; i1 != i1e; ++i1){
      var->add(derive(sym, *i0, *i1));
    }
  }
}

void SEOpv3::symbolize(SymExec &sym, SymVar *var) {
  SENode *v0 = at(0);
  SymVar *r0 = v0->getSymbol(sym);

  SENode *v1 = at(1);
  SymVar *r1 = v1->getSymbol(sym);

  SENode *v2 = at(2);
  SymVar *r2 = v2->getSymbol(sym);

  SymVar::iterator i0 = r0->begin(), i0e = r0->end();
  for(; i0 != i0e; ++i0){
    SymVar::iterator i1 = r1->begin(), i1e = r1->end();
    for(; i1 != i1e; ++i1){
      SymVar::iterator i2 = r2->begin(), i2e = r2->end();
      for(; i2 != i2e; ++i2){
        var->add(derive(sym, *i0, *i1, *i2));
      }
    }
  }
}
void SEOpv4::symbolize(SymExec &sym, SymVar *var) {
    SENode *v0 = at(0);
    SymVar *r0 = v0->getSymbol(sym);

    SENode *v1 = at(1);
    SymVar *r1 = v1->getSymbol(sym);

    SENode *v2 = at(2);
    SymVar *r2 = v2->getSymbol(sym);

    SENode *v3 = at(3);
    SymVar *r3 = v3->getSymbol(sym);

    SymVar::iterator i0 = r0->begin(), i0e = r0->end();
    for(; i0 != i0e; ++i0){
        SymVar::iterator i1 = r1->begin(), i1e = r1->end();
        for(; i1 != i1e; ++i1){
            SymVar::iterator i2 = r2->begin(), i2e = r2->end();
            for(; i2 != i2e; ++i2){
                SymVar::iterator i3 = r3->begin(), i3e=r3->end();
                for(;i3!=i3e;++i3)
                    var->add(derive(sym, *i0, *i1, *i2,*i3));
            }
        }
    }
}

// concrete symbolize
#define CHECK_NODE(val_type)                                          \
  assert(isa<val_type>(val));                                         \
  val_type *op = cast<val_type>(val);

#define CHECK_INST(val_type)                                          \
  assert(isa<val_type>(inst));                                        \
  val_type *op = cast<val_type>(inst);

#define GET_ORACLE                                                    \
  ModuleOracle &oracle = sym.getOracle();

#define INIT_EXPR                                                     \
  SymExpr expr;                                                       \
  SymCond cond;

#define FINI_EXPR                                                     \
  return new SymVal(expr, cond);

SymVal *SENodeCInt::derive(SymExec &sym) {
  INIT_EXPR;

  CHECK_NODE(ConstantInt);

  expr.expr = sym.createConstMachInt(op);

  FINI_EXPR;
}

SymVal *SENodeCFloat::derive(SymExec &sym) {
    INIT_EXPR;
    GET_ORACLE;

    CHECK_NODE(ConstantFP);

    expr.expr = sym.createConstMachFloat(op);

    FINI_EXPR;
}

SymVal *SENodeCNull::derive(SymExec &sym) {
  INIT_EXPR;

  CHECK_NODE(ConstantPointerNull);

  expr.expr = sym.createConstNull();

  FINI_EXPR;
}

SymVal *SENodeCAgg::derive(SymExec &sym) {
    INIT_EXPR;
    GET_ORACLE;

    CHECK_NODE(ConstantAggregate);

    expr.expr = sym.createConstMachStruct(op);

    FINI_EXPR;
}

SymVal *SENodeCAggZero::derive(SymExec &sym) {
    INIT_EXPR;
    GET_ORACLE;

    CHECK_NODE(ConstantAggregateZero);

    expr.expr = sym.createConstMachStructZero(op);

    FINI_EXPR;
}

SymVal *SENodeCDataSeq::derive(SymExec &sym) {
    INIT_EXPR;
    GET_ORACLE;

    CHECK_NODE(ConstantDataSequential);
    expr.expr = sym.createConstMachDataSeq(op);

    FINI_EXPR;
}

SymVal *SENodeCBlkAddr::derive(SymExec &sym) {
    INIT_EXPR;
    GET_ORACLE;

    CHECK_NODE(BlockAddress);
    expr.expr = sym.createConstPointer((uintptr_t)0);

    FINI_EXPR;
}

// support undef node since UBSAN intrinsic cleaner
// will emit those in insertvalueinst
SymVal *SENodeUndef::derive(SymExec &sym) {
    INIT_EXPR;

    CHECK_NODE(UndefValue);
    GET_ORACLE;

    Type *ty = op->getType();
    uint64_t bits = oracle.getTypeWidth(ty);
    // errs()<<"total size: "<<bits<<"\n"<<*ty<<"\n";
    // errs()<<oracle.getTypeOffset(ty,0)<<"\n";
    // errs()<<oracle.getTypeOffset(ty,1)<<"\n";
    expr.expr = sym.createVarMachInt(bits);

    FINI_EXPR;
}
SymVal *SENodeParam::derive(SymExec &sym) {
  INIT_EXPR;

  CHECK_NODE(Argument);
  GET_ORACLE;

  // very conservative check on whether this param is a pointer or value
  bool ptr = false;
  Type *ty = op->getType();
  if(ty->isPointerTy()){
    ptr = true;
  }

  else if(oracle.isReintPointerType(ty)){
    for(SENode *u : usrs){
      SENodeInst *ui = dyn_cast<SENodeInst>(u);
      assert(ui != nullptr);
      if(isa<SENodeSpecialCall>(ui))
        continue;
      if(isa<SEOpvCast2Ptr>(ui->getSingleOpv())){
        ptr = true;
        break;
      }
    }
  }

  if(ptr){
    expr.expr = sym.createPtrExtern();
    //expr.expVoidPointerTy = pointToMemory;
  }
  else if(ty->isFloatingPointTy()) {
      expr.expr = sym.createVarMachFloat(oracle.getTypeWidth(ty));
      //expr.expVoidPointerTy = individual;
  }
  else {
    expr.expr = sym.createVarMachInt(ty->getIntegerBitWidth());
    //expr.expVoidPointerTy = individual;
  }

  FINI_EXPR;
}

SymVal *SENodeGlobal::derive(SymExec &sym) {
  INIT_EXPR;

  CHECK_NODE(GlobalVariable);

  // only pointer type is allowed for global variables
  Type *ty = op->getType();
  assert(ty->isPointerTy());


  expr.expr = sym.createPtrExtern();
  FINI_EXPR;
}

SymVal *SENodeLocal::derive(SymExec &sym) {
  INIT_EXPR;

  CHECK_NODE(AllocaInst);

  // only pointer type is allowed for local variables
  Type *ty = op->getType();
  assert(ty->isPointerTy());

  Optional<uint64_t> allocBits = op->getAllocationSizeInBits(sym.getOracle().getDataLayout());
  assert(allocBits.hasValue());
  expr.expr = sym.createPtrStack();
  //expr.expVoidPointerTy = pointToMemory;
  uint64_t allocBytes = allocBits.getValue() / sym.getOracle().getBits();
  expr.allocStatus.upperBound = allocBytes;
  expr.allocStatus.lowerBound = 0;
  FINI_EXPR;
}

SymVal *SEOpvCastTrunc::derive(SymExec &sym,
    SymVal *orig) {

  INIT_EXPR;

  CHECK_INST(CastInst);
  GET_ORACLE;

  expr.expr = sym.castMachIntTrunc(orig->getExpr().expr,
      oracle.getTypeWidth(op->getType()));
  expr.allocStatus = orig->getExpr().allocStatus;
  //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
  FINI_EXPR;
}

SymVal *SEOpvCastFPTrunc::derive(SymExec &sym,
                               SymVal *orig) {

    INIT_EXPR;

    CHECK_INST(CastInst);
    GET_ORACLE;

    expr.expr = sym.castFPConvert(orig->getExpr().expr,
                                  oracle.getTypeWidth(op->getType()));
    expr.allocStatus = orig->getExpr().allocStatus;
    //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
    FINI_EXPR;
}

SymVal *SEOpvCastZExt::derive(SymExec &sym,
    SymVal *orig) {

  INIT_EXPR;

  CHECK_INST(CastInst);
  GET_ORACLE;

  expr.expr = sym.castMachIntZExt(orig->getExpr().expr,
      oracle.getTypeWidth(op->getType()));
  expr.allocStatus = orig->getExpr().allocStatus;
  //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
  FINI_EXPR;
}

SymVal *SEOpvCastSExt::derive(SymExec &sym,
    SymVal *orig) {

  INIT_EXPR;

  CHECK_INST(CastInst);
  GET_ORACLE;

  expr.expr = sym.castMachIntSExt(orig->getExpr().expr, oracle.getTypeWidth(op->getType()));
  expr.allocStatus = orig->getExpr().allocStatus;
  //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
  FINI_EXPR;
}

SymVal *SEOpvCastFPExt::derive(SymExec &sym,
                              SymVal *orig) {

    INIT_EXPR;

    CHECK_INST(CastInst);
    GET_ORACLE;

    expr.expr = sym.castFPConvert(orig->getExpr().expr,
                                  oracle.getTypeWidth(op->getType()));
    expr.allocStatus = orig->getExpr().allocStatus;
    //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
    FINI_EXPR;
}

SymVal *SEOpvCast2Ptr::derive(SymExec &sym,
    SymVal *orig) {

  INIT_EXPR;

  CHECK_INST(CastInst);

  expr.expr = orig->getExpr().expr;
  expr.allocStatus = orig->getExpr().allocStatus;
  //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
  FINI_EXPR;
}

SymVal *SEOpvCast2Int::derive(SymExec &sym,
    SymVal *orig) {

  INIT_EXPR;

  CHECK_INST(CastInst);

  expr.expr = orig->getExpr().expr;
  expr.allocStatus = orig->getExpr().allocStatus;
  //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
  FINI_EXPR;
}

SymVal *SEOpvCastType::derive(SymExec &sym,
    SymVal *orig) {

  INIT_EXPR;

  CHECK_INST(CastInst);
  GET_ORACLE;

  // if it is float point sort, change it to bv
  expr.expr = sym.prepareBitCast(orig->getExpr().expr,
                                 oracle.getTypeWidth(op->getType()));
  expr.allocStatus = orig->getExpr().allocStatus;
  //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
  FINI_EXPR;
}

SymVal *SEOpvCastFPToSI::derive(SymExec &sym,
                                SymVal *orig) {

    INIT_EXPR;

    CHECK_INST(CastInst);
    GET_ORACLE;

    expr.expr = sym.castFPToMachInt(orig->getExpr().expr,
                                    oracle.getTypeWidth(op->getType()));
    expr.allocStatus = orig->getExpr().allocStatus;
    FINI_EXPR;
}

SymVal *SEOpvCastFPToUI::derive(SymExec &sym,
                                SymVal *orig) {

    INIT_EXPR;

    CHECK_INST(CastInst);
    GET_ORACLE;

    expr.expr = sym.castFPToMachUInt(orig->getExpr().expr,
                                     oracle.getTypeWidth(op->getType()));
    expr.allocStatus = orig->getExpr().allocStatus;
    //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
    FINI_EXPR;
}

SymVal *SEOpvCastSIToFP::derive(SymExec &sym,
                                SymVal *orig) {

    INIT_EXPR;

    CHECK_INST(CastInst);
    GET_ORACLE;

    expr.expr = sym.castMachIntToFP(orig->getExpr().expr,
                                    oracle.getTypeWidth(op->getType()));
    expr.allocStatus = orig->getExpr().allocStatus;
    //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
    FINI_EXPR;
}

SymVal *SEOpvCastUIToFP::derive(SymExec &sym,
                                SymVal *orig) {

    INIT_EXPR;

    CHECK_INST(CastInst);
    GET_ORACLE;

    expr.expr = sym.castMachUIntToFP(orig->getExpr().expr,
                                     oracle.getTypeWidth(op->getType()));
    expr.allocStatus = orig->getExpr().allocStatus;
    //expr.expVoidPointerTy = orig->getExpr().expVoidPointerTy;
    FINI_EXPR;
}

SymVal *SEOpvCalcAdd::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvadd(sym.getContext(), lexp, rexp);

  // overflow/underflow assertions
  if(op->hasNoSignedWrap()){
    sym.addAssert(Z3_mk_bvadd_no_overflow(sym.getContext(), lexp, rexp, true));
  }

  if(op->hasNoUnsignedWrap()){
    sym.addAssert(Z3_mk_bvadd_no_overflow(sym.getContext(), lexp, rexp, false));
  }

  sym.addAssert(Z3_mk_bvadd_no_underflow(sym.getContext(), lexp, rexp));

  FINI_EXPR;
}

SymVal *SEOpvCalcFAdd::derive(SymExec &sym,
                              SymVal *lhs, SymVal *rhs) {

    INIT_EXPR;

    CHECK_INST(BinaryOperator);

    // adjust sizes for math ops
    Z3_ast lexp = lhs->getExpr().expr;
    Z3_ast rexp = rhs->getExpr().expr;

    sym.prepareFCalcOperation(
        op->getOperand(0)->getType(), lexp,
        op->getOperand(1)->getType(), rexp);

    Z3_ast rm = Z3_mk_fpa_rtz(sym.getContext());
    expr.expr = Z3_mk_fpa_add(sym.getContext(), rm, lexp, rexp);

    FINI_EXPR;
}

SymVal *SEOpvCalcSub::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvsub(sym.getContext(), lexp, rexp);

  // overflow/underflow assertions
  if(op->hasNoSignedWrap()){
    sym.addAssert(Z3_mk_bvsub_no_underflow(sym.getContext(), lexp, rexp, true));
  }

  if(op->hasNoUnsignedWrap()){
    sym.addAssert(Z3_mk_bvsub_no_underflow(sym.getContext(), lexp, rexp, false));
  }

  sym.addAssert(Z3_mk_bvsub_no_overflow(sym.getContext(), lexp, rexp));

  FINI_EXPR;
}

SymVal *SEOpvCalcFSub::derive(SymExec &sym,
                              SymVal *lhs, SymVal *rhs) {

    INIT_EXPR;

    CHECK_INST(BinaryOperator);

    // adjust sizes for math ops
    Z3_ast lexp = lhs->getExpr().expr;
    Z3_ast rexp = rhs->getExpr().expr;

    sym.prepareFCalcOperation(
        op->getOperand(0)->getType(), lexp,
        op->getOperand(1)->getType(), rexp);

    Z3_ast rm = Z3_mk_fpa_rtz(sym.getContext());
    expr.expr = Z3_mk_fpa_sub(sym.getContext(), rm, lexp, rexp);

    FINI_EXPR;
}

SymVal *SEOpvCalcMul::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvmul(sym.getContext(), lexp, rexp);

  // overflow/underflow assertions
  if(op->hasNoSignedWrap()){
    sym.addAssert(Z3_mk_bvmul_no_overflow(sym.getContext(), lexp, rexp, true));
  }

  if(op->hasNoUnsignedWrap()){
    sym.addAssert(Z3_mk_bvmul_no_overflow(sym.getContext(), lexp, rexp, false));
  }

  sym.addAssert(Z3_mk_bvmul_no_underflow(sym.getContext(), lexp, rexp));

  FINI_EXPR;
}

SymVal *SEOpvCalcFMul::derive(SymExec &sym,
                              SymVal *lhs, SymVal *rhs) {

    INIT_EXPR;

    CHECK_INST(BinaryOperator);

    // adjust sizes for math ops
    Z3_ast lexp = lhs->getExpr().expr;
    Z3_ast rexp = rhs->getExpr().expr;

    sym.prepareFCalcOperation(
        op->getOperand(0)->getType(), lexp,
        op->getOperand(1)->getType(), rexp);

    Z3_ast rm = Z3_mk_fpa_rtz(sym.getContext());
    expr.expr = Z3_mk_fpa_mul(sym.getContext(), rm, lexp, rexp);

    FINI_EXPR;
}

SymVal *SEOpvCalcUDiv::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvudiv(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvCalcSDiv::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvsdiv(sym.getContext(), lexp, rexp);

  // overflow/underflow assertions
  sym.addAssert(Z3_mk_bvsdiv_no_overflow(sym.getContext(), lexp, rexp));

  FINI_EXPR;
}

SymVal *SEOpvCalcFDiv::derive(SymExec &sym,
                              SymVal *lhs, SymVal *rhs) {

    INIT_EXPR;

    CHECK_INST(BinaryOperator);

    // adjust sizes for math ops
    Z3_ast lexp = lhs->getExpr().expr;
    Z3_ast rexp = rhs->getExpr().expr;

    sym.prepareFCalcOperation(
        op->getOperand(0)->getType(), lexp,
        op->getOperand(1)->getType(), rexp);

    Z3_ast rm = Z3_mk_fpa_rtz(sym.getContext());
    expr.expr = Z3_mk_fpa_div(sym.getContext(), rm, lexp, rexp);

    FINI_EXPR;
}

SymVal *SEOpvCalcURem::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvurem(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvCalcSRem::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvsrem(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvCalcFRem::derive(SymExec &sym,
                              SymVal *lhs, SymVal *rhs) {

    INIT_EXPR;

    CHECK_INST(BinaryOperator);

    // adjust sizes for math ops
    Z3_ast lexp = lhs->getExpr().expr;
    Z3_ast rexp = rhs->getExpr().expr;

    sym.prepareFCalcOperation(
        op->getOperand(0)->getType(), lexp,
        op->getOperand(1)->getType(), rexp);

    expr.expr = Z3_mk_fpa_rem(sym.getContext(), lexp, rexp);

    FINI_EXPR;
}

SymVal *SEOpvCalcShl::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvshl(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvCalcLShr::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvlshr(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvCalcAShr::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvashr(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvCalcAnd::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvand(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvCalcOr::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvor(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvCalcXor::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(BinaryOperator);

  // adjust sizes for math ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCalcOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = Z3_mk_bvxor(sym.getContext(), lexp, rexp);

  FINI_EXPR;
}

SymVal *SEOpvICmpEq::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(ICmpInst);
  GET_ORACLE;

  // adjust sizes for cmp ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCmpOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = sym.castBoolToMachInt(
      Z3_mk_eq(sym.getContext(), lexp, rexp),
      oracle.getTypeWidth(op->getType()));

  FINI_EXPR;
}

SymVal *SEOpvFCmpEq::derive(SymExec &sym,
                            SymVal *lhs, SymVal *rhs) {

    INIT_EXPR;

    CHECK_INST(FCmpInst);
    GET_ORACLE;

    // adjust sizes for cmp ops
    Z3_ast lexp = lhs->getExpr().expr;
    Z3_ast rexp = rhs->getExpr().expr;

    sym.prepareFCmpOperation(
        op->getOperand(0)->getType(), lexp,
        op->getOperand(1)->getType(), rexp);

    expr.expr = sym.castBoolToMachInt(
        Z3_mk_fpa_eq(sym.getContext(), lexp, rexp),
        oracle.getTypeWidth(op->getType()));

    FINI_EXPR;
}

SymVal *SEOpvICmpNe::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(ICmpInst);
  GET_ORACLE;

  // adjust sizes for cmp ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  sym.prepareCmpOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  expr.expr = sym.castBoolToMachIntInversed(
      Z3_mk_eq(sym.getContext(), lexp, rexp),
      oracle.getTypeWidth(op->getType()));

  FINI_EXPR;
}

SymVal *SEOpvFCmpNe::derive(SymExec &sym,
                            SymVal *lhs, SymVal *rhs) {

    INIT_EXPR;

    CHECK_INST(FCmpInst);
    GET_ORACLE;

    // adjust sizes for cmp ops
    Z3_ast lexp = lhs->getExpr().expr;
    Z3_ast rexp = rhs->getExpr().expr;

    sym.prepareFCmpOperation(
        op->getOperand(0)->getType(), lexp,
        op->getOperand(1)->getType(), rexp);

    expr.expr = sym.castBoolToMachIntInversed(
        Z3_mk_fpa_eq(sym.getContext(), lexp, rexp),
        oracle.getTypeWidth(op->getType()));

    FINI_EXPR;
}

SymVal *SEOpvICmpRel::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(ICmpInst);
  GET_ORACLE;

  // adjust sizes for cmp ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  // errs()<<"icmp: "<< *op <<"\n";
  // errs()<<"icmp:ty1 "<< *op->getOperand(0)->getType() <<"\n";
  // errs()<<"icmp:ty2 "<< *op->getOperand(1)->getType() <<"\n";
  sym.prepareCmpOperation(
      op->getOperand(0)->getType(), lexp,
      op->getOperand(1)->getType(), rexp);

  // generic function pointer
  Z3_ast (*action)(Z3_context, Z3_ast, Z3_ast);

  switch(op->getPredicate()){
    case CmpInst::ICMP_UGT:
      action = Z3_mk_bvugt;
      break;
    case CmpInst::ICMP_UGE:
      action = Z3_mk_bvuge;
      break;
    case CmpInst::ICMP_ULT:
      action = Z3_mk_bvult;
      break;
    case CmpInst::ICMP_ULE:
      action = Z3_mk_bvule;
      break;
    case CmpInst::ICMP_SGT:
      action = Z3_mk_bvsgt;
      break;
    case CmpInst::ICMP_SGE:
      action = Z3_mk_bvsge;
      break;
    case CmpInst::ICMP_SLT:
      action = Z3_mk_bvslt;
      break;
    case CmpInst::ICMP_SLE:
      action = Z3_mk_bvsle;
      break;
    default:
      DUMP.typedValue(op);
      llvm_unreachable("Unknown ICMP predicate");
      break;
  }

  expr.expr = sym.castBoolToMachInt(
      action(sym.getContext(), lexp, rexp),
      oracle.getTypeWidth(op->getType()));

  FINI_EXPR;
}

SymVal *SEOpvFCmpRel::derive(SymExec &sym,
    SymVal *lhs, SymVal *rhs) {

  INIT_EXPR;

  CHECK_INST(FCmpInst);
  GET_ORACLE;

  // adjust sizes for cmp ops
  Z3_ast lexp = lhs->getExpr().expr;
  Z3_ast rexp = rhs->getExpr().expr;

  Z3_ast isNaN = sym.prepareFCmpOperation(
                     op->getOperand(0)->getType(), lexp,
                     op->getOperand(1)->getType(), rexp);

  // generic function pointer
  Z3_ast (*action)(Z3_context, Z3_ast, Z3_ast);
  Z3_ast res;

  if (op->getPredicate() == FCmpInst::FCMP_UNO) {
      res = isNaN;
  }

  else {
      bool ordered = false;
      switch(op->getPredicate()){
        case FCmpInst::FCMP_OGT:
            ordered = true;
        case FCmpInst::FCMP_UGT:
            action = Z3_mk_fpa_gt;
            break;
        case FCmpInst::FCMP_OGE:
            ordered = true;
        case FCmpInst::FCMP_UGE:
            action = Z3_mk_fpa_geq;
            break;
        case FCmpInst::FCMP_OLT:
            ordered = true;
        case FCmpInst::FCMP_ULT:
            action = Z3_mk_fpa_lt;
            break;
        case FCmpInst::FCMP_OLE:
            ordered = true;
        case FCmpInst::FCMP_ULE:
            action = Z3_mk_fpa_leq;
            break;
        case FCmpInst::FCMP_UNO:
            break;

        default:
            DUMP.typedValue(op);
            llvm_unreachable("Unsupported FCMP predicate");
            break;
      }

      Z3_ast ops[2];
      ops[0] = action(sym.getContext(), lexp, rexp);
      ops[1] = isNaN;
      if (ordered) {
          ops[1] = Z3_mk_not(sym.getContext(), isNaN);
          res = Z3_mk_and(sym.getContext(), 2, ops);
      } else {
          res = Z3_mk_or(sym.getContext(), 2, ops);
      }
  }

  expr.expr = sym.castBoolToMachInt(
      res,
      oracle.getTypeWidth(op->getType()));

  FINI_EXPR;
}

SymVal *SEOpvFCmpTrue::derive(SymExec &sym) {

    INIT_EXPR;

    CHECK_INST(FCmpInst);

    expr.expr = sym.createConstBool(true);

    FINI_EXPR;
}

SymVal *SEOpvFCmpFalse::derive(SymExec &sym) {

    INIT_EXPR;

    CHECK_INST(FCmpInst);

    expr.expr = sym.createConstBool(false);

    FINI_EXPR;
}
/*
 * Calculate the offset for this gep instruction
 * and store the value into the return value and shift
 * input:
 * 1. GEP instruction
 * 2. the symbolization system
 * 3. the SymVal for the base pointer
 * 4. the SymVal for the indexs for the GEP instrcution
 * 5. the calculated shhift(returned)
 * output:
 * 1. the symbolized calculated pointer
 * 2. input 5
 */
static Z3_ast deriveGEPExpr(GetElementPtrInst *gep,
    SymExec &sym, SymVal *pval, queue<SymVal *> &idxs,int &shift) {

  // build the offset expression
  Z3_ast expr = sym.createConstPointer(0);
  shift = 0;

  // follow GEP by index
  Value *ptr = gep->getPointerOperand();

  Value *idx;
  Type *cty, *nty;

  cty = ptr->getType();
  int totalShift = 0;
  int singleShift = 0;
  User::op_iterator i = gep->idx_begin(), ie = gep->idx_end();
  ModuleOracle& mo = sym.getOracle();
  for(; i != ie; ++i){
    idx = i->get();
    //calculate shift
    int unitSize = 0;
    int times = 0;
    // nothing to GEP on if it is integerexpr type
    assert(!isa<IntegerType>(cty));

    if(isa<PointerType>(cty)){
      nty = cty->getPointerElementType();
      Z3_ast tys = sym.createConstTypeSize(nty);
      unitSize = mo.getTypeSize(nty);
      Z3_ast tid;
      if(isa<ConstantInt>(idx)){
        ConstantInt * ci = cast<ConstantInt>(idx);
        tid = sym.createConstPointerFromInt(ci);
        times = ci->getZExtValue();
      } else {
        // no times can be calculated
        tid = idxs.front()->getExpr().expr;
        idxs.pop();
      }
      singleShift = times * unitSize;
      Z3_ast off = Z3_mk_bvmul(sym.getContext(), tys, tid);
      expr = Z3_mk_bvadd(sym.getContext(), expr, off);
    }

    else if(isa<StructType>(cty)){
      assert(isa<ConstantInt>(idx));
      unsigned fid = cast<ConstantInt>(idx)->getZExtValue();
      nty = cty->getStructElementType(fid);

      Z3_ast off = sym.createConstTypeOffset(cty, fid);
      singleShift = mo.getTypeOffset(cty,fid);
      expr = Z3_mk_bvadd(sym.getContext(), expr, off);
    }

    else if(isa<ArrayType>(cty)){
      nty = cty->getArrayElementType();

      Z3_ast tys = sym.createConstTypeSize(nty);

      unitSize = mo.getTypeSize(nty);

      Z3_ast tid;
      if(isa<ConstantInt>(idx)){
        ConstantInt * ci = cast<ConstantInt>(idx);
        tid = sym.createConstPointerFromInt(ci);
        times = ci->getZExtValue();
      } else {
        tid = idxs.front()->getExpr().expr;
        idxs.pop();
      }
      singleShift = times * unitSize;
      Z3_ast off = Z3_mk_bvmul(sym.getContext(), tys, tid);
      expr = Z3_mk_bvadd(sym.getContext(), expr, off);
    }
    else if(isa<VectorType>(cty)){
        nty = cty->getVectorElementType();

        Z3_ast tys = sym.createConstTypeSize(nty);

        unitSize = mo.getTypeSize(nty);

        Z3_ast tid;
        if(isa<ConstantInt>(idx)){
            ConstantInt * ci = cast<ConstantInt>(idx);
            tid = sym.createConstPointerFromInt(ci);
            times = ci->getZExtValue();
        } else {
            tid = idxs.front()->getExpr().expr;
            idxs.pop();
        }

        singleShift = times * unitSize;
        Z3_ast off = Z3_mk_bvmul(sym.getContext(), tys, tid);
        expr = Z3_mk_bvadd(sym.getContext(), expr, off);
    }
    else {
      DUMP.typedValue(gep);
      llvm_unreachable("Unhandled GEP type");
    }

    totalShift += singleShift;

    cty = nty;
  }

  // ensure we unrolled correctly
  assert(cty == gep->getResultElementType());
  assert(idxs.empty());

  shift = totalShift;
  // get full expr
  return Z3_mk_bvadd(sym.getContext(), pval->getExpr().expr, expr);
}

SymVal *SEOpvGEPIdx0::derive(SymExec &sym,
    SymVal *ptr) {

  INIT_EXPR;

  CHECK_INST(GetElementPtrInst);

  queue<SymVal *> idxs;
  int shift;
  expr.expr = deriveGEPExpr(op, sym, ptr, idxs,shift);

  SymExpr ptrExpr = ptr->getExpr();
  //if comes from a alloc
  FINI_EXPR;
}

SymVal *SEOpvGEPIdx1::derive(SymExec &sym,
    SymVal *ptr, SymVal *idx0) {

  INIT_EXPR;

  CHECK_INST(GetElementPtrInst);

  queue<SymVal *> idxs;
  idxs.push(idx0);
  int shift;
  expr.expr = deriveGEPExpr(op, sym, ptr, idxs,shift);

  SymExpr ptrExpr = ptr->getExpr();
  FINI_EXPR;
}

SymVal *SEOpvGEPIdx2::derive(SymExec &sym,
    SymVal *ptr, SymVal *idx0, SymVal *idx1) {

  INIT_EXPR;

  CHECK_INST(GetElementPtrInst);

  queue<SymVal *> idxs;
  idxs.push(idx0);
  idxs.push(idx1);
  int shift;
  expr.expr = deriveGEPExpr(op, sym, ptr, idxs,shift);

  SymExpr ptrExpr = ptr->getExpr();
  FINI_EXPR;
}

SymVal *SEOpvGEPIdx3::derive(SymExec &sym,
                             SymVal *ptr, SymVal *idx0, SymVal *idx1,SymVal *idx2) {

    INIT_EXPR;

    CHECK_INST(GetElementPtrInst);

    queue<SymVal *> idxs;
    idxs.push(idx0);
    idxs.push(idx1);
    idxs.push(idx2);

    int shift;
    expr.expr = deriveGEPExpr(op, sym, ptr, idxs,shift);
    SymExpr ptrExpr = ptr->getExpr();
    FINI_EXPR;
}

SymVal *SEOpvPhi::derive(SymExec &sym,
    SymVal *tran) {

  INIT_EXPR;

  CHECK_INST(PHINode);

  expr.expr = tran->getExpr().expr;

  FINI_EXPR;
}

SymVal *SEOpvSelect::derive(SymExec &sym,
    SymVal *cval, SymVal *tval, SymVal *fval) {

  INIT_EXPR;

  CHECK_INST(SelectInst);

  expr.expr = Z3_mk_ite(sym.getContext(),
      sym.castMachIntToBool(cval->getExpr().expr),
      tval->getExpr().expr, fval->getExpr().expr);

  FINI_EXPR;
}

SymVal *SEOpvBranch::derive(SymExec &sym,
    SymVal *cval) {

  INIT_EXPR;

  CHECK_INST(BranchInst);

  int c = host->getGraph()->getCond(host);
  if(c == 0){
    // true value taken
    sym.addAssert(sym.castMachIntToBool(cval->getExpr().expr));
  } else if(c == 1){
    // false value taken
    sym.addAssert(sym.castMachIntToBoolInversed(cval->getExpr().expr));
  } else {
    // no branch is taken, do nothing
  }

  expr.expr = sym.createSpecialNone();

  FINI_EXPR;
}

SymVal *SEOpvLoad::derive(SymExec &sym,
    SymVal *ptr) {

  INIT_EXPR;

  CHECK_INST(LoadInst);
  GET_ORACLE;
  uint64_t width = oracle.getTypeWidth(op->getType());
  uint64_t size = oracle.getTypeSize(op->getType());
  expr.expr = sym.loadMemory(ptr->getExpr().expr,size);
  // since llvm's mem model use bit granularity,
  // but our mem model use byte granularity, we
  // need to truncate the load result.

  if (width != size * 8) {
      expr.expr = sym.castMachIntTrunc(expr.expr, width);
  }
  Value* loading_from = op->getPointerOperand();
  if(GlobalVariable* glb = dyn_cast<GlobalVariable>(loading_from)){
      //loading from a global variable
      ModuleOracle &oracle = sym.getOracle();
      Function& cur_func = host->getGraph()->func;
      pair<formalize_kind,struct SysctlInfo*> sysctl_info = oracle.issysctl(glb,&cur_func);
      if(sysctl_info.first ==  F_Const){
          if(sysctl_info.second->has_initializer){
              int64_t initializer = sysctl_info.second->initializer;
              //assert this memory to be the constant
              sym.addAssert(Z3_mk_eq(sym.getContext(), expr.expr, sym.createConstMachIntUnsigned(initializer,width)));
          }
      }
      else if(sysctl_info.first == F_User){
          if(sysctl_info.second->has_lower_bound){
              int64_t low_bound = sysctl_info.second->lower_bound;
              sym.addAssert(Z3_mk_bvuge(sym.getContext(), expr.expr, sym.createConstMachIntUnsigned(low_bound,width)));
          }
          if(sysctl_info.second->has_upper_bound){
              int64_t up_bound = sysctl_info.second->upper_bound;
              sym.addAssert(Z3_mk_bvule(sym.getContext(), expr.expr, sym.createConstMachIntUnsigned(up_bound,width)));
          }
      }
  }

  sym.prepareIfLoadFP(op, expr);

  FINI_EXPR;
}

SymVal *SEOpvStore::derive(SymExec &sym,
    SymVal *ptr, SymVal *vop) {

  INIT_EXPR;

  CHECK_INST(StoreInst);
  Z3_ast addr_expr = ptr->getExpr().expr;
  Z3_ast value_expr = sym.prepareIfStoreFP(op->getValueOperand(), vop->getExpr());
  //check if the stored value is a constant or even a \0
  bool isNumeral = false;
  bool isZero = false;
  if(Z3_is_numeral_ast(sym.getZ3Context(),value_expr)){
      isNumeral = true;
      uint64_t num;
      if(Z3_get_numeral_uint64(sym.getZ3Context(),value_expr,&num)){
          if(num == 0)
              isZero = true;
      }
  }
  sym.storeMemory(addr_expr,value_expr);

  expr.expr = sym.createSpecialNone();

  FINI_EXPR;
}

static Z3_ast deriveExtValExpr(ExtractValueInst *extval,
                               ModuleOracle& mo,
                               SymExec &sym, SymVal *aggval) {

  unsigned int saddr = 0;

  // follow ExtVal by index
  Value *ptr = extval->getAggregateOperand();
  unsigned int idx;
  Type *cty, *nty;

  cty = ptr->getType();

  ExtractValueInst::idx_iterator i = extval->idx_begin(), ie = extval->idx_end();
  for(; i != ie; ++i){
    idx = *i;

    // nothing to extract if it is integer type
    assert(!isa<IntegerType>(cty));

    if(isa<StructType>(cty)){
      nty = cty->getStructElementType(idx);
      saddr += mo.getTypeOffset(cty, idx);
    }
    else if(isa<ArrayType>(cty)){
      nty = cty->getArrayElementType();
      unsigned int tys = mo.getTypeSize(nty);
      saddr += idx * tys;
    }
    else {
      DUMP.typedValue(extval);
      llvm_unreachable("Unhandled ExtractValue type");
    }

    cty = nty;
  }
  // get extracted expr
  assert(cty == extval->getType());

  Z3_ast res = sym.extractMachBitsVar(aggval->getExpr().expr,
                                      saddr, mo.getTypeWidth(cty));
  return res;
}
SymVal *SEOpvExtVal::derive(SymExec &sym,
                            SymVal *ptr) {

    INIT_EXPR;

    CHECK_INST(ExtractValueInst);

    GET_ORACLE;

    //TODO(yaohway: test the new impl)
    expr.expr = deriveExtValExpr(op, oracle, sym, ptr);
    expr.allocStatus = ptr->getExpr().allocStatus;
    // expr.expr = ptr->getExpr().expr;

    FINI_EXPR;
}

static Z3_ast deriveInsertValExpr(InsertValueInst *insval,
                                  ModuleOracle &mo,
                                  SymExec &sym,
                                  SymVal *aggval,
                                  SymVal *insertedval) {

  unsigned int saddr = 0;

  // follow InsVal by index
  Value *ptr = insval->getAggregateOperand();

  unsigned int idx;
  Type *cty, *nty;

  cty = ptr->getType();

  InsertValueInst::idx_iterator i = insval->idx_begin(), ie = insval->idx_end();
  for(; i != ie; ++i){
      idx = *i;

      // nothing to unfold if it is integer type
      assert(!isa<IntegerType>(cty));

      if(isa<StructType>(cty)){
          nty = cty->getStructElementType(idx);
          saddr += mo.getTypeOffset(cty, idx);
      }
      else if(isa<ArrayType>(cty)){
          nty = cty->getArrayElementType();
          unsigned int tys = mo.getTypeSize(nty);
          saddr += idx * tys;
      }
      else {
          DUMP.typedValue(insval);
          llvm_unreachable("Unhandled InsertValue type");
      }

      cty = nty;
  }

  // get inserted expr
  Z3_ast res = sym.replaceMachBitsVar(aggval->getExpr().expr,
                                      saddr, mo.getTypeSize(cty),
                                      insertedval->getExpr().expr);
  assert(sym.getExprSortSize(res) == mo.getTypeSize(insval->getType()));
  return res;
}

SymVal *SEOpvInsertVal::derive(SymExec &sym,
                               SymVal *ptr,
                               SymVal *vop) {

    INIT_EXPR;
    GET_ORACLE;

    CHECK_INST(InsertValueInst);

    expr.expr = deriveInsertValExpr(op, oracle, sym, ptr, vop);

    FINI_EXPR;
}

// locators
bool SEGraph::locateValue(int cur, Value *val, int &seq) {
  // none-instructions are always located at location -1
  if(!isa<Instruction>(val)){
    seq = -1;
    return true;
  }

  // instructions can only be located from instructions
  //errs()<<"val:"<<*val<<'\n';
  assert(cur >= 0);
  //for(auto traceIt = trace.begin();traceIt != trace.end();traceIt++)
      //errs()<<"trace:"<<**traceIt<<'\n';
  for(seq = cur; seq >= 0; seq--){
    if(trace.at(seq) == val){
      return true;
    }
  }

  return false;
}

// node getter
SENode *SEGraph::getNode(int seq, Value *val) {
  auto k = make_pair(seq, val);

  // first check whether the node exists
  auto i = nodes.find(k);
  if(i != nodes.end()){
    return i->second;
  }


  // then make sure the value exists at given location
  if(seq < 0){
    assert(!isa<Instruction>(val));
  } else {
    assert(trace.at(seq) == val);
  }

  // finally build the node

  SENode *res = buildNode(seq, val);
  nodes.insert(make_pair(k, res));
  return res;
}
void SEGraph::push_context(SliceOracle* new_orc) {
    so_back = so;
    so = new_orc;

    if(trace_back.empty()){
        for(auto trace_it = trace.begin();trace_it != trace.end();trace_it++){
            trace_back.push_back(*trace_it);
        }
    }
    if(nodes_back.empty()){
        for(map<pair<int, Value *>, SENode *>::iterator node_it = nodes.begin(); node_it != nodes.end();node_it ++){
            nodes_back.insert(make_pair(node_it->first,node_it->second));
        }
    }
    if(conds_back.empty()){
        for(auto cond_it = conds.begin(); cond_it != conds.end();cond_it++){
            conds_back.insert(make_pair(cond_it->first,cond_it->second));
        }
    }
    count_back = count;

}
void SEGraph::pop_context() {
    assert(so_back != nullptr);
    so = so_back;

    assert(!trace_back.empty());
    trace.clear();
    trace.shrink_to_fit();
    for(auto trace_it = trace_back.begin(); trace_it != trace_back.end();trace_it++){
        trace.push_back(*trace_it);
    }

    assert(!nodes_back.empty());
    for(auto node_it = nodes.begin(); node_it != nodes.end(); node_it++){
        if(!in_original_node_set(node_it->second)){
            delete node_it->second;
        }
    }
    nodes.clear();
    for(map<pair<int, Value *>, SENode *>::iterator node_it = nodes_back.begin(); node_it != nodes_back.end();node_it ++){
        nodes.insert(make_pair(node_it->first,node_it->second));
    }

    assert(!conds_back.empty());
    conds.clear();
    for(auto cond_it = conds_back.begin();cond_it != conds_back.end();cond_it++){
        conds.insert(make_pair(cond_it->first,cond_it->second));
    }
    count = count_back;
}

bool SEGraph::in_original_node_set(SENode* n){
    for(auto back_node = nodes_back.begin(); back_node != nodes_back.end(); back_node++)
    {
        SENode* node = back_node->second;
        if(node == n)
            return true;
    }
    return false;
}
SENode *SEGraph::getNodeOrBuild(int cur, Value *val) {
  // find the seq for the val
  int seq;
  if(!locateValue(cur, val, seq)){
      for(auto traceIt = trace.begin();traceIt != trace.end();traceIt++)
          errs()<<**traceIt<<'\n';
      errs()<<"[Err]missing:"<<*val<<" in the trace\n";
      throw("[Err]instruction is missing");
  }
  assert(seq <= cur);

  return getNode(seq, val);
}

SENode *SEGraph::getNodeOrNull(int seq, Value *val) {
  auto k = make_pair(seq, val);
  auto i = nodes.find(k);
  if(i == nodes.end()){
    return nullptr;
  } else {
    return i->second;
  }
}

SENode *SEGraph::getNodeOrFail(int cur, Value *val) {
    // find the seq for the val
    int seq;
    assert(locateValue(cur, val, seq));
    assert(seq <= cur);

    // get the node
    SENode *res = getNodeOrNull(seq, val);
    assert(res != nullptr);
    return res;
}

SENode *SEGraph::getNodeProbe(Value *val, bool ask_updated) {
    if(ask_updated){
        if(updated_values.find(val) != updated_values.end()){
            assert(updated_values.at(val) !=  val);
            return getNodeProbe(updated_values.at(val));
        }
    }
    if(!isa<Instruction>(val)){
        return getNodeOrNull(-1, val);
    }


    SENode *node;
    assert(highest_seq != -1);
    for(int i = highest_seq; i >= 0; i--){
        node = getNodeOrNull(i, val);
        if(node != nullptr){
            return node;
        }
    }
    return nullptr;
}

SENode *SEGraph::getNodeById(int id) {
    for(auto nodeIt = nodes.begin(); nodeIt != nodes.end(); nodeIt++){
        if(nodeIt->second->getId() == id)
            return nodeIt->second;
    }
    return nullptr;
}

// node builder
#define INIT_TYPE(val_type)                                         \
  if(isa<val_type>(val)) {                                          \
    val_type *op = cast<val_type>(val);

#define INIT_NODE(se_type)                                          \
  SENode##se_type *node = new SENode##se_type(seq, op, this);       \
  addNode(node);

#define FINI_NODE                                                   \
  return node;

#define FINI_TYPE                                                   \
  }

#define INIT_TYPE_NODE(val_type, se_type)                           \
  INIT_TYPE(val_type)                                               \
  INIT_NODE(se_type)

#define FINI_TYPE_NODE                                              \
  FINI_NODE                                                         \
  FINI_TYPE

#define IGNORE_NODE                                                 \
  SENodeUnknown *node = new SENodeUnknown(seq, op, this);           \
  addNode(node);                                                    \
  return node;

#define IGNORE_TYPE(val_type)                                       \
  INIT_TYPE(val_type)                                               \
  IGNORE_NODE                                                       \
  FINI_TYPE

#define UNHANDLED(msg)                                              \
  DUMP.typedValue(val);                                             \
  llvm_unreachable("Unhandled: " msg);                              \
  return nullptr;

SENode* SEGraph::buildSpecial(int seq, CallInst *val) {
    //INIT_NODE
    SENodeSpecialCall * node = new SENodeSpecialCall(seq,val,this);
    //errs()<<"building special:"<<*val<<'\n';
    addNode(node);
    //for(auto operand = val->arg_begin(); operand != val->arg_end();operand++){
    //    errs()<<"operand:"<<**operand<<'\n';
    //}
    for(auto operand = val->arg_begin(); operand != val->arg_end();operand++){
        SENode* op = getNodeOrBuild(seq,*operand);
        node->addDep(op);
        op->addUsr(node);
    }
    //FINI_NODE
    return node;
}
SENode *SEGraph::buildNode(int seq, Value *val) {
  //TODO: handle Floatpoint binary operator
  // variables
  INIT_TYPE_NODE(Argument, Param) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(GlobalVariable, Global) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(AllocaInst, Local) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(UndefValue, Undef) {} FINI_TYPE_NODE;

  // constants
  // lcm comment:
  // this has a problem that, say, 2 instructions both use a constant, an integer, zero(which is very common to see)
  // and in the SEGraph, the user of this constant zero will comprise both of them
  // this is not too much a issue,(this should be a smart move actually, because this reduce the # of nodes that need to be constructed)
  // unless you want to figure out the data dependency for a variable, in this case,  you will want there to be 2 seperate constant zero.
  INIT_TYPE_NODE(ConstantInt, CInt) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(ConstantAggregate, CAgg) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(ConstantAggregateZero, CAggZero) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(ConstantDataSequential, CDataSeq) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(BlockAddress,CBlkAddr) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(ConstantPointerNull, CNull) {} FINI_TYPE_NODE;

  INIT_TYPE_NODE(ConstantFP, CFloat) {} FINI_TYPE_NODE;
  // instructions
  // if have dependency which is unknown, convert it as unknown
  INIT_TYPE(CastInst) {
      SENode *orig = getNodeOrBuild(seq, op->getOperand(0));
      if(isa<SENodeUnknown>(orig)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(CastInst, Cast) {
    SENode *orig = getNodeOrBuild(seq, op->getOperand(0));
    switch(op->getOpcode()){
        case Instruction::Trunc:
            node->addOpv(new SEOpvCastTrunc(seq, op, orig));
            break;
        case Instruction::ZExt:
            node->addOpv(new SEOpvCastZExt(seq, op, orig));
            break;
        case Instruction::SExt:
            node->addOpv(new SEOpvCastSExt(seq, op, orig));
            break;
        case Instruction::PtrToInt:
            node->addOpv(new SEOpvCast2Int(seq, op, orig));
            break;
        case Instruction::IntToPtr:
            node->addOpv(new SEOpvCast2Ptr(seq, op, orig));
            break;
        case Instruction::BitCast:
            node->addOpv(new SEOpvCastType(seq, op, orig));
            break;
        case Instruction::FPTrunc:
            node->addOpv(new SEOpvCastFPTrunc(seq, op, orig));
            break;
        case Instruction::FPExt:
            node->addOpv(new SEOpvCastFPExt(seq, op, orig));
            break;
        case Instruction::FPToSI:
            node->addOpv(new SEOpvCastFPToSI(seq, op, orig));
            break;
        case Instruction::FPToUI:
            node->addOpv(new SEOpvCastFPToUI(seq, op, orig));
            break;
        case Instruction::SIToFP:
            node->addOpv(new SEOpvCastSIToFP(seq, op, orig));
            break;
        case Instruction::UIToFP:
            node->addOpv(new SEOpvCastUIToFP(seq, op, orig));
            break;
        default:
            DUMP.typedValue(val);
            llvm_unreachable("Unhandled cast type");
            break;
    }
  } FINI_TYPE_NODE;

  INIT_TYPE(BinaryOperator) {
      SENode *lhs= getNodeOrBuild(seq, op->getOperand(0));
      SENode *rhs= getNodeOrBuild(seq, op->getOperand(1));
      if(isa<SENodeUnknown>(lhs) || isa<SENodeUnknown>(rhs)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(BinaryOperator, Calc) {
    SENode *lhs = getNodeOrBuild(seq, op->getOperand(0));
    SENode *rhs = getNodeOrBuild(seq, op->getOperand(1));

    switch(op->getOpcode()){
      case Instruction::Add:
        node->addOpv(new SEOpvCalcAdd(seq, op, lhs, rhs));
        break;
      case Instruction::Sub:
        node->addOpv(new SEOpvCalcSub(seq, op, lhs, rhs));
        break;
      case Instruction::Mul:
        node->addOpv(new SEOpvCalcMul(seq, op, lhs, rhs));
        break;
      case Instruction::UDiv:
        node->addOpv(new SEOpvCalcUDiv(seq, op, lhs, rhs));
        break;
      case Instruction::SDiv:
        node->addOpv(new SEOpvCalcSDiv(seq, op, lhs, rhs));
        break;
      case Instruction::URem:
        node->addOpv(new SEOpvCalcURem(seq, op, lhs, rhs));
        break;
      case Instruction::SRem:
        node->addOpv(new SEOpvCalcSRem(seq, op, lhs, rhs));
        break;
      case Instruction::Shl:
        node->addOpv(new SEOpvCalcShl(seq, op, lhs, rhs));
        break;
      case Instruction::LShr:
        node->addOpv(new SEOpvCalcLShr(seq, op, lhs, rhs));
        break;
      case Instruction::AShr:
        node->addOpv(new SEOpvCalcAShr(seq, op, lhs, rhs));
        break;
      case Instruction::And:
        node->addOpv(new SEOpvCalcAnd(seq, op, lhs, rhs));
        break;
      case Instruction::Or:
        node->addOpv(new SEOpvCalcOr(seq, op, lhs, rhs));
        break;
      case Instruction::Xor:
        node->addOpv(new SEOpvCalcXor(seq, op, lhs, rhs));
        break;
      case Instruction::FAdd:
        node->addOpv(new SEOpvCalcFAdd(seq, op, lhs, rhs));
        break;
      case Instruction::FSub:
        node->addOpv(new SEOpvCalcFSub(seq, op, lhs, rhs));
        break;
      case Instruction::FMul:
        node->addOpv(new SEOpvCalcFMul(seq, op, lhs, rhs));
        break;
      case Instruction::FDiv:
        node->addOpv(new SEOpvCalcFDiv(seq, op, lhs, rhs));
        break;
      case Instruction::FRem:
        node->addOpv(new SEOpvCalcFRem(seq, op, lhs, rhs));
        break;
      default:
        DUMP.typedValue(val);
        llvm_unreachable("Unhandled binary operator");
        break;
    }
  } FINI_TYPE_NODE;

  INIT_TYPE(ICmpInst) {
      SENode *lhs= getNodeOrBuild(seq, op->getOperand(0));
      SENode *rhs= getNodeOrBuild(seq, op->getOperand(1));
      if(isa<SENodeUnknown>(lhs) || isa<SENodeUnknown>(rhs)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(ICmpInst, ICmp) {

    SENode *lhs = getNodeOrBuild(seq, op->getOperand(0));
    SENode *rhs = getNodeOrBuild(seq, op->getOperand(1));

    switch(op->getPredicate()){
    case CmpInst::ICMP_EQ:
        node->addOpv(new SEOpvICmpEq(seq, op, lhs, rhs));
        break;

      case CmpInst::ICMP_NE:
        node->addOpv(new SEOpvICmpNe(seq, op, lhs, rhs));
        break;

      default:
        node->addOpv(new SEOpvICmpRel(seq, op, lhs, rhs));
        break;
    }
  } FINI_TYPE_NODE;

  INIT_TYPE(FCmpInst) {
      SENode *lhs= getNodeOrBuild(seq, op->getOperand(0));
      SENode *rhs= getNodeOrBuild(seq, op->getOperand(1));
      if(isa<SENodeUnknown>(lhs) || isa<SENodeUnknown>(rhs)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(FCmpInst, FCmp) {

      SENode *lhs = getNodeOrBuild(seq, op->getOperand(0));
      SENode *rhs = getNodeOrBuild(seq, op->getOperand(1));

      switch(op->getPredicate()){
      case FCmpInst::FCMP_OEQ:
      case FCmpInst::FCMP_UEQ:
          node->addOpv(new SEOpvFCmpEq(seq, op, lhs, rhs));
          break;

      case FCmpInst::FCMP_ONE:
      case FCmpInst::FCMP_UNE:
          node->addOpv(new SEOpvFCmpNe(seq, op, lhs, rhs));
          break;

      case FCmpInst::FCMP_TRUE:
          node->addOpv(new SEOpvFCmpTrue(seq, op));
      case FCmpInst::FCMP_FALSE:
          node->addOpv(new SEOpvFCmpFalse(seq, op));

      default:
          node->addOpv(new SEOpvFCmpRel(seq, op, lhs, rhs));
          break;
      }
  } FINI_TYPE_NODE;

  INIT_TYPE(GetElementPtrInst) {
      vector<Value *> vars;
      int properly_decompossed = SEGUtil::decompose(op, vars);
      if(vars.size() > 3){
          properly_decompossed = false;
      }
      if(! properly_decompossed){
          IGNORE_NODE;
      }
      SENode *ptr = getNodeOrBuild(seq, op->getPointerOperand());
      if(isa<SENodeUnknown>(ptr)) {
          IGNORE_NODE;
      }
      switch(vars.size()) {
      case 0:
          break;
      case 1:
          {
              SENode *idx0 = getNodeOrBuild(seq, vars[0]);
              if(isa<SENodeUnknown>(idx0)) {
                  IGNORE_NODE;
              }
          }
          break;
      case 2:
          {
              SENode *idx0 = getNodeOrBuild(seq, vars[0]);
              SENode *idx1 = getNodeOrBuild(seq, vars[1]);
              if(isa<SENodeUnknown>(idx0) || isa<SENodeUnknown>(idx1)) {
                  IGNORE_NODE;
              }
          }
          break;
      case 3:
        {
            SENode *idx0 = getNodeOrBuild(seq, vars[0]);
            SENode *idx1 = getNodeOrBuild(seq, vars[1]);
            SENode *idx2 = getNodeOrBuild(seq, vars[2]);
            if(isa<SENodeUnknown>(idx0) || isa<SENodeUnknown>(idx1) || isa<SENodeUnknown>(idx2)) {
                IGNORE_NODE;
            }
            break;
        }
      default:
          llvm_unreachable("Too many unknown variables in GEP index");
          break;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(GetElementPtrInst, GEP) {
    vector<Value *> vars;
    SEGUtil::decompose(op, vars);
    // NOTE there is no hard limit on this num, it is just that it fits
    // well with GEPIdx0/1/2
    assert(vars.size() <= 3);

    SENode *ptr = getNodeOrBuild(seq, op->getPointerOperand());
    switch(vars.size()) {
      case 0:
        node->addOpv(new SEOpvGEPIdx0(seq, op, ptr));
        break;
      case 1:
        {
          SENode *idx0 = getNodeOrBuild(seq, vars[0]);
          node->addOpv(new SEOpvGEPIdx1(seq, op, ptr, idx0));
        }
        break;
      case 2:
        {
          SENode *idx0 = getNodeOrBuild(seq, vars[0]);
          SENode *idx1 = getNodeOrBuild(seq, vars[1]);
          node->addOpv(new SEOpvGEPIdx2(seq, op, ptr, idx0, idx1));
        }
        break;
      case 3:
        {
            SENode *idx0 = getNodeOrBuild(seq, vars[0]);
            SENode *idx1 = getNodeOrBuild(seq, vars[1]);
            SENode *idx2 = getNodeOrBuild(seq, vars[2]);
            node->addOpv(new SEOpvGEPIdx3(seq, op, ptr, idx0, idx1,idx2));
        }
        break;
      default:
        llvm_unreachable("Too many unknown variables in GEP index");
        break;
    }
  } FINI_TYPE_NODE;

  INIT_TYPE(PHINode) {
      Value *opv = SEGUtil::backtrace(seq, op, trace, so);
      SENode *tran = getNodeOrBuild(seq, opv);
      if(isa<SENodeUnknown>(tran)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(PHINode, Phi) {
    Value *opv = SEGUtil::backtrace(seq, op, trace, so);
    SENode *tran = getNodeOrBuild(seq, opv);
    node->addOpv(new SEOpvPhi(seq, op, tran));
  } FINI_TYPE_NODE;

  INIT_TYPE(SelectInst) {
      SENode *cval = getNodeOrBuild(seq, op->getCondition());
      SENode *tval = getNodeOrBuild(seq, op->getTrueValue());
      SENode *fval = getNodeOrBuild(seq, op->getFalseValue());
      if(isa<SENodeUnknown>(cval)
         || isa<SENodeUnknown>(tval)
         || isa<SENodeUnknown>(fval)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(SelectInst, Select) {
    SENode *cval = getNodeOrBuild(seq, op->getCondition());
    SENode *tval = getNodeOrBuild(seq, op->getTrueValue());
    SENode *fval = getNodeOrBuild(seq, op->getFalseValue());
    node->addOpv(new SEOpvSelect(seq, op, cval, tval, fval));
  } FINI_TYPE_NODE;

  INIT_TYPE(BranchInst) {
      if(op->isUnconditional()){
          IGNORE_NODE;
      }
      SENode *cval = getNodeOrBuild(seq, op->getCondition());
      if(isa<SENodeUnknown>(cval)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(BranchInst, Branch) {
    if(op->isUnconditional()){
        IGNORE_NODE;
    }
    SENode *cval = getNodeOrBuild(seq, op->getCondition());
    node->addOpv(new SEOpvBranch(seq, op, cval));
  } FINI_TYPE_NODE;

  INIT_TYPE(LoadInst) {
      SENode *ptr = getNodeOrBuild(seq, op->getPointerOperand());
      if(isa<SENodeUnknown>(ptr)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(LoadInst, Load) {
    SENode *ptr = getNodeOrBuild(seq, op->getPointerOperand());
    node->addOpv(new SEOpvLoad(seq, op, ptr));
  } FINI_TYPE_NODE;

  INIT_TYPE(StoreInst) {
      SENode *ptr = getNodeOrBuild(seq, op->getPointerOperand());
      SENode *vop = getNodeOrBuild(seq, op->getValueOperand());
      if(isa<SENodeUnknown>(ptr) || isa<SENodeUnknown>(vop)) {
          IGNORE_NODE;
      }// why don't just move it the the following clause?
  } FINI_TYPE;
  INIT_TYPE_NODE(StoreInst, Store) {
    SENode *ptr = getNodeOrBuild(seq, op->getPointerOperand());
    SENode *vop = getNodeOrBuild(seq, op->getValueOperand());
    node->addOpv(new SEOpvStore(seq, op, ptr, vop));
  } FINI_TYPE_NODE;

  INIT_TYPE(CallInst) {
    // inline asm
    if(op->isInlineAsm()){
      InlineAsm *bin = dyn_cast<InlineAsm>(op->getCalledValue());
      assert(bin != nullptr);
      string fn = bin->getAsmString();

#define ASMCALL_NODE
#include "Asmcall.def"
//ALL_OTHER_ASM_NODE
//#undef ALL_OTHER_ASM_NODE
#undef ASMCALL_NODE

      // TODO other inline asm
      IGNORE_NODE;
    }

    // lib calls
    Function *tar = op->getCalledFunction();
    if(tar == nullptr){
      // TODO indirect call
      IGNORE_NODE;
    }

    string fn = tar->getName().str();
    if(tar->isIntrinsic()){
      Helper::convertDotInName(fn);
    }
#define LIBCALL_NODE
#include "Libcall.def"
//ALL_OTHER_CALLS_NODE
//#undef ALL_OTHER_CALLS_NODE
#undef LIBCALL_NODE

    // TODO other lib calls
    IGNORE_NODE;
  } FINI_TYPE;

  INIT_TYPE(ExtractValueInst) {
      // errs() << *op->getAggregateOperand()->getType() <<"XXX\n";
      SENode *ptr = getNodeOrBuild(seq, op->getAggregateOperand());
      if(isa<SENodeUnknown>(ptr)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(ExtractValueInst, ExtVal) {
    SENode *ptr = getNodeOrBuild(seq, op->getAggregateOperand());
    node->addOpv(new SEOpvExtVal(seq, op, ptr));
  } FINI_TYPE_NODE;

  INIT_TYPE(InsertValueInst) {
      SENode *ptr = getNodeOrBuild(seq, op->getAggregateOperand());
      SENode *val = getNodeOrBuild(seq, op->getInsertedValueOperand());
      if(isa<SENodeUnknown>(ptr) || isa<SENodeUnknown>(val)) {
          IGNORE_NODE;
      }
  } FINI_TYPE;
  INIT_TYPE_NODE(InsertValueInst, InsertVal) {
      SENode *ptr = getNodeOrBuild(seq, op->getAggregateOperand());
      SENode *val = getNodeOrBuild(seq, op->getInsertedValueOperand());
      node->addOpv(new SEOpvInsertVal(seq, op, ptr, val));
  } FINI_TYPE_NODE;

  // TODO, handle ExtractElementInst and InsertElemInst
  IGNORE_TYPE(ExtractElementInst);
  IGNORE_TYPE(InsertElementInst);
  IGNORE_TYPE(Function);
  IGNORE_TYPE(MetadataAsValue);

  errs()<<*val<<"type:"<<*(val->getType())<<'\n';
  UNHANDLED("Unknown value type for node building");
}

// SEGraph construction
static inline void collectDeps(SENode *node, set<SENode *> &deps) {
  if(deps.find(node) != deps.end()){
    return;
  }
  deps.insert(node);
  SENode::linkIter di = node->depBegin(), de = node->depEnd();
  for(; di != de; ++di){
    collectDeps(*di, deps);
  }
}

string SEGraph::str_nodetype(SENode* node ){
    SEType t = node->getType();
    if( t == SE_CInt){
        return "SE_CInt";
    }else if(t == SE_CFloat){
        return "SE_CFloat";
    }else if(t == SE_CNull){
        return "SE_CNull";
    }else if(t == SE_CAgg){
        return "SE_CAgg";
    }else if( t == SE_CAggZero ){
        return "SE_CAggZero";
    }else if(t == SE_CDataSeq){
        return "SE_CDataSeq";
    }else if(t == SE_CBlkAddr){
        return "SE_CBlkAddr";
    }else if(t == SE_Param){
        return "SE_Param";
    }else if(t == SE_Global){
        return "SE_Global";
    }else if(t == SE_Local){
        return "SE_Local";
    }else if(t == SE_Undef){
        return "SE_Undef";
    }
    else if(t >= SE_Cast && t <= SE_CastUIToFP){
        return "SE_Cast";
    }else if(t >= SE_CalcAdd && t <= SE_Calc){
        return "SE_Calc";
    }else if(t >= SE_ICmpEq && t <= SE_FCmp){
        return "SE_Cmp";
    }else if(t >= SE_GEPIdx0 && t <= SE_GEP){
        return "SE_GEP";
    }else if(t == SE_Phi){
        return "SE_Phi";
    }else if(t == SE_Select){
        return "SE_Select";
    }else if(t == SE_Branch){
        return "SE_Branch";
    }else if(t == SE_Load){
        return "SE_Load";
    }else if(t == SE_Store){
        return "SE_Store";
    }else if(t > SE_Store && t <= SE_Call){
        return "SE_HandledCall";
    }else if(t > SE_Call && t <= SE_Asm){
        return "SE_HandledAsm";
    }else if(t == SE_ExtVal){
        return "SE_ExtVal";
    }else if(t == SE_InsertVal){
        return "SE_InsertVal";
    }else if(t == SE_SpecialCall){
        return "SE_SpecialCall";
    }else if(t >= SE_UnhandledCall && t <= SE_Call_Unhandled4){
        return "SE_Unhandled_call";
    }else if(t >= SE_UnhandledAsm && t<= SE_Asm_Unhandled3){
        return "SE_Asm_Unhandled_asm";
    }else if(t == SE_Unhandled){
        return "SE_Unhandled";
    }else {
        llvm_unreachable("out of range enum");
    }
}

void displayBasicDep(SENode * pnode)
{
    errs()<<pnode->getId()<<"|"<<pnode->getSeq()<<"|"<<*pnode->getVal()<<"\n";
    int usesize = pnode->deps.size();
    errs() <<"deps:"<<usesize<<"\n";
    SENode::linkIter ub = pnode->deps.begin();
    SENode::linkIter ue = pnode->deps.end();
    for(;ub!=ue;ub++)
        errs()<<"  "<< pnode->getGraph()->str_nodetype(*ub)<<"|"<<(*ub)->getId()<<"|"<<(*ub)->getSeq()<<'|'<<*(*ub)->getVal()<<"\n";
}
void displayBasicUsr(SENode * pnode)
{
  errs()<<pnode->getId()<<"|"<<pnode->getSeq()<<"|"<<*pnode->getVal()<<"\n";
  int usesize = pnode->deps.size();
  errs() <<"usrs:"<<usesize<<"\n";
  SENode::linkIter ub = pnode->usrs.begin();
  SENode::linkIter ue = pnode->usrs.end();
  for(;ub!=ue;ub++)
    errs()<<"  "<<pnode->getGraph()->str_nodetype(*ub)<<"|"<<(*ub)->getId()<<"|"<<(*ub)->getSeq()<<'|'<<*(*ub)->getVal()<<"\n";
}


void SEGraph::displayForDeps(){
    for(auto node : nodes){
      displayBasicDep(node.second);
    }
}
void SEGraph::displaySubstitutes(){
    for(auto update: updated_values){
        errs()<<*(update.first) <<"updated by \n"<<*(update.second)<<"\n\n";
    }
}

void SEGraph::displayForUsrs(){
  for(auto node : nodes){
    displayBasicUsr(node.second);
  }
}
void SEGraph::displayForDeps(set<SENode*> nodesset){
    map<int,SENode*> sortedNodes;
    for(auto node : nodesset) {
        sortedNodes.insert(make_pair(node->getId(),node));
    }
    for(auto node : sortedNodes){
      displayBasicDep(node.second);
    }
}

void SEGraph::displayTrace(){

    for(auto node : nodes){
        errs()<<node.first.first<<","<<*node.first.second<<'\n';
    }
}
void SEGraph::additional_seg(iseq &new_trace,SliceOracle * new_or){
    //follow trace
    int seq = trace.size();
    errs()<<"len nodes:"<<nodes.size()<<"\n trace len:"<<trace.size()<<'\n';
    for(auto new_trace_it = new_trace.begin(); new_trace_it != new_trace.end(); new_trace_it++){
        trace.push_back(*new_trace_it);
    }
    for(auto traceIt = trace.begin(); traceIt != trace.end(); traceIt++){
        errs()<<"trace:"<<**traceIt<<'\n';
    }
    for(Instruction *inst : new_trace){
        if(inst->isTerminator()){
            BranchInst *branch = dyn_cast<BranchInst>(inst);
            assert(branch != nullptr);

            if(branch->isConditional()){
                SENode *brch = getNodeOrBuild(seq, branch);

                SliceBlock *host = so->getSliceHost(branch);
                assert(host != nullptr);

                SliceBlock *next = so->getSliceHost(trace.at(seq + 1));
                assert(next != nullptr);

                BasicBlock *tval = branch->getSuccessor(0);
                BasicBlock *fval = branch->getSuccessor(1);
                // errs()<<"*************************\n";
                // errs()<<"next:"<<*next->getBlock();
                // errs()<<"tval:"<<*tval;
                // errs()<<"fval:"<<*fval;

                if(host->inSTab(next, tval) && host->inSTab(next, fval)){
                    BasicBlock * theOne = next->getBlock();
                    if(tval == theOne)
                        conds.insert(make_pair(brch, 0));
                    else if(fval == theOne)
                        conds.insert(make_pair(brch, 1));
                    else{
                        errs()<<*branch<<'\n';
                        assert(false && "next not in the trace");
                    }
                }

                else if(host->inSTab(next, tval)){
                    conds.insert(make_pair(brch, 0));
                    // errs()<<"tval is successor of next\n";
                }

                else {
                    conds.insert(make_pair(brch, 1));
                    // errs()<<"fval is successor of next\n";
                }
            }
        }
        else {
            //errs()<<"seq:"<<seq<<" "<<*inst<<'\n';
            SENode * a = getNodeOrBuild(seq, inst);
        }
        // go to the next inst in list
        seq++;
    }

    //trimGraph();
}
void SEGraph::followTrace() {
    int seq = 0;
    Instruction* lastInst = trace.back();
    for(Instruction *inst : trace){
        // steped on a condition or a connection
        if(inst->isTerminator()){
            BranchInst *branch = dyn_cast<BranchInst>(inst);
            assert(branch != nullptr);
      
            if(branch->isConditional()){
                SENode *brch = getNodeOrBuild(seq, branch);
        
                SliceBlock *host = so->getSliceHost(branch);
                assert(host != nullptr);
        
                SliceBlock *next = so->getSliceHost(trace.at(seq + 1));
                assert(next != nullptr);
        
                BasicBlock *tval = branch->getSuccessor(0);
                BasicBlock *fval = branch->getSuccessor(1);
                // errs()<<"*************************\n";
                // errs()<<"next:"<<*next->getBlock();
                // errs()<<"tval:"<<*tval;
                // errs()<<"fval:"<<*fval;
        
                if(host->inSTab(next, tval) && host->inSTab(next, fval)){
                    //errs()<<"Warn:Found an conditional variable whose T and F branches are both in scope\n";
                }
        
                else if(host->inSTab(next, tval)){
                    conds.insert(make_pair(brch, 0));
                }
        
                else {
                    conds.insert(make_pair(brch, 1));
                }
            }
        }
        // normal instrucitons
        else {
            //errs()<<"building SEG seq:"<<seq<<" "<<*inst<<'\n';
            if(inst == lastInst){
                int seq1;
                CallInst* callI = cast<CallInst>(inst);
                assert(locateValue(seq, callI, seq1));
                assert(seq1<= seq);
                auto key = make_pair(seq,callI);
                SENode * specialNode = buildSpecial(seq1,callI);
                nodes.insert(make_pair(key,specialNode));
                //errs() <<str_nodetype(specialNode)<<'\n';
            }
            else{
                SENode * a = getNodeOrBuild(seq, inst);
                //errs() <<str_nodetype(a)<<'\n';
            }
                
            
    
        }
        // go to the next inst in list
        seq++;
    }
}

#define MAX_COND_TRACES 100
static void DFS(SENode* cur,vector<SENode*> dependency,vector<vector<SENode*> >* dependencies){
    if(dependencies->size() > MAX_COND_TRACES){
        return;
    }
    dependency.push_back(cur);
    if(!isa<SENodeUnknown>(cur)){
        dependency.pop_back();// pop the node which is the first node that is not SENodeUnown
        dependencies->push_back(dependency);
    }else{
        //displayBasic1(cur);
        for(auto depB = cur->depBegin();depB != cur->depEnd();depB++){
            DFS(*depB,dependency,dependencies);
        }
    }
}

void SEGraph::trimGraph() {
  set<SENode *> deps;

  // collect init set
  SENode *term = getNode(trace.size() - 1, trace.back());
  assert(term != nullptr);
  collectDeps(term, deps);

  for(auto &i : conds){
    SENode *cond = i.first;
    collectDeps(cond, deps);
  }

  // expand the set
  bool changed = true;
  while(changed) {
    changed = false;

    for(auto &i : nodes){
      SENode *node = i.second;
      //if this node is IN deps, then move on
      if(deps.find(node) != deps.end()){
        continue;
      }
      SENode::linkIter di = node->depBegin(), de = node->depEnd();
      //iterate this node's dependent
      for(; di != de; ++di){
        // expand the set only when any of the dependency of node i is also
        // in the initially collected deps set.
        if(deps.find(*di) != deps.end()){
          collectDeps(node, deps);
          changed = true;
          break;
        }
      }

      if(changed){
        break;
      }
    }
  }

  // delete non-related nodes
  set<SENode *> dels;
  for(auto &i : nodes){
    SENode *node = i.second;
    if(deps.find(node) == deps.end()){
      dels.insert(node);
    }
  }

  for(SENode *node : dels){
      deleteNode(node);
  }
}
static const map<string, vector<int> > unhandleParasLibCall({
    {string("llvm_objectsize_i64_p0i8"),             {0}},
    {string("__get_free_pages"),                     {0}},
    {string("__kmalloc"),                            {0}},
    {string("__kmalloc_track_caller"),               {0}},
    {string("__vmalloc"),                            {0}},
    {string("krealloc"),                            {0}},
    {string("kvmalloc_node"),                       {0}},
    {string("vmalloc"),                            {0}},
    {string("vzalloc"),                            {0}},
    {string("kmem_cache_alloc_trace"),                            {0,1,2}},
    {string("kmemdup"),                            {0,1}},
    {string("__copy_user_flushcache"),              {1,2}},
    {string("__copy_user_nocache"),                 {1,2}},
    {string("_copy_from_user"),                     {1,2}},
    {string("memdup_user"),                         {0,1}},
    {string("strcmp"),                            {0,1}},
    {string("strlen"),                            {0}},
    {string("strncmp"),                            {0,1,2}},
    {string("strnlen"),                            {0}},
    {string("strnlen_user"),                            {0}}
});
static const map<string, vector<int> > unhandleParasAsmCall({
    {string("call __get_user_${4:P}"),                            {0,1}}
});

static const map<string, int> libcallFetchDests({
    {string("__copy_user_flushcache"), 0},
    {string("__copy_user_nocache"), 0},
    {string("_copy_from_user"), 0},
    {string("memdup_user"), -1},
    {string("memdup_user_nul"), -1}
});
static const map<string, int > asmcallFetchDests({
    {string("call __get_user_${4:P}"), -1}
});

bool SEGraph::externalDependency(SENode* UBBrNode,set<int>&extPara){
    //BranchInst* bugTrigger = cast<BranchInst>(cast<SENodeBranch>(UBBrNode)->getVal());
    set<SENode *> deps;
    collectDeps(UBBrNode,deps);
    //displayForDeps(deps);
    expandDep2(deps);
    bool externDep = false;
    for(set<SENode *>::iterator eachNode = deps.begin();eachNode != deps.end();eachNode++){
        if(SENodeParam * para = dyn_cast<SENodeParam>(*eachNode)){
            extPara.insert(para->getCastedVal()->getArgNo());
        }
        else if(SENodeGlobal * glb = dyn_cast<SENodeGlobal>(*eachNode)){
            externDep = true;
        }
    }
    return externDep;
}
void SEGraph::expandDep2(set<SENode *> &deps){
    // expand the set
    bool changed = true;
    while(changed) {
        changed = false;
        for(auto &i : nodes){
            SENode *node = i.second;
            //if this node is IN deps, then move on
            if(deps.find(node) != deps.end()){
                continue;
            }
            SENode::linkIter di = node->depBegin(), de = node->depEnd();
            //iterate this node's dependent
            for(; di != de; ++di){
                // expand the set only when any of the dependency of node i is also
                // in the initially collected deps set.
                // and this dependent is a pointer
                if(deps.find(*di) != deps.end() && (*di)->getVal()->getType()->isPointerTy()){
                    if(isa<SENodeLoad>(node))
                        continue;
                    //this is a fetch function
                    if(isa<SENodeCall>(node) || isa<SENodeAsm>(node)){
                        if(Fetch::isfetch(cast<CallInst>(node->getVal())) != nullptr){
                            deps.insert(node);
                            changed = true;
                            break;
                        }
                    }
                    collectDeps(node,deps);
                    changed = true;
                    break;
                }
            }
            if(changed){
                break;
            }
        }
    }
}
void SEGraph::expandDep3(set<SENode *> &deps){
    // expand the set
    bool changed = true;
    while(changed) {
        changed = false;
        for(auto &i : nodes){
            SENode *node = i.second;
            //if this node is IN deps, then move on
            if(deps.find(node) != deps.end()){
                continue;
            }
            SENode::linkIter di = node->depBegin(), de = node->depEnd();
            //iterate this node's dependent
            for(; di != de; ++di){
                // expand the set only when any of the dependency of node i is also
                // in the initially collected deps set.
                //if(deps.find(*di) != deps.end() ){//&& (*di)->getVal()->getType()->isPointerTy()){
                if(deps.find(*di) != deps.end() && !isa<Constant>((*di)->getVal())){
                    errs()<<"new:"<<*node->getVal()<<'\n';
                    int size = deps.size();
                    set<SENode*> oldset = deps;
                    collectDeps(node, deps);
                    size = deps.size() - size;

                    set<SENode*> sub;
                    for(auto eachnew = deps.begin();eachnew != deps.end();eachnew++){
                        if(oldset.find(*eachnew) == oldset.end())
                            sub.insert(*eachnew);
                    }
                    //displayForDeps(sub);
                    errs()<<"increase "<<size<<"\n\n";
                    //deps.insert(node);
                    changed = true;
                    break;
                }
            }
            if(changed){
                break;
            }
        }
    }
}
bool SEGraph::fetchTriggerabilityScan(SENode* triggerNode) {
    //BranchInst* bugTrigger = cast<BranchInst>(cast<SENodeBranch>(triggerNode)->getVal());
    set<SENode *> deps;
    collectDeps(triggerNode,deps);
    expandDep2(deps);
    for(set<SENode*>::iterator depIt = deps.begin();depIt != deps.end();depIt++){
        Value* v = (*depIt)->getVal();
        if(CallInst* ci = dyn_cast<CallInst>(v)){
            const FetchDef * fth = Fetch::isfetch(ci);
            if(fth == nullptr)
                continue;
            int fetchDstIdx = fth->dst;
            if(fetchDstIdx > -2){
                Value* fetchDst = nullptr;
                if(fetchDstIdx == -1)
                    fetchDst = ci;
                else
                    fetchDst = ci->getArgOperand(fetchDstIdx);
                SENode* fetchDstNode = getNodeProbe(fetchDst);
                if(fetchDstNode != nullptr && deps.find(fetchDstNode) != deps.end())
                    return true;
            }
        }
    }
    return false;
}
bool SEGraph::fetchTriggerabilityScan(vector<SENode*>& triggerNode) {
    bool ret = false;
    for(auto eachTrigger = triggerNode.begin() ; eachTrigger != triggerNode.end(); eachTrigger++){
        ret |= fetchTriggerabilityScan(*eachTrigger);
    }
    return ret;
}
void SEGraph::filterTrace(iseq &filt) {
  int len = trace.size();
  for(int c = 0; c < len; c++){
    Instruction *i = trace.at(c);

    // check if the stmt is in SEG
    SENode *node = getNodeOrNull(c, i);
    if(node == nullptr){
      continue;
    }

    // replay the node
    filt.push_back(i);
  }
}
SEGraph::SEGraph(SliceOracle *s, iseq t,Function& f)
: so(s), trace(t), count(0),func(f) {
    followTrace();
    trimGraph();
    highest_seq = get_highest_seq();
}
SEGraph::SEGraph(SliceOracle *s, iseq t,map<SENode *, int>& oldConds,Function & f):
so(nullptr),trace(t),count(0),func(f){
    int seq = 0;
    map<BranchInst*,int> alreadyPorcessed;
    for(auto oldCond = oldConds.begin(); oldCond != oldConds.end();oldCond++) {
        SENode* old_node = oldCond->first;
        Value* old_inst = old_node->getVal();
        alreadyPorcessed.insert(make_pair(cast<BranchInst>(oldCond->first->getVal()),oldCond->second));
    }
    for(Instruction *inst : trace){
        if(inst->isTerminator()){
            BranchInst *branch = dyn_cast<BranchInst>(inst);
            if(branch == nullptr)
            {
                abort = true;
                return;
            }
            if(branch->isConditional()){
                SENode *brch = getNodeOrBuild(seq, branch);
                auto is_processed = alreadyPorcessed.find(branch);
                if(is_processed != alreadyPorcessed.end()) {
                    conds.insert(make_pair(brch,is_processed->second));
                    seq++;
                    continue;
                }
                SliceBlock *host = s->getSliceHost(branch);
                if(host == nullptr){
                    seq++;
                    continue;
                }
                SliceBlock *next = s->getSliceHost(trace.at(seq + 1));
                if(next == nullptr){
                    seq++;
                    continue;
                }
                BasicBlock *tval = branch->getSuccessor(0);
                BasicBlock *fval = branch->getSuccessor(1);

                if(host->inSTab(next, tval) && host->inSTab(next, fval)){
                    //assert(sbtrace.find(next)!= sbtrace.end());
                    BasicBlock * theOne = next->getBlock();
                    if(tval == theOne)
                        conds.insert(make_pair(brch, 0));
                    else if(fval == theOne)
                        conds.insert(make_pair(brch, 1));
                    else{
                        errs()<<*branch<<'\n';
                        assert(false && "next not in the trace");
                    }
                }
                else if(host->inSTab(next, tval)){
                    conds.insert(make_pair(brch, 0));
                }
                else {
                    conds.insert(make_pair(brch, 1));
                }
            }
        }
        else {
            SENode * a = getNodeOrBuild(seq, inst);
        }
        seq++;
    }
    trimGraph();
    highest_seq = get_highest_seq();
}

void SEGraph::copy(map<int,SENode*>& newTrace,SEGraph* caller_graph){
    //displayForUsrs();
    // inherent the updated_value

    // since the caller_graph is a newly built SEG, it shouldn't have
    // any updated_values
    assert(caller_graph->updated_values.size() == 0);
    caller_graph->updated_values.insert(updated_values.begin(),updated_values.end());

    //re-oraganize the sequence
    int caller_traceLen = caller_graph->trace.size();
    for(int nodeId = 0 ; nodeId < count; nodeId++){
        SENode* curNode = getNodeById(nodeId);
        if(curNode == nullptr)
            continue;
        SENode* newNode = curNode->copy(caller_graph);
        //adjust the seq according to the concatenation
        int seq = newNode->getSeq();
        if(seq >= 0){
            int new_seq = caller_traceLen + seq;
            newNode->setSeq(new_seq);
        }
        newTrace.insert(make_pair(nodeId,newNode));
    }
    // need to refine the usrs/deps
    for(int nodeId = 0 ; nodeId < count; nodeId++){
        if(newTrace.find(nodeId) == newTrace.end())
            continue;
        SENode* curNode = newTrace.at(nodeId);
        //deal with usrs field
        set<SENode*> newUsrs;
        int oldUsrSize = curNode->numUsrs();
        for(auto usrIt = curNode->usrBegin();usrIt != curNode->usrEnd(); usrIt++){
            SENode* usr = *usrIt;
            int usrId = usr->getId();
            assert(newTrace.find(usrId)!= newTrace.end());
            newUsrs.insert(newTrace[usrId]);
            //curNode->replace(usr,newTrace[usrId]);
        }
        curNode->usrs.clear();
        assert(curNode->usrs.size() == 0);
        assert(newUsrs.size() == oldUsrSize);
        for(auto newUsrIt = newUsrs.begin(); newUsrIt != newUsrs.end();newUsrIt++)
            curNode->addUsr(*newUsrIt);
        //deal with deps
        set<SENode*> newDeps;
        int oldDepSize = curNode->numDeps();
        for(auto depIt = curNode->depBegin();depIt != curNode->depEnd(); depIt++){
            SENode* dep = *depIt;
            int depId = dep->getId();
            assert(newTrace.find(depId)!=newTrace.end());
            assert(newTrace.find(depId) != newTrace.end());
            newDeps.insert(newTrace[depId]);
            // if it's an inst node,
            //need to refine the opvs as well
            if(SENodeInst* instNode = dyn_cast<SENodeInst>(curNode)){
                instNode->replace(dep,newTrace[depId]);
            }
        }
        curNode->deps.clear();
        assert(curNode->deps.size() == 0);
        assert(newDeps.size() == oldDepSize);
        for(auto newDepIt = newDeps.begin();newDepIt != newDeps.end();newDepIt++)
            curNode->addDep(*newDepIt);
    }
    return;
}
void SEGraph::verify(){
  for(auto nodeIt = nodes.begin(); nodeIt != nodes.end();nodeIt++){
    SENode* node = nodeIt->second;
    for( SENode* dep : node->deps){
      assert(dep->usrs.find(node) != dep->usrs.end());
    }
    for(SENode* usr : node->usrs){
      assert(usr->deps.find(node) != usr->deps.end());
    }
  }
}

bool SEGraph::unhandledNode(){
  int len = trace.size();
  for(int c = 0; c < len; c++) {
      Instruction *i = trace.at(c);
      SENode *node = getNodeOrNull(c, i);
      if (node == nullptr) {
          continue;
      }
      if(isa<BranchInst>(i)){
          if(isa<SENodeUnknown>(node)){
              return true;
          }
      }
  }

  return false;
}

void SEGraph::displayDepForNode(SENode*node){
    set<SENode*> deps;
    collectDeps(node,deps);
    displayForDeps(deps);
}
int SEGraph::get_highest_seq(){
    int ret = -1;
    for(auto node_it = nodes.begin(); node_it != nodes.end();node_it++){
        ret = node_it->first.first > ret? node_it->first.first : ret;
    }
    return ret;
};

int SEGraph::mergeSEG(SEGraph* calleeGraph,CallInst* callsite,set<int>& oldDeps){
    //merge constants and other staff first
    /*debug purpose
    if(callsite->getCalledFunction() != nullptr){
        string calledFuncName = callsite->getCalledFunction()->getName().str();
        string callerFuncName = callsite->getFunction()->getName().str();
        if(calledFuncName == "__hid_request" && callerFuncName=="lenovo_features_set_tpkbd"){
            __asm__("nop");
            displayForDeps();
        }
    }*/
    int ret = 1;
    SENode * callSiteRawNode = getNodeProbe(callsite,false);
    SENodeSpecialCall* callSiteNode = cast<SENodeSpecialCall>(callSiteRawNode);
    map<int,SENode*> newCalleeSEG;
    calleeGraph->copy(newCalleeSEG,this);
    set<int> callSitesParameterIdxs;
    set<int> usedCalleParameters;
    for(int i = 0 ; i < callsite->getNumArgOperands();i++)
        callSitesParameterIdxs.insert(i);
    // add callee's SEG to the current(caller's SEG)
    for(map<int,SENode*>::iterator newCalleeNodeIt = newCalleeSEG.begin(); newCalleeNodeIt != newCalleeSEG.end();newCalleeNodeIt++){
        int oldId = newCalleeNodeIt->first;
        SENode* curNode = newCalleeNodeIt->second;
        Value* curVal = curNode->getVal();
        int curSeq = curNode->getSeq();
        if(curSeq >= 0){
            if( !(isa<SENodeLocal>(curNode) || isa<SENodeInst>(curNode))){
                errs()<<"unhandled merged node:";
                errs()<<*curNode->getVal()<<'\n';
                calleeGraph->displayForDeps();
                assert(false);
            }
            // special care needs to be taken if it's a conditional var
            if(isa<BranchInst>(curVal)){
                SENode* oldCondNode = calleeGraph->getNodeById(oldId);
                int br = calleeGraph->getCond(oldCondNode);
                if(br != -1){
                    conds.insert(make_pair(curNode,br));
                }else{
                    // branch whose T/F successors are both in this trace
                    //calleeGraph->displayConds();
                    //errs()<<"missing branch:"<<*curVal<<'\n';
                    //assert(false);
                }
            }
            // due to the trim graph,
            // the seq might not be the index of this intruction in the trace
            trace.push_back(cast<Instruction>(curVal));
            addNode(curNode);
            //errs()<<"highest_seq:"<<highest_seq()<<"|trace len - 1:"<<trace.size() - 1<<'\n';
            //__asm__("nop");
        }
        // if this is not an instruction
        if(curSeq < 0){
            // if it's an argument, then need to replace it.
            if(SENodeParam* argNode = dyn_cast<SENodeParam>(curNode)){
                Argument* arg = cast<Argument>(curVal);
                int argIdx = arg->getArgNo();
                usedCalleParameters.insert(argIdx);
                Value * para = callsite->getArgOperand(argIdx);
                SENode* new_para_node = getNodeProbe(para,false);
                assert(new_para_node != nullptr);
                if(isa<SENodeUnknown>(new_para_node)){
                    // new para is affected by unknown instruction, abort this trace
                    ret = -1;
                }
                // update its relations
                for(auto oldArgUserIt = curNode->usrBegin(); oldArgUserIt != curNode->usrEnd();oldArgUserIt++){
                    SENodeInst* oldArgUser = cast<SENodeInst>(*oldArgUserIt);
                    oldArgUser->replace(curNode,new_para_node);
                    oldArgUser->delDep(curNode);
                    oldArgUser->addDep(new_para_node);

                    new_para_node->addUsr(oldArgUser);
                    //curNode->delUsr(oldArgUser)
                }
                if(para != arg){
                    updated_values[arg] = para;
                }
                curNode->usrs.clear();
                delete(curNode);
                continue;
            }
            // if it's another leaf node which this SEG already has
            // then no need to add it
            pair<int,Value*> key = make_pair(curSeq,curVal);
            if(nodes.find(key) != nodes.end()){
                //there is a leaf node
                SENode* newLeaf = nodes.at(key);
                SENode* oldLeaf = curNode;
                for(auto oldLeafUser = oldLeaf->usrBegin(); oldLeafUser != oldLeaf->usrEnd();oldLeafUser++){
                    SENodeInst* usrInOld = cast<SENodeInst>(*oldLeafUser);
                    usrInOld->replace(oldLeaf,newLeaf);
                    usrInOld->delDep(oldLeaf);
                    usrInOld->addDep(newLeaf);

                    //oldLeaf->delUsr(usrInOld);
                    newLeaf->addUsr(usrInOld);
                }
                delete(curNode);
                continue;
            }
            addNode(curNode);
        }
    }
    //delete the call site node
    deleteNode(callSiteNode);
    verify();
    highest_seq = get_highest_seq();
    return ret;
}
