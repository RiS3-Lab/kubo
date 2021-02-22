#ifndef SYMBOLIC_H_
#define SYMBOLIC_H_

#include "Project.h"
#include "Fetch.h"
//#include "llvm/Support/Casting.h"
// memory layout
#define PAGE_SIZE           4096                  // 4KB

#define STACK_SLOT_LEN      1024 * PAGE_SIZE         // 4MB
#define STACK_SLOT_NUM      128

#define EXTERN_SLOT_LEN     1024 * PAGE_SIZE      // 4MB
#define EXTERN_SLOT_NUM     128

#define HEAP_SLOT_LEN       1024 * PAGE_SIZE      // 4MB
#define HEAP_SLOT_NUM       128

// derived
#define STACK_BASE          PAGE_SIZE + STACK_SLOT_LEN
#define STACK_TERM          STACK_BASE + STACK_SLOT_LEN * STACK_SLOT_NUM

#define EXTERN_BASE         STACK_TERM + EXTERN_SLOT_LEN
#define EXTERN_TERM         EXTERN_BASE + EXTERN_SLOT_LEN * EXTERN_SLOT_NUM

#define HEAP_BASE           EXTERN_TERM + HEAP_SLOT_LEN
#define HEAP_TERM           HEAP_BASE + HEAP_SLOT_LEN * HEAP_SLOT_NUM

class SENode;
class SEGraph;
class SEOpv;

struct PointerStatus{
    int upperBound;
    int lowerBound = 0;
};

// classes
struct SymExpr {
  Z3_ast expr;
  // for now this buffer is only applied to memory allocated on the stack
  //TODO: apply this to memory on the heap
  PointerStatus allocStatus;

  SymExpr()
    : expr(nullptr) {}

  SymExpr(const SymExpr &other)
    : expr(other.expr), allocStatus(other.allocStatus) {}
  ~SymExpr(){ // buff is deleted before the function is finished.
  }
  void simplify(Z3_context ctxt) {
      // errs()<<"fundamental: before symexpr simplify\n";
    expr = Z3_simplify(ctxt, expr);
    Z3_error_code e = Z3_get_error_code(ctxt);
    if (e != Z3_OK)
        errs()<<Z3_get_error_msg(ctxt, e);
    // errs()<<"fundamental: after symexpr simplify\n";
  }

  void dump(Z3_context ctxt) {
    errs() << "expr: ";
    DUMP.typedExpr(ctxt, expr);
  }
};

struct SymCond {
  vector<Z3_ast> conds;

  SymCond()
    : conds() {}

  SymCond(const SymCond &other)
    : conds(other.conds) {}

  void simplify(Z3_context ctxt) {
    for(unsigned i = 0; i < conds.size(); i++){
      conds[i] = Z3_simplify(ctxt, conds[i]);
    }
  }

  void dump(Z3_context ctxt) {
    errs() << "cond: [\n";
    for(Z3_ast c : conds) {
      errs() << "\t";
      DUMP.typedExpr(ctxt, c);
    }
    errs() << "]\n";
  }
};


class SymVal {
  // represent a single value and the conditions it follows
  public:
    SymVal(SymExpr &e, SymCond &c)
      : expr(e), cond(c){}

    ~SymVal() {}

    SymExpr &getExpr() {
      return expr;
    }

    SymCond &getCond() {
      return cond;
    }

    bool ready() {
      return expr.expr != nullptr;
    }

    void simplify(Z3_context ctxt) {
      expr.simplify(ctxt);
      cond.simplify(ctxt);
    }

    void dump(Z3_context ctxt) {
      expr.dump(ctxt);
      cond.dump(ctxt);
    }


  protected:
    SymExpr expr;
    SymCond cond;
};

class SymVar {
  // represent all possible values of a variable
  public:
    SymVar() {}

    ~SymVar() {
      for(SymVal *i : vals) {
        delete i;
      }
    }

    void add(SymVal *i) {
      vals.insert(i);
    }

    typedef typename set<SymVal *>::iterator iterator;

    iterator begin() {
      return vals.begin();
    }

    iterator end() {
      return vals.end();
    }

    SymVal *getSingleVal() {
      assert(vals.size() == 1);
      return *vals.begin();
    }

    bool ready() {
      return getSingleVal()->ready();
    }

    void simplify(Z3_context ctxt) {
      SymVal *v = getSingleVal();
      if(v->ready()){
        v->simplify(ctxt);
      }
    }

    int getValsNum(){
        return vals.size();
    }
    void dump(Z3_context ctxt) {
      SymVal *v = getSingleVal();
      if(v->ready()){
        v->dump(ctxt);
      }
        // for (auto v: vals) {
        //     if(v->ready()){
        //         v->dump(ctxt);
        //     }
        // }
    }

  protected:
    set<SymVal *> vals;
};

enum  CheckResult {
  // solver return
  CK_SAT          = 0,
  CK_UNDEF        = 1,
  CK_UNSAT        = 2,
  // symbllization
  CK_UNREL        = 3,
  CK_SYMERR       = 4,
  CK_OUTBOUND     = 5,
};

enum  UBResult {
    // solver return
    UB_True         = 2,
    UB_Unsure       = 1,
    UB_False        = 0
};

class SymExec {
  public:
    SymExec(ModuleOracle &m);

    ~SymExec();

    void push_solver(){
        Z3_solver_push(ctxt, solver);
    }
    void pop_solver(){
        //errs()<<Z3_solver_get_num_scopes(ctxt,solver);
        Z3_solver_pop(ctxt, solver, 1);
    }
    // post_ubs

    // symbol
    Z3_symbol newSymbol() {
      return Z3_mk_int_symbol(ctxt, varcount++);
    }

    // machine float point sorts
    Z3_sort getMachFloatSort(unsigned width) {
        auto i = sfloats.find(width);
        if(i != sfloats.end()) {
            return i->second;
        }

        switch (width) {
        case 16:
            return Z3_mk_fpa_sort_16(ctxt);
        case 32:
            return Z3_mk_fpa_sort_32(ctxt);
        case 64:
            return Z3_mk_fpa_sort_64(ctxt);
        case 128:
            return Z3_mk_fpa_sort_128(ctxt);
        case 80:
            if (mo.getTriple().getOS() == Triple::Linux)
                return Z3_mk_fpa_sort(ctxt, /*ebits=*/15, /*significand=*/64);
            else if (mo.getTriple().getOS() == Triple::FreeBSD)
                return Z3_mk_fpa_sort_64(ctxt);
            else {
                llvm_unreachable("unknown OS type");
            }
        case 40:
            return Z3_mk_fpa_sort(ctxt, /*ebits=*/8, /*significand=*/31);
        default:
            errs() << width <<"\n";
            llvm_unreachable("unknown float point size");
        }
    }

    // machine integer sorts
    Z3_sort getPointerSort() {
      return SORT_Pointer;
    }

    Z3_sort getMachIntSort(unsigned width) {
      auto i = sints.find(width);
      if(i != sints.end()){
        return i->second;
      }

      Z3_sort res = Z3_mk_bv_sort(ctxt, width);
      sints.insert(make_pair(width, res));
      return res;
    }

    // sort checking
    Z3_sort getExprSort(Z3_ast expr) {
      return Z3_get_sort(ctxt, expr);
    }

    unsigned getExprSortWidth(Z3_ast expr) {
      Z3_sort srt = getExprSort(expr);
      if(Z3_get_sort_kind(ctxt, srt) != Z3_BV_SORT){
          DUMP.typedExpr(ctxt, expr);
      }
      assert(Z3_get_sort_kind(ctxt, srt) == Z3_BV_SORT);
      return Z3_get_bv_sort_size(ctxt, srt);
    }

    unsigned getExprSortSize(Z3_ast expr) {
      return getExprSortWidth(expr) / mo.getBits();
    }

    bool isBoolSort(Z3_ast expr) {
      return Z3_get_sort_kind(ctxt, getExprSort(expr)) == Z3_BOOL_SORT;
    }

    bool isIntSort(Z3_ast expr) {
      return Z3_get_sort_kind(ctxt, getExprSort(expr)) == Z3_INT_SORT;
    }

    bool isMachIntSort(Z3_ast expr) {
      return Z3_get_sort_kind(ctxt, getExprSort(expr)) == Z3_BV_SORT;
    }

    bool isPointerSort(Z3_ast expr) {
      return getExprSort(expr) == SORT_Pointer;
    }

    bool isMemSlotSort(Z3_ast expr) {
      return getExprSort(expr) == SORT_MemSlot;
    }

    bool isSameSort(Z3_ast expr1, Z3_ast expr2) {
      return getExprSort(expr1) == getExprSort(expr2);
    }

    bool isSameMachIntSort(Z3_ast expr1, Z3_ast expr2) {
      return isSameSort(expr1, expr2) && isMachIntSort(expr1);
    }

    bool isSameBoolSort(Z3_ast expr1, Z3_ast expr2) {
      return isSameSort(expr1, expr2) && isBoolSort(expr1);
    }

    // constant creation
    Z3_ast createConstBool(bool val) {
      return val ? Z3_mk_true(ctxt) : Z3_mk_false(ctxt);
    }

    Z3_ast createConstMachIntUnsigned(uint64_t val, unsigned width) {
      Z3_ast ret = Z3_mk_unsigned_int64(ctxt, val, getMachIntSort(width));
      return ret;
    }

    Z3_ast createConstMachBigIntUnsigned(string val, unsigned width) {
        Z3_ast ret = Z3_mk_numeral(ctxt, val.c_str(), getMachIntSort(width));
        return ret;
    }

    Z3_ast createConstMachIntSigned(int64_t val, unsigned width) {
      Z3_ast ret = Z3_mk_int64(ctxt, val, getMachIntSort(width));
      return ret;
    }

    Z3_ast createConstMachIntBool(bool val, unsigned width) {
      return val
        ? createConstMachIntSigned(-1, width)
        : createConstMachIntSigned( 0, width);
    }

    Z3_ast createConstPointer(uintptr_t val) {
      Z3_ast ret = Z3_mk_unsigned_int64(ctxt, val, SORT_Pointer);
      return ret;
    }

    Z3_ast createConstNull() {
      return createConstPointer(0);
    }

    Z3_ast createConstMachDataSeq(Value *val) {
        auto cseq = dyn_cast<ConstantDataSequential>(val);
        Type *ty = cseq->getElementType();
        IntegerType * ity = dyn_cast<IntegerType>(ty);
        assert(ity != nullptr);// currently just handle integer for convenience
        Z3_ast res = nullptr;
        // Recurisvely build aggregate z3_ast expr
        int numElement = cseq->getNumElements();
        for(int i = 0 ; i < numElement;i++)
        {
            Z3_ast cur = Z3_mk_unsigned_int64(ctxt, cseq->getElementAsInteger(i), getMachIntSort(mo.getTypeWidth(ity)));
            res = res != nullptr? Z3_mk_concat(ctxt, cur, res) : cur;
        }
        return res;
    }

    Z3_ast createConstMachStruct(Value *val);
    Z3_ast createConstMachGEP(Instruction * i);


    Z3_ast createConstMachStructZero(Value *val) {
        auto cagg = dyn_cast<ConstantAggregateZero>(val);
        Type *ty = cagg->getType();
        unsigned num_ele = cagg->getNumElements();
        Z3_ast res = nullptr;
        // Recurisvely build aggregate z3_ast expr
        for(unsigned idx = 0; idx < num_ele; ++idx){
            Value *tmp_val = cagg->getElementValue(idx);

            Type *nty = tmp_val->getType();
            Z3_ast cur;
            if(auto tmp_cagg = dyn_cast<ConstantAggregateZero>(tmp_val)){
                cur = createConstMachStructZero(tmp_cagg);
            }
            else if(auto tmp_cval = dyn_cast<ConstantInt>(tmp_val)){
                cur = createConstMachInt(tmp_cval);
                // find out the real width of the sub field
                if(isa<StructType>(ty)){
                //if(is_struct){
                    unsigned size = mo.getStructFieldSize(ty, idx);
                    if (getExprSortSize(cur) != size) {
                        cur = castMachIntZExt(cur, size * 8);
                    }
                }
            }
            else if(ConstantPointerNull * pcpn = dyn_cast<ConstantPointerNull>(tmp_val)){
                cur = createConstNull();
            }
            else {
                errs()<<"val:"<<*cagg<<'\n';
                errs()<<"val ty:"<<*ty<<'\n';
                errs()<<"idx:"<<idx<<'\n';
                errs()<<"tmp_Val"<<*tmp_val<<'\n';
                errs()<<"nty:"<<*nty<<'\n';
                DUMP.typedValue(val);
                llvm_unreachable("Unhandled aggregate type");
            }
            res = res != nullptr? Z3_mk_concat(ctxt, cur, res) : cur;
        }

        return res;
    }

    // special creation
    Z3_ast createSpecialNone() {
      return Z3_mk_string(ctxt, "NONE");
    }

    // llvm linkage
    Z3_ast createConstTypeSize(Type *ty) {
      return createConstPointer(mo.getTypeSize(ty));
    }

    Z3_ast createConstTypeWidth(Type *ty) {
      return createConstPointer(mo.getTypeWidth(ty));
    }

    Z3_ast createConstTypeOffset(Type *ty, unsigned fid) {
      return createConstPointer(mo.getTypeOffset(ty, fid));
    }

    Z3_ast createConstMachInt(ConstantInt *ci) {
      if(ci->getBitWidth() <= 64) {
        return createConstMachIntUnsigned(ci->getZExtValue(), ci->getBitWidth());
      } else {
        // constant with more than 64 bit width
        unsigned width = ci->getBitWidth();
        string num = ci->getValue().toString(/*radix=*/10, /*isSigned=*/true);
        return createConstMachBigIntUnsigned(num, width);
      }
    }

    Z3_ast createConstMachFloat(ConstantFP *fi) {
        uint64_t width = mo.getTypeWidth(fi->getType());
        switch(width) {
        case 32: {
            float v = fi->getValueAPF().convertToFloat();
            return Z3_mk_fpa_numeral_float(ctxt, v, getMachFloatSort(width));
        }
        case 64: {
            double v = fi->getValueAPF().convertToDouble();
            return Z3_mk_fpa_numeral_double(ctxt, v, getMachFloatSort(width));
        }
        default:
            llvm_unreachable("unsupported float type");
        }
    }

    Z3_ast createConstPointerFromInt(ConstantInt *ci) {
      return createConstPointer(ci->getZExtValue());
    }

    void prepareCalcOperation(
        Type *lty, Z3_ast lexp,
        Type *rty, Z3_ast rexp) {

      uint64_t lw = mo.getTypeWidth(lty);
      uint64_t rw = mo.getTypeWidth(rty);

      assert(lw == rw && lw == getExprSortWidth(lexp)
             && rw == getExprSortWidth(rexp));
    }

    void prepareFCalcOperation(
        Type *lty, Z3_ast lexp,
        Type *rty, Z3_ast rexp) {

        uint64_t lw = mo.getTypeWidth(lty);
        uint64_t rw = mo.getTypeWidth(rty);

        assert(lw == rw);
    }

    void prepareCmpOperation(
        Type *lty, Z3_ast lexp,
        Type *rty, Z3_ast rexp) {

      uint64_t lw = mo.getTypeWidth(lty);
      uint64_t rw = mo.getTypeWidth(rty);

      assert(lw == rw &&
          lw == getExprSortWidth(lexp) && rw == getExprSortWidth(rexp));
    }

    Z3_ast prepareFCmpOperation(
        Type *lty, Z3_ast lexp,
        Type *rty, Z3_ast rexp) {

        uint64_t lw = mo.getTypeWidth(lty);
        uint64_t rw = mo.getTypeWidth(rty);

        assert(lw == rw);

        // return if either operand is NAN
        Z3_ast res[2]; 
        res[0] = Z3_mk_fpa_is_nan(ctxt, lexp);
        res[1] = Z3_mk_fpa_is_nan(ctxt, rexp);
        return Z3_mk_or(ctxt, 2, res);
    }
    // if loading a FP val, convert `expr` from bv sort to fp sort
    void prepareIfLoadFP(LoadInst *li, SymExpr &expr) {
        Type *ty = li->getType();
        if (!ty->isFloatingPointTy()) return;
        expr.expr = castMachIntToFP(expr.expr, mo.getTypeWidth(ty));
    }

    // if storing a FP val, convert `expr` from fp sort to bv sort
    Z3_ast prepareIfStoreFP(Value *sv, SymExpr &expr) {
        Type *ty = sv->getType();
        if (!ty->isFloatingPointTy()) {
            return expr.expr;
        } else {
            return castFPToMachInt(expr.expr, mo.getTypeWidth(ty));
        }
    }

    // variable creation
    Z3_ast createVarMachInt(unsigned width) {
      return Z3_mk_const(ctxt, newSymbol(), getMachIntSort(width));
    }
    
    Z3_ast createVarMachFloat(unsigned width) {
        return Z3_mk_const(ctxt, newSymbol(), getMachFloatSort(width));
    }

    Z3_ast createVarPointer() {
      return Z3_mk_const(ctxt, newSymbol(), getPointerSort());
    }

    Z3_ast extractMachBitsVar(Z3_ast expr, unsigned saddr, unsigned width) {
        assert((saddr >= 0) && (saddr * 8 + width  <= getExprSortWidth(expr)));
        return Z3_mk_extract(ctxt, saddr * 8 + width - 1,  saddr * 8, expr);
    }

    Z3_ast replaceMachBitsVar(Z3_ast expr, unsigned saddr,
                              unsigned size, Z3_ast nexpr) {
        unsigned total_size = getExprSortSize(expr);
        assert((saddr >= 0) && (saddr + size  <= total_size));

        int prv_end = saddr * 8 - 1;
        // errs()<<"prv_end:"<< prv_end<<"\n";
        int nxt_start = (saddr + size) * 8;
        // errs()<<"nxt_start:"<< nxt_start<<"\n";
        Z3_ast prev =
            (prv_end > 0) ?
            Z3_mk_extract(ctxt, prv_end, 0, expr) :
            nullptr;
        // if(prev != nullptr){
        //     errs() << "prev: ";
        //     DUMP.typedExpr(ctxt, prev);
        // }
        Z3_ast next =
            (nxt_start < total_size * 8) ?
            Z3_mk_extract(ctxt, total_size * 8 - 1, nxt_start, expr) :
            nullptr;
        // if(next != nullptr){
        //     errs() << "next: ";
        //     DUMP.typedExpr(ctxt, next);
        // }

        Z3_ast new_val = nexpr;
        int new_size = getExprSortSize(nexpr);
        if (new_size != size){
            new_val = castMachIntZExt(nexpr, size*8); 
        }
        // errs() << "new val: ";
        // DUMP.typedExpr(ctxt, new_val);

        Z3_ast res =
            prev != nullptr ? Z3_mk_concat(ctxt, new_val, prev) : new_val;
        // if(res != nullptr){
        //     errs() << "res1: ";
        //     DUMP.typedExpr(ctxt, res);
        // }
        res =
            next != nullptr ? Z3_mk_concat(ctxt, next, res) : res;
        // if(res != nullptr){
        //     errs() << "res2: ";
        //     DUMP.typedExpr(ctxt, res);
        // }

        assert(getExprSortSize(res) == getExprSortSize(expr));
        return res;
    }

    // memory model
    Z3_ast incAndRetPtr(Z3_ast &ptr, unsigned len) {
      Z3_ast res = ptr;

      ptr = Z3_mk_bvadd(ctxt, ptr, createConstPointer(len));
      ptr = Z3_simplify(ctxt, ptr);

      return res;
    }

    Z3_ast createPtrStack() {
      return incAndRetPtr(sptr, STACK_SLOT_LEN);
    }

    Z3_ast createPtrExtern() {
      return incAndRetPtr(eptr, EXTERN_SLOT_LEN);
    }

    Z3_ast createPtrHeap() {
      return incAndRetPtr(hptr, HEAP_SLOT_LEN);
    }

    Z3_ast loadMemory(Z3_ast addr, unsigned size) {
      assert(isPointerSort(addr) && size != 0);
      Z3_ast res = Z3_mk_select(ctxt, memory, addr);

      Z3_ast idx, cur, par;
      for(unsigned i = 1; i < size; i++){
        idx = createConstPointer(i);
        cur = Z3_mk_bvadd(ctxt, addr, idx);
        par = Z3_mk_select(ctxt, memory, cur);
        res = Z3_mk_concat(ctxt, par, res);
      }
      return res;
    }

    void storeMemory(Z3_ast addr, Z3_ast expr) {
      // get the size to be overriden
      unsigned size = getExprSortSize(expr);
      storedMems.insert(make_pair(addr,size));
      Z3_ast idx, cur, par;
      for(unsigned i = 0; i < size; i++){
        idx = createConstPointer(i);
        cur = Z3_mk_bvadd(ctxt, addr, idx);
        par = Z3_mk_extract(ctxt,(i + 1) * mo.getBits() - 1, i * mo.getBits(),expr);
        memory = Z3_mk_store(ctxt, memory, cur, par);
      }
    }

    // casts
    Z3_ast prepareBitCast(Z3_ast expr, unsigned width) {
        Z3_ast res = expr;
        Z3_sort srt = getExprSort(expr);
        if (Z3_get_sort_kind(ctxt, srt) == Z3_FLOATING_POINT_SORT) {
            res = castFPToMachInt(expr, width);
        }
        return res;
    }

    Z3_ast castMachIntTrunc(Z3_ast expr, unsigned width) {
      return Z3_mk_extract(ctxt, width - 1, 0, expr);
    }

    Z3_ast castFPConvert(Z3_ast expr, unsigned width) {
      Z3_ast rm = Z3_mk_fpa_rtz(ctxt);
      return Z3_mk_fpa_to_fp_float(ctxt, rm, expr, getMachFloatSort(width));
    }
    
    Z3_ast castMachIntZExt(Z3_ast expr, unsigned width) {
      unsigned sz = getExprSortWidth(expr);
      return Z3_mk_zero_ext(ctxt, width - sz, expr);
    }

    Z3_ast castMachIntSExt(Z3_ast expr, unsigned width) {
      unsigned sz = getExprSortWidth(expr);
      return Z3_mk_sign_ext(ctxt, width - sz, expr);
    }

    Z3_ast castFPToMachInt(Z3_ast expr, unsigned width) {
      Z3_ast rm = Z3_mk_fpa_rtz(ctxt);
      return Z3_mk_fpa_to_sbv(ctxt, rm, expr, width);
    }
    
    Z3_ast castFPToMachUInt(Z3_ast expr, unsigned width) {
      Z3_ast rm = Z3_mk_fpa_rtz(ctxt);
      return Z3_mk_fpa_to_ubv(ctxt, rm, expr, width);
    }
    
    Z3_ast castMachIntToFP(Z3_ast expr, unsigned width) {
      Z3_ast rm = Z3_mk_fpa_rtz(ctxt);
      return Z3_mk_fpa_to_fp_signed(ctxt, rm, expr, getMachFloatSort(width));
    }
    
    Z3_ast castMachUIntToFP(Z3_ast expr, unsigned width) {
      Z3_ast rm = Z3_mk_fpa_rtz(ctxt);
      return Z3_mk_fpa_to_fp_unsigned(ctxt, rm, expr, getMachFloatSort(width));
    }
    
    Z3_ast castBoolToMachInt(Z3_ast expr, unsigned width) {
      assert(isBoolSort(expr));
      return Z3_mk_ite(ctxt, expr,
          createConstMachIntSigned(-1, width),
          createConstMachIntSigned(0, width));
    }

    Z3_ast castBoolToMachIntInversed(Z3_ast expr, unsigned width) {
      assert(isBoolSort(expr));
      return Z3_mk_ite(ctxt, expr,
          createConstMachIntSigned(0, width),
          createConstMachIntSigned(-1, width));
    }

    Z3_ast castMachIntToBool(Z3_ast expr) {
      unsigned sz = getExprSortWidth(expr);
      return Z3_mk_not(ctxt,
          Z3_mk_eq(ctxt, expr, createConstMachIntSigned(0, sz)));
    }

    Z3_ast castMachIntToBoolInversed(Z3_ast expr) {
      unsigned sz = getExprSortWidth(expr);
      return Z3_mk_eq(ctxt, expr, createConstMachIntSigned(0, sz));
    }

    Z3_ast castMachIntSwap(Z3_ast expr) {
      unsigned sz = getExprSortSize(expr);
      assert(sz % 2 == 0);
      Z3_ast res = Z3_mk_extract(ctxt, mo.getBits() - 1, 0, expr);

      Z3_ast par;
      for(unsigned i = 1; i < sz; i++){
        par = Z3_mk_extract(ctxt,
            (i + 1) * mo.getBits() - 1, i * mo.getBits(),
            expr);

        res = Z3_mk_concat(ctxt, res, par);
      }

      return res;
    }

    // solver
    void addAssert(Z3_ast expr) {
      Z3_solver_assert(ctxt, solver, expr);
    }

    Z3_ast_vector getAssertions() {
      Z3_solver_get_assertions(ctxt, solver);
    }


    void dumpAssertions() {
      Z3_ast_vector assertions = getAssertions();
      errs()<<"Solver assertions: "
            <<Z3_ast_vector_to_string(ctxt, assertions)
            <<"\n";
    }

    Z3_model getModel() {
      return Z3_solver_get_model(ctxt, solver);
    }
    //optimizer
    void copyAssertsToOptimizer(){
        Z3_ast_vector asserts = getAssertions();
        unsigned int assertNum = Z3_ast_vector_size(ctxt,asserts);
        for(unsigned int i = 0 ; i < assertNum; i ++){
            Z3_optimize_assert(ctxt,optimizer,Z3_ast_vector_get(ctxt,asserts,i));
        }
    }
    uint64_t optimizeMax(Z3_ast expr){
        return Z3_optimize_maximize(ctxt,optimizer,expr);
    }

    uint64_t optimizeMin(Z3_ast expr){
        return Z3_optimize_minimize(ctxt,optimizer,expr);
    }

    Z3_lbool optimizeCheck(){
        return Z3_optimize_check(ctxt,optimizer,0,0);
    }
    Z3_ast getOptimizedMax(uint64_t handle){
        return Z3_optimize_get_upper(ctxt, optimizer, handle);
    }
    Z3_ast getOptimizedMin(uint64_t handle){
        return Z3_optimize_get_lower(ctxt, optimizer, handle);
    }
    // context
    Z3_context getZ3Context() {
        return ctxt;
    }

    Z3_context getContext() {
      return ctxt;
    }

    ModuleOracle &getOracle() {
      return mo;
    }

    // SENode getter
    SymVar *getOrCreateVar(SENode *node, bool &exist) {
      auto i = cache.find(node);
      if(i != cache.end()){
        exist = true;
        return i->second;
      }

      exist = false;
      try {
          SymVar *var = new SymVar();
          cache.insert(make_pair(node, var));
          return var;
      }
      catch (const bad_alloc& e) {
          errs() << "allocation fails "<< e.what() <<"\n";
          return nullptr;
      }
    }

    SymVar *getVar(SENode *node) {
      auto i = cache.find(node);
      if(i == cache.end()){
        return nullptr;
      } else {
        return i->second;
      }
    }

    void simplify() {
      for(auto &i : cache){
        i.second->simplify(ctxt);
      }
    }


    void complete();

    // checks
    CheckResult checkOverlap(
        SymVar *src1, SymVar *len1,
        SymVar *src2, SymVar *len2);

    Z3_lbool checkOverlap(
        Z3_ast s1, Z3_ast l1,
        Z3_ast s2, Z3_ast l2);

    // verify if constraints added along the path is satisfiable
    CheckResult checkPathValid();
    // get the answer for this value if sat
    uint64_t getSatVal(SEGraph* seg, SENode* node);
    //3rd filter
    CheckResult nullifyFetchReachability(vector<pair<Fetch*,int>>, SEGraph *);
    bool nullifyFetchReachabilityScan(vector<pair<Fetch*,int>>, SEGraph*);

    //4th filter
    CheckResult nullifyGlobalParaReach(SEGraph *);
    //pass the abort message so that the program can continue
    void test(SEGraph*);
    bool abort;
    //display Z3_ast_kind, debug purpose
    void display_Z3_ast_kind(Z3_ast par);
    //readyness cache
    map<SEOpv*,bool> readyCache;
  protected:
    // module info
    ModuleOracle &mo;

    // z3 solver
    Z3_context ctxt;
    Z3_solver solver;
    Z3_optimize optimizer;

    // list of used sorts
    Z3_sort SORT_Pointer;
    Z3_sort SORT_MemSlot;
    Z3_sort SORT_MemBlob;


    // symbol counter
    int varcount;

    // the memory model
    Z3_ast memory;

    Z3_ast sptr;
    Z3_ast eptr;
    Z3_ast hptr;

    // machine integer sorts
    map<unsigned, Z3_sort> sints;

    // machine floating point sorts
    map<unsigned, Z3_sort> sfloats;

    // caches
    map<SENode *, SymVar *> cache;
    //stored memory
    map<Z3_ast,unsigned> storedMems;

};
#endif /* SYMBOLIC_H_ */
