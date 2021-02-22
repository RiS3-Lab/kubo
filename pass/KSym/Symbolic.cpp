#include "Project.h"
#include <llvm/IR/IntrinsicInst.h>
// main
SymExec::SymExec(ModuleOracle &m) 
  : mo(m) {
  // init Z3
  Z3_config cfg = Z3_mk_config();
  // 5s timeout for solver
  Z3_set_param_value(cfg, "timeout", "1000");
  ctxt = Z3_mk_context(cfg);
  Z3_del_config(cfg);

  solver = Z3_mk_solver(ctxt);
  Z3_solver_inc_ref(ctxt, solver);

  optimizer = Z3_mk_optimize(ctxt);
  Z3_optimize_inc_ref(ctxt, optimizer);
  // create sorts
  unsigned ptrsz = mo.getPointerWidth(); 
  SORT_Pointer = Z3_mk_bv_sort(ctxt, ptrsz);
  sints[ptrsz] = SORT_Pointer;

  SORT_MemSlot = Z3_mk_bv_sort(ctxt, mo.getBits());
  SORT_MemBlob = Z3_mk_array_sort(ctxt, SORT_Pointer, SORT_MemSlot);

  // symbol counter
  varcount = 0;

  // memory model
  memory = Z3_mk_const(ctxt,
      Z3_mk_string_symbol(ctxt, "memory"),
      SORT_MemBlob);

  sptr = Z3_mk_bvmul(ctxt, createVarPointer(), createConstPointer(PAGE_SIZE));
  addAssert(Z3_mk_bvuge(ctxt, sptr, createConstPointer(STACK_BASE)));

  eptr = Z3_mk_bvmul(ctxt, createVarPointer(), createConstPointer(PAGE_SIZE));
  addAssert(Z3_mk_bvuge(ctxt, eptr, createConstPointer(EXTERN_BASE)));

  hptr = Z3_mk_bvmul(ctxt, createVarPointer(), createConstPointer(PAGE_SIZE));
  addAssert(Z3_mk_bvuge(ctxt, hptr, createConstPointer(HEAP_BASE)));
}
Z3_ast SymExec::createConstMachGEP(Instruction * i){
    //create a ptr
    Z3_ast ret = createPtrExtern();
    return ret;
    //get the constant
    //TODO: read the content from the constant string
    //GetElementPtrInst * gep = dyn_cast<GetElementPtrInst>(i);
    //Value *ptr = gep->getPointerOperand();


    //Value *idx;
    //Type *cty, *nty;

}


Z3_ast SymExec::createConstMachStruct(Value *val) {
    auto cagg = dyn_cast<ConstantAggregate>(val);
    Type *ty = cagg->getType();
    unsigned num_ele;
    Z3_ast res = nullptr;

    // Recurisvely build aggregate z3_ast expr
    Constant::op_iterator cagg_i = cagg->op_begin();
    Constant::op_iterator cagg_e = cagg->op_end();
    for(unsigned idx = 0; cagg_i != cagg_e; ++cagg_i, ++idx){
        Value *tmp_val = *cagg_i;
        Type *nty = tmp_val->getType();
        Z3_ast cur = nullptr;
        //errs()<<"struct:"<<*val<<'\n'<<"tmp_val:"<<*tmp_val<<'\n';
        if(isa<BlockAddress>(tmp_val))
        {
            DUMP.typedValue(val);
            llvm_unreachable("Unhandled aggregate type");
        }
        else if(auto tmp_cagg = dyn_cast<ConstantAggregate>(tmp_val)){
            cur = createConstMachStruct(tmp_cagg);
        }
        else if(auto tmp_CData = dyn_cast<ConstantData>(tmp_val))
        {
            if(ConstantDataSequential * pCDS = dyn_cast<ConstantDataSequential>(tmp_CData)){
                cur = createConstMachDataSeq(pCDS);
            }
            else if(auto tmp_cval = dyn_cast<ConstantInt>(tmp_CData)){
                cur = createConstMachInt(tmp_cval);
                // find out the real width of the sub field
                if(auto cstruct = dyn_cast<ConstantStruct>(cagg)){
                    unsigned size = mo.getStructFieldSize(ty, idx);
                    if (getExprSortSize(cur) != size) {
                        cur = castMachIntZExt(cur, size * 8);
                    }
                }
            }
            else if(auto pCAZ = dyn_cast<ConstantAggregateZero>(tmp_val))
                cur = createConstMachStructZero(pCAZ);
            else if(isa<ConstantPointerNull>(tmp_val))
                cur = createConstNull();
            else if(isa<UndefValue>(tmp_val)){
                Type *ty = tmp_val->getType();
                uint64_t bits = mo.getTypeWidth(ty);
                cur = createVarMachInt(bits);
            }else{
                DUMP.typedValue(val);
                llvm_unreachable("Unhandled global type for ConstantData");
            }
        }
        else if(ConstantExpr * pCE = dyn_cast<ConstantExpr>(tmp_val))
        {
            switch(pCE->getOpcode()){
                case Instruction::GetElementPtr:{
                    // it's constant,
                    cur = createConstMachGEP(pCE->getAsInstruction());
                    break;
                }
                case Instruction::Trunc:
                case Instruction::ZExt:
                case Instruction::SExt:
                case Instruction::FPToUI:
                case Instruction::FPToSI:
                case Instruction::UIToFP:
                case Instruction::SIToFP:
                case Instruction::FPTrunc:
                case Instruction::FPExt:
                case Instruction::PtrToInt:
                case Instruction::IntToPtr:
                case Instruction::BitCast:
                {
                    Type* ty = pCE->getType();
                    cur = createConstMachIntUnsigned(0,mo.getTypeWidth(ty));
                    break;
                }
                default:
                    DUMP.typedValue(val);
                    llvm_unreachable("Unhandled constExpr in agg type");
                    break;
            }
        }
        else if(isa<GlobalValue>(tmp_val))
        {
            if(GlobalVariable * pGV = dyn_cast<GlobalVariable>(tmp_val)){
                // just borrow from SymVal *SENodeGlobal::derive(SymExec &sym)
                Type *ty = pGV->getType();
                assert(ty->isPointerTy());
                cur = createPtrExtern();
            }
            else if(Function * pgv = dyn_cast<Function>(tmp_val)) {
                cur = createConstPointer((uintptr_t) 0);
                //cur=createSpecialNone();
            }
            else if(isa<GlobalIndirectSymbol>(tmp_val)){
                DUMP.typedValue(val);
                llvm_unreachable("Unhandled aggregate type");
            }
            else{
                DUMP.typedValue(val);
                llvm_unreachable("Unhandled global type for aggregate");
            }
        }
        //else if(Instruction * i = dyn_cast<Instruction>(tmp_val)){
            //original ConstantExpr

        //}
        else {
            DUMP.typedValue(val);
            llvm_unreachable("Unhandled aggregate type");
        }
        res = res != nullptr? Z3_mk_concat(ctxt, cur, res) : cur;
    }
    return res;
}
SymExec::~SymExec() {
  // clean up cache
  for(auto const &i : cache){
    delete i.second;
  }
  // destroy z3
  Z3_solver_dec_ref(ctxt, solver);
  Z3_del_context(ctxt);
}

void SymExec::complete() {
  // memory model
  addAssert(Z3_mk_bvule(ctxt, sptr, createConstPointer(STACK_TERM)));
  addAssert(Z3_mk_bvule(ctxt, eptr, createConstPointer(EXTERN_TERM)));
  addAssert(Z3_mk_bvule(ctxt, hptr, createConstPointer(HEAP_TERM)));
}
void displayBasic(SENode * pnode)
{
    errs()<<pnode->getId()<<"|"<<pnode->getSeq()<<"|"<<*pnode->getVal()<<"\n";
    int usesize = pnode->deps.size();
    errs() <<"deps:"<<usesize<<"\n";
    SENode::linkIter ub = pnode->deps.begin();
    SENode::linkIter ue = pnode->deps.end();
    for(;ub!=ue;ub++)
        errs()<<"  "<<(*ub)->getId()<<"|"<<(*ub)->getSeq()<<"|"<<*(*ub)->getVal()<<"\n";
}
void displaySEG(SEGraph*seg){
    for(auto segIt = seg->begin(); segIt != seg->end(); segIt++){
        displayBasic(segIt->second);
    }
}

// symbolize the SEG
bool SEGraph::symbolize(SymExec &sym) {
  // symbolize all leaf and var nodes
  for(auto &i : nodes){
    SENode *node = i.second;
    //displayBasic(node);
    if(isa<SENodeLeaf>(node) || isa<SENodeVar>(node)){
      //errs()<<"leaf/var node: " << *node->getVal()<<"\n";
      //displayBasic(node);
      SymVar *var = node->getSymbol(sym);
    }
  }
  // iterate through the trace
  int len = trace.size();
  for(int c = 0; c < len; c++){
    Instruction *i = trace.at(c);
    SENode *node = getNodeOrNull(c, i);
    if(node == nullptr){
        continue;
    }

    // symbolize the node
    //errs()<<"inst node: " << *node->getVal()<<"\n";
    SymVar *var = node->getSymbol(sym);
    // if seed is affected by unhandled node
    if(c == len - 1){
        for(auto seedDepIt = node->depBegin();seedDepIt != node->depEnd();seedDepIt++){
            SENode* depNode = *seedDepIt;
            if(isa<SENodeUnknown>(depNode)){
                return true;
            }
        }
        assert(var == nullptr);
    }
    //if branch is affected by unhandled node
    if(isa<BranchInst>(i)){
        //errs()<<*i;
        if(var == nullptr){
        //    errs()<<"|unhandled\n";
            return true;
        }
    }
  }
  // finish up
  sym.complete();
  return false;
}
bool SEGraph::additional_constraints(SymExec& sym){

    for(auto &i: nodes){
        SENode* node = i.second;
        if(in_original_node_set(node))
            continue;
        if(isa<SENodeLeaf>(node) || isa<SENodeVar>(node)){
            SymVar *var = node->getSymbol(sym);
        }
    }
    int len = trace_back.size();
    int new_len = trace.size();
    for(int c = len;c < new_len; c++){
        Instruction* i = trace.at(c);
        //errs()<<*i<<'\n';
        SENode* node = getNodeOrNull(c,i);
        if(node ==nullptr)
            continue;
        SymVar* var = node->getSymbol(sym);
        if(isa<BranchInst>(i)){
            if(var == nullptr)
                return true;
        }
    }
    return false;
}
static inline void replayNode(SENode *node, 
    SymExec &sym, Z3_context ctxt, Z3_model model) {

#if defined(KSYM_DEBUG) && defined(KSYM_DEBUG_FUNC)
  SLOG.map();

  SLOG.log("inst", Helper::getValueRepr(node->getVal()));

  SymVar *svar = node->getSymbol(sym);
  if(svar == nullptr){
    SLOG.log("expr", "<unknown>");
    SLOG.log("eval", "<unknown>");
  } else {
    Z3_ast expr = svar->getSingleVal()->getExpr().expr;
    SLOG.log("expr", Helper::getExprRepr(ctxt, expr)); 

    Z3_ast eval;
    assert(Z3_model_eval(ctxt, model, expr, true, &eval) == Z3_TRUE);
    SLOG.log("eval", Helper::getExprRepr(ctxt, eval));
  }

  SLOG.pop();
#endif
}

void SEGraph::replay(SymExec &sym) {
  Z3_context ctxt = sym.getContext();
  Z3_model model = sym.getModel();

  // dump all var nodes
  for(auto &i : nodes){
    SENode *node = i.second;
    if(isa<SENodeVar>(node)){
      replayNode(node, sym, ctxt, model);
    }
  }

  // iterate through the trace
  int len = trace.size();
  for(int c = 0; c < len; c++){
    Instruction *i = trace.at(c);

    // check if the stmt is in SEG
    SENode *node = getNodeOrNull(c, i);
    if(node == nullptr){
      continue;
    }

    // replay the node
    replayNode(node, sym, ctxt, model);
  }
}


void SymExec::test(SEGraph *seg){
    push_solver();

    vector<pair<Value*,Z3_ast>> targets;
    for(auto node : *seg){
        SENode * pnode = node.second;
        if(isa<SENodeParam>(pnode)){
            errs()<<*node.first.second<<'\n';
            targets.push_back(make_pair(node.first.second,getVar(pnode)->getSingleVal()->getExpr().expr));
        }
    }

    Z3_ast blankSingle = createVarMachInt(8);
    Z3_ast y = createVarMachInt(8);
    Z3_ast z = Z3_mk_bvadd(ctxt,blankSingle,y);
    addAssert(Z3_mk_bvugt(ctxt,z, createConstMachIntUnsigned(4,8)));

    memory = Z3_mk_store(ctxt, memory, createConstPointer(0), blankSingle);

    Z3_ast copied = Z3_mk_select(ctxt,memory,createConstPointer(0));
    Z3_lbool result = Z3_solver_check(ctxt, solver);


    errs()<<Z3_ast_to_string(ctxt,blankSingle)<<'\n';
    errs()<<Z3_ast_to_string(ctxt,copied)<<'\n';
    Z3_ast single_eval;
    Z3_ast copied_eval;
    Z3_bool singleS = Z3_model_eval(ctxt,Z3_solver_get_model(ctxt,solver),blankSingle,true,&single_eval);
    Z3_bool boundedS = Z3_model_eval(ctxt,Z3_solver_get_model(ctxt,solver),copied,true,&copied_eval);

    errs()<<Z3_ast_to_string(ctxt,single_eval)<<'\n';
    errs()<<Z3_ast_to_string(ctxt,copied_eval)<<'\n';
    uint64_t single_int;
    uint64_t copied_int;
    Z3_get_numeral_uint64(ctxt,single_eval,&single_int);
    Z3_get_numeral_uint64(ctxt,copied_eval,&copied_int);

    errs()<< Z3_ast_to_string(ctxt,single_eval)<<' '<<single_int<<'\n';
    errs()<<Z3_ast_to_string(ctxt,copied_eval)<<' '<<copied_int<<"\n\n";

    /*
    Z3_ast blankInMemory = Z3_mk_select(ctxt,memory,targets[0].second);
    Z3_ast blankSingle = createVarMachInt(32);
    Z3_ast bounded = targets[1].second;



    Z3_lbool result = Z3_solver_check(ctxt, solver);

    Z3_ast memory_eval;
    Z3_ast single_eval;
    Z3_ast bounded_eval;
    Z3_bool memoryS = Z3_model_eval(ctxt,Z3_solver_get_model(ctxt,solver),blankInMemory,true,&memory_eval);
    Z3_bool singleS = Z3_model_eval(ctxt,Z3_solver_get_model(ctxt,solver),blankSingle,true,&single_eval);
    Z3_bool boundedS = Z3_model_eval(ctxt,Z3_solver_get_model(ctxt,solver),bounded,true,&bounded_eval);

    uint64_t memory_int;
    uint64_t single_int;
    uint64_t  bounded_int;
    Z3_get_numeral_uint64(ctxt,memory_eval,&memory_int);
    Z3_get_numeral_uint64(ctxt,single_eval,&single_int);
    Z3_get_numeral_uint64(ctxt,bounded_eval,&bounded_int);
    errs()<<Z3_ast_to_string(ctxt,memory_eval) <<" "<<memory_int<<'\n';
    errs()<< Z3_ast_to_string(ctxt,single_eval)<<' '<<single_int<<'\n';
    errs()<<Z3_ast_to_string(ctxt,bounded_eval)<<' '<<bounded_int<<"\n\n";

    copyAssertsToOptimizer();
    //uint64_t memory_handle_up = optimizeMax(blankInMemory);
    //uint64_t single_handle_up = optimizeMax(blankSingle);
    //uint64_t bounded_handle_up = optimizeMax(bounded);

    uint64_t memory_handle_min = optimizeMin(blankInMemory);
    uint64_t single_handle_min = optimizeMin(blankSingle);
    uint64_t bounded_handle_min = optimizeMin(bounded);

    Z3_lbool optSuccess = optimizeCheck();

    //Z3_ast opt_blank_memory_up = getOptimizedMax(memory_handle_up);
    //Z3_ast opt_blank_single_up = getOptimizedMax(single_handle_up);
    //Z3_ast opt_bounded_up = getOptimizedMax(bounded_handle_up);

    Z3_ast opt_blank_memory_min = getOptimizedMin(memory_handle_min);
    Z3_ast opt_blank_single_min = getOptimizedMin(single_handle_min);
    Z3_ast opt_bounded_min = getOptimizedMin(bounded_handle_min);

    //Z3_get_numeral_uint64(ctxt,opt_blank_memory_up,&memory_int);
    //Z3_get_numeral_uint64(ctxt,opt_blank_single_up,&single_int);
    //Z3_get_numeral_uint64(ctxt,opt_bounded_up,&bounded_int);

    //errs()<<Z3_ast_to_string(ctxt,opt_blank_memory_up) <<" "<<memory_int<<'\n';
    //errs()<< Z3_ast_to_string(ctxt,opt_blank_single_up)<<' '<<single_int<<'\n';
    //errs()<<Z3_ast_to_string(ctxt,opt_bounded_up)<<' '<<bounded_int<<"\n\n";

    Z3_get_numeral_uint64(ctxt,opt_blank_memory_min,&memory_int);
    Z3_get_numeral_uint64(ctxt,opt_blank_single_min,&single_int);
    Z3_get_numeral_uint64(ctxt,opt_bounded_min,&bounded_int);

    errs()<<Z3_ast_to_string(ctxt,opt_blank_memory_min) <<" "<<memory_int<<'\n';
    errs()<< Z3_ast_to_string(ctxt,opt_blank_single_min)<<' '<<single_int<<'\n';
    errs()<<Z3_ast_to_string(ctxt,opt_bounded_min)<<' '<<bounded_int<<'\n';


    Z3_solver_pop(ctxt, solver, 1);*/
}
// checks
CheckResult SymExec::checkPathValid() {

    Z3_lbool result = Z3_solver_check(ctxt, solver);

    switch (result) {
      case Z3_L_TRUE:
        return CK_SAT;

      case Z3_L_FALSE:
        return CK_UNSAT;

      case Z3_L_UNDEF:
        return CK_UNDEF;
    }

    llvm_unreachable("Should not reach here");
    return CK_SYMERR;
}


uint64_t SymExec::getSatVal(SEGraph* seg, SENode* node){

    Z3_ast to_eval = getVar(node)->getSingleVal()->getExpr().expr;
    Z3_ast evaled;
    Z3_bool if_sat = Z3_model_eval(ctxt,Z3_solver_get_model(ctxt,solver),to_eval,true,&evaled);

    errs()<<Z3_ast_to_string(ctxt,evaled)<<'\n';
    uint64_t evaled_int;
    Z3_get_numeral_uint64(ctxt,evaled,&evaled_int);
    return evaled_int;
}



void SymExec::display_Z3_ast_kind(Z3_ast par){
    Z3_ast_kind kind = Z3_get_ast_kind(ctxt,par);
    switch(kind){
        case Z3_NUMERAL_AST :{
            errs()<<"numeral\n";
            break;
        }
        case Z3_APP_AST:{
            errs()<<"app\n";
            break;
        }
        case Z3_VAR_AST:{
            errs()<<"var\n";
            break;
        }
        case Z3_QUANTIFIER_AST:{
            errs()<<"quantifier\n";
            break;
        }
        case Z3_SORT_AST:{
            errs()<<"sort\n";
            break;
        }
        case Z3_FUNC_DECL_AST:{
            errs()<<"func_decl\n";
            break;
        }
        case Z3_UNKNOWN_AST:{
            errs()<<"unknown ast\n";
            break;
        }
    }
}


/*
CheckResult SymExec::nullifyFetchReachability(vector<pair<Fetch *,int>> fetches, SEGraph * seg) {
    //concretize the memory with load
    map<uint64_t , int> memRange;
    Z3_ast evaled_ast;
    uint64_t evaled_int;
    Z3_model solved_model = Z3_solver_get_model(ctxt,solver);
    Z3_model_inc_ref(ctxt,solved_model);
    for(auto &conMem : storedMems){
        assert(Z3_model_eval(ctxt,solved_model,conMem.first,true,&evaled_ast));
        assert(Z3_get_numeral_uint64(ctxt,evaled_ast,&evaled_int));
        memRange.insert(make_pair(evaled_int,conMem.second));
    }
    //save context
    push_solver();
    for(auto &i:fetches){
        SENode * dst_senode = seg->getNodeOrFail(i.second,i.first->dst);
        assert(!isa<SENodeUndef>(dst_senode));//should never appear because it's defined in no where
        if(isa<SENodeUnknown>(dst_senode)){
            errs()<<"ERR:3rd filter: dst affected by unknown Node\n";
            return CK_UNDEF;
        }
        SymVar * dst_symvar = getVar(dst_senode);
        if(dst_symvar->getValsNum() != 1)
        {
            errs() << "Err:fetch's dest's sym values are not one\n";
            return CK_UNDEF;
        }
        Z3_ast dst_ast = dst_symvar->getSingleVal()->getExpr().expr;
        Value * len_val = i.first->len;
        uint64_t solved_len;
        if(ConstantInt * pCInt = dyn_cast<ConstantInt>(len_val)){
            solved_len = pCInt->getZExtValue();
        } else{
            if(i.first->dstType == dstHasType){
                solved_len = getExprSortSize(dst_ast);
            }
            else{
                SENode * len_SENode = seg->getNodeOrFail(i.second,len_val);
                assert(!isa<SENodeUndef>(dst_senode));//should never appear because it's defined in no where
                if(isa<SENodeUnknown>(len_SENode)){
                    errs()<<"ERR:3rd filter: len affected by unknown Node\n";
                    return CK_UNDEF;
                }
                Z3_ast len_ast = getVar(len_SENode)->getSingleVal()->getExpr().expr;
                // copy the all hard assertations(as what they are) from solver to optimizer
                copyAssertsToOptimizer();
                // we want to maximun this len
                uint64_t handle = optimizeMax(len_ast);
                Z3_lbool optSuccess = optimizeCheck();
                if(optSuccess == Z3_L_TRUE){
                    Z3_ast r = getOptimizedMax(handle);
                    assert(Z3_get_numeral_uint64(ctxt,r,&solved_len));
                }
                else if(optSuccess ==  Z3_L_FALSE){
                    errs() <<"ERR:solver proves solveable but optimizer produced unsolveable, use Z3_optimize_get_unsat_core to further investigate\n";
                    return CK_UNDEF;
                    llvm_unreachable("solver proves solveable but optimizer produced unsolveable, use Z3_optimize_get_unsat_core to further investigate");
                }
                else{
                    string errmsg(Z3_optimize_get_reason_unknown(ctxt,optimizer));
                    errs() << "ERR:optimizer return UNDEF:"<<errmsg<<'\n';
                    return CK_UNDEF;
                    llvm_unreachable("Opimizer returned UNDEF\n");
                }
            }
        }
        errs()<<"obtained len:"<<solved_len<<'\n';
        //sanity check, if the length for this fetched data is too long, sth is not right.
        //for(auto node: *seg){
        //    displayBasic1(node.second);
        //}
        //errs()<<"len var:"<<*len_val<<'\n';
        //assert(solved_len <= 5000);
        if(solved_len > 5000)
            return CK_OUTBOUND;
        Z3_ast poisened8bit = createConstMachBigIntUnsigned("165", mo.getBits());
        if(i.first->dstType == dstHasType) {
            //This case is solely for _get_user fuction in which the fetched data is an isolated symbolic value
            //this assumption will fail when the return value is actually a 64 bit vec
            assert(!isPointerSort(dst_ast));
            //direct assert the dst to null
            unsigned dst_ast_width = getExprSortWidth(dst_ast);
            unsigned byteWidth =  mo.getBits();
            for(unsigned offset = 0 ; offset < dst_ast_width;offset += byteWidth){
                addAssert(Z3_mk_eq(ctxt, poisened8bit, Z3_mk_extract(ctxt,offset+byteWidth - 1,offset,dst_ast)));
            }
        }else {
            assert(isPointerSort(dst_ast));
            assert(Z3_model_eval(ctxt,solved_model,dst_ast,true,&evaled_ast));
            assert(Z3_get_numeral_uint64(ctxt,evaled_ast,&evaled_int));
            vector<unsigned> intersection;
            for (auto &range : memRange){
                uint64_t h1 = range.first + range.second;
                uint64_t h2 = evaled_int + solved_len;// [...), left close, right open
                if(h1 > evaled_int && h2 > range.first){
                    // there is a overlap
                    for(unsigned idx = 0;idx<solved_len;idx++){
                        if ((evaled_int+idx)>= range.first and (evaled_int+idx) < h1)
                            intersection.push_back(idx);
                    }
                }
            }
            Z3_ast idx_ast, cur, par;
            for (unsigned idx = 0; idx < solved_len; idx++) {
                //offset
                if(find(intersection.begin(),intersection.end(),idx) != intersection.end())
                    continue;
                idx_ast = createConstPointer(idx);
                //cursor
                cur = Z3_mk_bvadd(ctxt, dst_ast, idx_ast);
                //memory pointed by cursor
                par = Z3_mk_select(ctxt, memory, cur);
                addAssert(Z3_mk_eq(ctxt, poisened8bit, par));
            }
        }
    }

    Z3_lbool result = Z3_solver_check(ctxt, solver);
    Z3_model_dec_ref(ctxt,solved_model);
    pop_solver();
    switch(result){
        case Z3_L_TRUE:
            return CK_SAT;

        case Z3_L_FALSE:
            return CK_UNSAT;

        case Z3_L_UNDEF:
            return CK_UNDEF;
    }

    llvm_unreachable("Should not reach here");
    return CK_SYMERR;
}
 */
#define MAX_DEPENDENCIES_TRACES 100
static void DFS_NODE(SENode* cur,vector<SENode*> dependency,vector<vector<SENode*>>* dependencies){
    if(dependencies->size() > MAX_DEPENDENCIES_TRACES){
        return;
    }
    dependency.push_back(cur);

    if(cur->numUsrs() == 0){
        dependencies->push_back(dependency);
    }else{
        for(auto userB = cur->usrBegin();userB != cur->usrEnd();userB++){
            DFS_NODE(*userB,dependency,dependencies);
        }
    }
}
/*
bool SymExec::nullifyFetchReachabilityScan(vector<pair<Fetch *,int>> fetches, SEGraph * seg) {
    for(auto &i:fetches){
        SENode * dst_senode = seg->getNodeOrFail(i.second,i.first->dst);
        if(i.first->dstType == dstHasType){
            // the fetched data is a symbolized value
            Value* fetchedData = i.first->dst;
            assert(!isa<PointerType>(fetchedData->getType()));
            SENode* fetchedDataNode = seg->getNodeProbe(fetchedData);
            assert(fetchedDataNode != nullptr && "fetched pointer is not mapped to SEG\n");
            vector<vector<SENode*>> dependencies;
            vector<SENode*> dependency;
            DFS_NODE(fetchedDataNode,dependency,&dependencies);
            for(vector<vector<SENode*>>::iterator eachDep = dependencies.begin();eachDep != dependencies.end();eachDep++){
                bool flowToCond = false;
                SENode* lastNode = eachDep->back();
                if(seg->isRealCond(lastNode))
                    flowToCond = true;
                if(flowToCond)
                    return true;
            }
        }else if(i.first->dstType == dstNoType){
            // the fetched data is a pointer
            Value* fetchedData = i.first->dst;
            assert(isa<PointerType>(fetchedData->getType()));
            Value * strippedFetchedData = fetchedData->stripPointerCasts();
            if(strippedFetchedData == fetchedData)
                assert(false && "pointer cannot be stripped\n");
            errs()<<*fetchedData<<'\n';
            errs()<<*strippedFetchedData<<'\n';
            SENode* fetchedDataNode = seg->getNodeProbe(fetchedData);
            SENode * strippedFetchedDataNode = seg->getNodeProbe(strippedFetchedData);
            assert(fetchedDataNode != nullptr && "fetched pointer is not mapped to SEG\n");
            assert(strippedFetchedDataNode != nullptr && "stripped pointer is not mapped to SEG\n");

            vector<vector<SENode*>> dependencies;
            vector<SENode*> dependency;
            DFS_NODE(fetchedDataNode,dependency,&dependencies);
            //displaySEG(seg);
            for(vector<vector<SENode*>>::iterator eachDep = dependencies.begin();eachDep != dependencies.end();eachDep++){
                bool hasLoad = false;//load the fetched data into llvm register
                bool flowToCond = false;
                for(vector<SENode*>::iterator eachNode = eachDep->begin();eachNode != eachDep->end();eachNode++){
                    if(isa<LoadInst>((*eachNode)->getVal()))
                        hasLoad = true;
                }
                SENode* lastNode = eachDep->back();
                if(seg->isRealCond(lastNode))
                    flowToCond = true;
                if(hasLoad && flowToCond)
                    return true;
            }

            vector<vector<SENode*>> strippedDependencies;
            vector<SENode*> strippedDependency;
            DFS_NODE(strippedFetchedDataNode,strippedDependency,&strippedDependencies);
            for(vector<vector<SENode*>>::iterator eachDep = strippedDependencies.begin();eachDep != strippedDependencies.end();eachDep++){
                bool hasLoad = false;
                bool flowToCond = false;
                for(vector<SENode*>::iterator eachNode = eachDep->begin();eachNode != eachDep->end();eachNode++){
                    if(isa<LoadInst>((*eachNode)->getVal()))
                        hasLoad = true;
                }
                SENode* lastNode = eachDep->back();
                if(seg->isRealCond(lastNode))
                    flowToCond = true;
                if(hasLoad && flowToCond)
                    return true;
            }
        }
    }
    return false;
}
*/
/*
CheckResult SymExec::nullifyGlobalParaReach(SEGraph * seg)
{
    push_solver();
    for(auto i = seg->begin();i!=seg->end();i++){
        SENode *node = i->second;
        Value * val = node->getVal();
        Type * ty = val->getType();
        //displayBasic(node);
        if(isa<SENodeParam>(node) || isa<SENodeGlobal>(node)){
            uint64_t typeSize = 0;
            SymExpr symexpr = getVar(node)->getSingleVal()->getExpr();
            Z3_ast ast_to_nullify = symexpr.expr;
            if(isa<SENodeGlobal>(node)) {
                if (!isa<PointerType>(ty)) {
                    llvm_unreachable(
                            "Global variables must be pointer type, as defined in SymVal SENodeGlobal::derive\n");
                }
                auto ptrty = cast<PointerType>(ty);
                assert(isPointerSort(ast_to_nullify));
                Type *ptrToTy = ptrty->getElementType();
                if (isa<IntegerType>(ptrToTy)) {
                    errs() << "Global variable points to a memory without length, initialize to 32 bytes\n";
                    typeSize = 32;
                } else {
                    //sanity check
                    if((isa<PointerType>(ptrToTy)) || (isa<FunctionType>(ptrToTy))){
                        errs() << "Double pointer or function as  global\n";
                        return CK_UNSAT;
                    }
                    typeSize = mo.getTypeSize(ptrToTy);
                }
            }
            else if(isa<SENodeParam>(node)){
                //if(symexpr.expVoidPointerTy == individual){
                    addAssert(Z3_mk_eq(ctxt, createConstMachBigIntUnsigned("165", getExprSortWidth(ast_to_nullify)), ast_to_nullify));
                    continue;
               // }
                //else if(symexpr.expVoidPointerTy == pointToMemory){
                    // then it must be a pointer, but Value itself may not be pointer type
                    if(auto ptrty = dyn_cast<PointerType>(ty)){
                        assert(isPointerSort(ast_to_nullify));
                        Type *ptrToTy = ptrty->getElementType();
                        if (isa<IntegerType>(ptrToTy)) {
                            errs() << "Para variable points to a memory without length, initialize to 32 bytes\n";
                            typeSize = 32;
                        } else {
                            //sanity check
                            if((isa<PointerType>(ptrToTy)) || (isa<FunctionType>(ptrToTy))){
                                errs() << "Double pointer or function as  global\n";
                                return CK_UNSAT;
                            }
                            typeSize = mo.getTypeSize(ptrToTy);
                        }
                    }else{
                        //well, this is for the case where the val is not a pointer but it was symbolized to a pointer
                        typeSize = 32;
                    }
                } //else if(symexpr.expVoidPointerTy == DonnotKnow){
                  //  llvm_unreachable("Type should be set for param\n");
                //}
            }
            Z3_ast poisened8bit = createConstMachBigIntUnsigned("165", mo.getBits());
            Z3_ast idx, cur, par;
            for (unsigned i = 0; i < typeSize; i++) {
                //offset
                idx = createConstPointer(i);
                //cursor
                cur = Z3_mk_bvadd(ctxt, ast_to_nullify, idx);
                //memory pointed by cursor
                par = Z3_mk_select(ctxt, memory, cur);
                addAssert(Z3_mk_eq(ctxt, poisened8bit, par));
            }
        }
    }
    Z3_lbool result = Z3_solver_check(ctxt, solver);

    pop_solver();
    switch(result){
        case Z3_L_TRUE:
            return CK_SAT;

        case Z3_L_FALSE:
            return CK_UNSAT;

        case Z3_L_UNDEF:
            return CK_UNDEF;
    }
}
 */
CheckResult SymExec::checkOverlap(
    SymVar *src1, SymVar *len1,
    SymVar *src2, SymVar *len2) {

  // make sure that their symbolic exprs exist
  if(src1 == nullptr || len1 == nullptr || src2 == nullptr || len2 == nullptr){
    return CK_SYMERR;
  }

  Z3_ast s1 = src1->getSingleVal()->getExpr().expr;
  Z3_ast l1 = len1->getSingleVal()->getExpr().expr;
  Z3_ast s2 = src2->getSingleVal()->getExpr().expr;
  Z3_ast l2 = len2->getSingleVal()->getExpr().expr;

  // NOTE: special treatment on 32bit size
  if(!isPointerSort(l1)){
    l1 = castMachIntZExt(l1, mo.getPointerWidth());
  }

  if(!isPointerSort(l2)){
    l2 = castMachIntZExt(l2, mo.getPointerWidth());
  }

  // the actual solverexits
  Z3_lbool result = checkOverlap(s1, l1, s2, l2);

  switch(result){
    case Z3_L_TRUE:
      return CK_SAT;

    case Z3_L_FALSE:
      return CK_UNSAT;

    case Z3_L_UNDEF:
      return CK_UNDEF;
  }

  llvm_unreachable("Should not reach here");
  return CK_SYMERR;
}

Z3_lbool SymExec::checkOverlap(
    Z3_ast s1, Z3_ast l1,
    Z3_ast s2, Z3_ast l2) {

  // sanity check
  assert(isPointerSort(s1) && isPointerSort(l1) &&
      isPointerSort(s2) && isPointerSort(l2));

  // save context
  push_solver();

  // condition: (s2 <= s1 < s2 + l2) || (s1 <= s2 < s1 + l1)
  Z3_ast d1 = Z3_mk_bvadd(ctxt, s1, l1);
  addAssert(Z3_mk_bvadd_no_overflow(ctxt, s1, l1, false));

  Z3_ast d2 = Z3_mk_bvadd(ctxt, s2, l2);
  addAssert(Z3_mk_bvadd_no_overflow(ctxt, s2, l2, false));

  Z3_ast cls[2];

  Z3_ast c1[2];
  c1[0] = Z3_mk_bvuge(ctxt, s1, s2);
  c1[1] = Z3_mk_bvult(ctxt, s1, d2);
  cls[0] = Z3_mk_and(ctxt, 2, c1);

  Z3_ast c2[2];
  c2[0] = Z3_mk_bvuge(ctxt, s2, s1);
  c2[1] = Z3_mk_bvult(ctxt, s2, d1);
  cls[1] = Z3_mk_and(ctxt, 2, c2);

  addAssert(Z3_mk_or(ctxt, 2, cls));

  // solve
  Z3_lbool result = Z3_solver_check(ctxt, solver);

  // restore context
    pop_solver();

  return result;
}
