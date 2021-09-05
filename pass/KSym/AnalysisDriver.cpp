#include "Project.h"
#include <chrono>
// utils
const int NUM_INTER_TRACES = 32;

static inline Value *getCallArgOrRet(CallInst *ci, int idx) {
  return idx >= 0 ? ci->getArgOperand(idx) : ci;
}




static inline Instruction *getFirstSanInst(BranchInst *bi) {
    if (starts_with(bi->getSuccessor(0)->getName(), "handler.")) {
        return bi->getSuccessor(0)->getFirstNonPHIOrDbg();
    }
    if (starts_with(bi->getSuccessor(1)->getName(), "handler.")) {
        return bi->getSuccessor(1)->getFirstNonPHIOrDbg();
    }
    //llvm_unreachable("WARN: can not find sanitizer branch");
    errs() << "WARN: can not find sanitizer branch";
    return nullptr;
}

void SAHandle::collectSeed() {
    for (auto &BB : func) {
        if (isSanitizerBlock(BB)){
            BranchInst *binst = cast<BranchInst>(BB.getTerminator());
            assert(binst->isConditional());
            if(CallInst *seed = getSanHandlerAsSeed(binst)){
                seeds.insert(seed);
            }
        }
    }
}

void SAHandle::collectFetch() {
  for(BasicBlock &bb : func){
    for(Instruction &i : bb){
      if(CallInst *ci = dyn_cast<CallInst>(&i)){
        const FetchDef *def;
        if(ci->isInlineAsm()){
          InlineAsm *target = cast<InlineAsm>(ci->getCalledValue());
          def = Fetch::findDefMatch(target->getAsmString());
        } else {
          Function *target = ci->getCalledFunction();
          if(target == nullptr){
            // TODO handle indirect call
            def = nullptr;
          } else {
            def = Fetch::findDefMatch(target->getName().str());
          }
        }

        if(def != nullptr){
          Fetch *fetch = new Fetch(ci,
              getCallArgOrRet(ci, def->src),
              getCallArgOrRet(ci, def->len),
              getCallArgOrRet(ci, def->dst));

          fts.insert(make_pair(ci, fetch));
        }
      }
    }
  }
}

Fetch *SAHandle::getFetchFromInst(Instruction *inst) {
  auto i = fts.find(inst);
  if(i == fts.end()){
    return nullptr;
  } else {
    return i->second;
  }
}

void SAHandle::remove_seeds_from_interTraces(set<CallInst*> seeds){
    for(auto eachInter = interPTraces.begin();eachInter != interPTraces.end();eachInter++){
        for(auto trace_it = eachInter->second.begin();trace_it != eachInter->second.end();){
            TraceStatus * to_remove_trace = *trace_it;
            CallInst* to_remove_seed = to_remove_trace->seed;
            if(seeds.find(to_remove_seed) != seeds.end()){
                trace_it = eachInter->second.erase(trace_it);
                delete(to_remove_trace);
                //errs()<<"deleted\n";
            }else{
                //errs()<<"not delete\n";
                trace_it++;
            }
        }
    }
};

// result collection
void SAHandle::addResult(CallInst *f, UBResult to_add) {
    auto i = result.find(f);

    // add if no result
    if(i == result.end()){
        result.insert(make_pair(f, to_add));
    }

    // if in higher rank, override
    else if(to_add > i->second){
        //result.insert(make_pair(f, res));
        result.at(f) = to_add;
    }
}

void SAHandle::mergeResult(map<CallInst*,UBResult>& ref_result) {
    for(auto eachResult : result){
        CallInst* f = eachResult.first;
        UBResult to_add = eachResult.second;

        auto i = ref_result.find(f);
        // add if no result
        if(i == ref_result.end()){
            ref_result.insert(make_pair(f, to_add));
        }

            // if in higher rank, override
        else if(to_add > i->second){
            //result.insert(make_pair(f, res));
            ref_result.at(f) = to_add;
        }
    }

}


set<CallInst*> SAHandle::aggregateResult(map<CallInst*,UBResult>& result_ref, UBResult res) {
    set<CallInst*> ret;

    for(auto &i : result_ref){
        if(i.second == res){
            ret.insert(i.first);
        }
    }

    return ret;
}


void collectDepsSeed(Value* i,set<Value*>&deps){
    if(deps.find(i) != deps.end()){
        return;
    }
    deps.insert(i);
    if(User*u = dyn_cast<User>(i)){
        for(auto opIt = u->op_begin(); opIt != u->op_end(); ++opIt){
            collectDepsSeed(*opIt, deps);
        }
    }
}
void expandDepsSeed(set<Value*>&deps){
    bool changed = true;
    while(changed) {
        changed = false;
        for(auto i : deps){
            //only care about pointers
            if(i->getType()->isPointerTy()){
                auto ui = i->use_begin(), ue = i->use_end();
                //iterate this node's user
                for(; ui != ue; ++ui){
                    if(isa<LoadInst>(*ui))
                        continue;
                    //this is a fetch function
                    if(isa<CallInst>(*ui) && Fetch::isfetch(cast<CallInst>(*ui)) != nullptr){
                        deps.insert(*ui);
                        changed = true;
                        break;
                    }
                    collectDepsSeed(*ui,deps);
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

extern cl::opt<string> FILTERF;
// analysis
Instruction* seed2root(CallInst*ci){
    BasicBlock* err_brch = cast<CallInst>(ci)->getParent();
    BasicBlock* check_brch = nullptr;
    for (auto it = pred_begin(err_brch), et = pred_end(err_brch); it != et; ++it)
    {
        BasicBlock* predecessor = *it;
        if(isSanitizerBlock(*predecessor)){
            check_brch = predecessor;
            break;
        }
    }
    if(check_brch == nullptr){
        errs()<<"err branch label:"<<err_brch->getName().str()<<'\n';
        errs()<<*(ci)<<'\n';
        llvm_unreachable("cannot get UB checking branch\n");
    }
    Instruction * term = check_brch->getTerminator();
    assert(term != nullptr);
    return term;
}

static inline void blistToIlist(blist &bl, Instruction *stop, iseq &il) {
    //errs()<<"stop:"<<*stop<<'\n';
    for(LLVMSliceBlock *bb : bl){
        //errs()<<"bb:"<<bb->getBasicBlock()->getName()<<'\n';
        LLVMSliceBlock::inst_iterator i = bb->inst_begin(), ie = bb->inst_end();
        for(; i != ie; ++i){
            //errs()<<"  inter trace"<<**i<<'\n';
            il.push_back(*i);
        }
    }

    // trim off from stop inst
    while(il.back() != stop){
        il.pop_back();
    }
}

static inline void blistToIlist_full(blist &bl, Instruction *stop, iseq &il) {
    for(LLVMSliceBlock *bb : bl){
        BasicBlock* raw_bb = bb->getBasicBlock();
        for(auto i = raw_bb->begin(); i != raw_bb->end(); ++i){
            il.push_back(&*i);
        }
    }
    if(stop != nullptr){
        while(il.back() != stop){
            il.pop_back();
        }
    }
}

void displaySlicedBlocks(iseq & trace){
    set<BasicBlock*> bbs;
    int b_c = 0;
    for(auto each_inst = trace.begin(); each_inst != trace.end(); each_inst++){
        Instruction* i = *each_inst;
        BasicBlock* b = i->getParent();
        if(bbs.find(b) == bbs.end()){
            dbgs()<<"BB("<<b_c++<<")"<<b->getName().str()<<'\n';
            bbs.insert(b);
        }
    }
}
bool SAHandle::analyze(CallInst *inst) {
    
    // collect reachable blocks
    set<BasicBlock *> reach;
    fo.getReachBlocks(inst->getParent(), reach);

    Instruction *  root = seed2root(inst);
    // create a slice
    Slice slice(reach, inst);
    //unhandled instruction encoutered, pointless to continue
    if(slice.ifAborted())
        return false;


    LLVMSlice wrap(&slice);
    SliceOracle oracle(wrap);

    // unroll
    UnrollPath *unrolled = oracle.getUnrolled(&wrap.getBasisBlock());

    // per-trace analysis
    UnrollPath::iterator it = unrolled->begin(), ie = unrolled->end();
    numTotalTrace.push_back((int)unrolled->size());

    for(unsigned ctr = 0; it != ie; ++it, ++ctr) {
        iseq trace;
        blistToIlist(*(*it), inst, trace);
        if(find(trace.begin(), trace.end(),root) == trace.end()){
            // root doesn't exist in the current trace, abort then
            continue;
        }
        TraceStatus* traceStatus = new TraceStatus(inst,root);
        if(traceStatus->bugType == ub_notype){
            //doesn't support(not interested in) this unhandler
            delete traceStatus;
            continue;
        }
        analyzeTrace(traceStatus, oracle, trace, *(*it), slice);
        /*
        if(traceStatus->satisfiable &&\
          traceStatus->depArgs.size() > 0 && \
          traceStatus->postBug && \
          !traceStatus->traceIncompleteConstraints && \
          !traceStatus->traceGlbDep){*/
        if(traceStatus->traceIncompleteConstraints) {
            // has global variable affecting this trace
            // or has unhandled instruction which makes the trace incomplete
            delete traceStatus;
            continue;
        }
        if(traceStatus->depArgs.size() > 0 \
            && !traceStatus->controledByUser)
        {
            addResult(traceStatus->seed,UB_Unsure);
            // insert the trace into interPTraces
            if(interPTraces.find(traceStatus->depArgs) == interPTraces.end()){
                interPTraces.insert(make_pair(traceStatus->depArgs,vector<TraceStatus*>()));
                interPTraces.at(traceStatus->depArgs).push_back(traceStatus);
            }else{
                interPTraces.at(traceStatus->depArgs).push_back(traceStatus);
            }
        }else{
            if(traceStatus->satisfiable &&
              traceStatus->postBug && traceStatus->controledByUser){
                addResult(traceStatus->seed,UB_True);
            }else{
                addResult(traceStatus->seed,UB_False);
            }
            delete(traceStatus);
        }
    }
#if defined(KSYM_DEBUG) && defined(KSYM_DEBUG_FUNC)
  SLOG.pop();
#endif
  return true;
}

bool SAHandle::analyzeInter(CallInst *call_site,vector<TraceStatus* >& calleeTraces) {
    // collect reachable blocks
    set<BasicBlock *> reach;
    fo.getReachBlocks(call_site->getParent(), reach);

    // create a slice
    Slice slice(reach, call_site);
    //unhandled instruction encoutered, pointless to continue
    if(slice.ifAborted())
        return false;

    // construct analysis
    LLVMSlice wrap(&slice);
    SliceOracle oracle(wrap);

    // unroll
    UnrollPath *unrolled = oracle.getUnrolled(&wrap.getBasisBlock());

    // per-trace analysis
    UnrollPath::iterator it = unrolled->begin(), ie = unrolled->end();
    fwriter->writeLine(fmt::format("Traces to callInst:{}|callee traces:{}\n",unrolled->size(),calleeTraces.size()));
    numTotalTrace.push_back((int)unrolled->size());
    numCalleeTraces.push_back((int)calleeTraces.size());

    for(unsigned ctr = 0; it != ie; ++it, ++ctr) {
        // for each trace from the start of the caller to this callsite that calls the callee
        if(time_out){
            break;
        }

        vector<TraceStatus*> traceStatuses = vector<TraceStatus*>();
        analyzeTraceInter(call_site, oracle, *(*it), slice,traceStatuses,calleeTraces);
        for(auto eachTraceIt = traceStatuses.begin(); eachTraceIt != traceStatuses.end();eachTraceIt++){
            TraceStatus* traceStatus = *eachTraceIt;
            if(traceStatus->traceIncompleteConstraints){
                // affected by glb or has unhandled instruction
                delete(traceStatus);
                continue;
            }
            assert(traceStatus->seed != nullptr);
            if(traceStatus->depArgs.size() > 0 && !traceStatus->controledByUser){
                addResult(traceStatus->seed,UB_Unsure);
                if(interPTraces.find(traceStatus->depArgs) == interPTraces.end()){
                    interPTraces.insert(make_pair(traceStatus->depArgs,vector<TraceStatus*>()));
                    interPTraces.at(traceStatus->depArgs).push_back(traceStatus);
              }else{
                  if(interPTraces.at(traceStatus->depArgs).size() > NUM_INTER_TRACES)
                      delete(traceStatus);
                  else
                      interPTraces.at(traceStatus->depArgs).push_back(traceStatus);
              }
            }else{
                // no need to check post bug, because it was checked
                if(traceStatus->satisfiable && traceStatus->controledByUser && traceStatus->postBug){
                    addResult(traceStatus->seed,UB_True);
                }else{
                    addResult(traceStatus->seed,UB_False);
                }
                delete(traceStatus);
            }
        }
    }
    return true;
}

#if defined(KSYM_DEBUG) && defined(KSYM_DEBUG_SVAR)
static inline void dumpValueSymbol(Value *val, SEGraph &seg, SymExec &sym) {
  SENode *node = seg.getNodeProbe(val);
  assert(node != nullptr);

  SymVar *svar = sym.getVar(node);
  if(svar == nullptr){
    errs() << "<unknown>\n";
  } else {
    svar->dump(sym.getContext());
  }
}
#endif

void printControFlow(SEGraph *seg){
    vector<BasicBlock*> cache;
    for(map<pair<int, Value *>, SENode *>::iterator segIt = seg->begin() ; segIt != seg->end() ; segIt++){
        if(Instruction * i = dyn_cast<Instruction>(segIt->first.second)){
            BasicBlock *b = i->getParent();
            if(find(cache.begin(),cache.end(),b) == cache.end()){
                Instruction * term = b->getTerminator();

                BranchInst *branch = dyn_cast<BranchInst>(term);
                assert(branch != nullptr);
                if(!branch->isConditional()){
                    errs()<<"BB:"<<b->getName()<<" unconditional \n";
                }
                else{
                    SENode* brNode = seg->getNodeProbe(branch);
                    assert(brNode != nullptr);
                    int whichBranch = seg->getCond(brNode);
                    assert(whichBranch != -1);
                    if(whichBranch == 0)
                        errs()<<"BB:"<<b->getName()<<" true \n";
                    else if(whichBranch == 1)
                        errs()<<"BB:"<<b->getName()<<" false \n";
                    else
                        assert(false);
                }

                cache.push_back(b);
            }
        }
    }
}

void printDbgInfo(vector<pair<Fetch *,int>> fetches,Instruction *inst){
    int i = 0;
    for(auto eachFetch = fetches.begin(); eachFetch != fetches.end(); eachFetch++,i++){
        Instruction * fetchIns = eachFetch->first->inst;
        DebugLoc fetchDbg = fetchIns->getDebugLoc();
        errs()<<"fetch src:"<<i<<":";
        fetchDbg.print(errs());
        errs()<<'\n';
    }
    errs()<<"seed src:";
    Instruction * term = inst->getParent()->getTerminator();
    DebugLoc termDbg = term->getDebugLoc();
    termDbg.print(errs());
    errs()<<'\n';
}

bool SAHandle::formalizeUB(SENodeSpecialCall* seedNode,int layer){
    
}
Instruction* SAHandle::seed2sanitizedVar(CallInst *seed,enum UB_types UB_type) {
    BasicBlock* errBranch = seed->getParent();
    CallInst* ub_handler = nullptr;

    BasicBlock* ubCheckBlock = errBranch->getSinglePredecessor();
    if(ubCheckBlock == nullptr)
    {
        assert(errBranch->hasNPredecessors(2));
        pred_iterator p_it = pred_begin(errBranch);
        pred_iterator p_end = pred_end(errBranch);
        for(;p_it != p_end;p_it++)
        {
            BasicBlock* bb = *p_it;
            if(cast<BranchInst>(bb->getTerminator())->isConditional()){
                ubCheckBlock = bb;
                break;
            }
        }
    }
    assert(ubCheckBlock != nullptr);
    Instruction * ub_value = nullptr;
    vector<enum UB_types> unimmediateUBTypes {uadd,usub,umul,sadd,ssub,smul,negate_overflow,shift_out_of_bound};
    if(find(unimmediateUBTypes.begin(),unimmediateUBTypes.end(),UB_type) != unimmediateUBTypes.end())
    {
        Instruction* term = ubCheckBlock->getTerminator();
        ExtractValueInst * conVar = dyn_cast<ExtractValueInst>(term->getOperand(0));
        if(conVar == nullptr){
            errs()<<"Err: UBSAN place is wrong\n";
            return nullptr;
        }
        InsertValueInst* sanitized_stuct = dyn_cast<InsertValueInst>(conVar->getOperand(0));
        if(sanitized_stuct == nullptr){
            //errs()<<*conVar<<*conVar->getFunction();
            //assert(false);
            errs()<<"Err:UB not clear\n";
            return nullptr;
        }
        if(sanitized_stuct->getNumUses() == 1){
            // this only one use can only be the "extractInst" inst which is pointless
            errs()<<"Err: sanitized value not used\n";
            return nullptr;
        }
        if(sanitized_stuct->getNumUses() >= 2){
            auto u_it = sanitized_stuct->user_begin();
            for(; u_it != sanitized_stuct->user_end(); u_it++){
                Value * use_of_struct = *u_it;
                //errs()<<*use_of_struct<<'\n';
                ExtractValueInst * extract_inst = dyn_cast<ExtractValueInst>(use_of_struct);
                if(extract_inst != nullptr && use_of_struct != conVar){
                    ub_value = extract_inst;
                    break;
                }
            }
        }
        if(ub_value == nullptr){
            // the sanitized value is never extracted from the struct
            return nullptr;
        }
        return ub_value;
    }
    else{
       return nullptr;
    }
}

bool SAHandle::stillWrong(Instruction * sanitized_var,SEGraph*seg){
    // check if it's checked
    SENode* sanitized_node = seg->getNodeProbe(sanitized_var);
    if(!sanitized_node){
        throw("[Err]sanitized value missing from the seg.");
    }
    vector<SENode*> useChain;
    vector<vector<SENode*> > useChains;
    collectUseChains(sanitized_node,useChain,&useChains);
    for(auto each_use_chain = useChains.begin(); each_use_chain != useChains.end();each_use_chain++){
        bool met_check = false;
        for(auto each_use_node = each_use_chain->begin(); each_use_node != each_use_chain->end(); each_use_node++){
            if(isa<SENodeBranch>(*each_use_node)){
                met_check = true;
            }
        }
        if(met_check == false){
            return true;
        }
    }
    return false;
}


int SAHandle::postBugAnalysis(TraceStatus* traceStatus,blist & old_blks ,Slice& old_slice){
    enum UB_types ub_type = seed2UBType(traceStatus->seed);
    Instruction * sanitized_value = seed2sanitizedVar(traceStatus->seed,ub_type);

    vector<enum UB_types> unimmediateUBTypes {uadd, usub, umul, sadd, ssub, smul, negate_overflow, shift_out_of_bound};
    if(find(unimmediateUBTypes.begin(),unimmediateUBTypes.end(),ub_type) != unimmediateUBTypes.end()) {
        // this is a UB that needs to be post-bug analyzed
        if(sanitized_value == nullptr){
            // this UB needs to be post-bug analyzed, but its sanitized value cannot be found
            // or its used cannot be found, so early abort
            return 0;
        }
        // find a meaningful use
        errs()<<"[info]post-bug analyzing sanitized value:"<<*sanitized_value<<'\n';
        BasicBlock* startBB = traceStatus->seed->getParent();
        vector<Instruction*> meaningful_uses;
        queue<User*> use_queue;
        for(auto user_it = sanitized_value->user_begin(); user_it != sanitized_value->user_end();user_it++)
        {
            use_queue.push(*user_it);
        }
        while(use_queue.size() > 0){
            User * cur_use = use_queue.front();
            use_queue.pop();
            Instruction* used = cast<Instruction>(cur_use);
            if(isa<CmpInst>(used) or isa<SelectInst>(used)){
                continue;
            }
            else if(CallInst* call_inst = dyn_cast<CallInst>(used)){
                Function* called_func = call_inst->getCalledFunction();
                if(called_func != nullptr){
                    string func_name = called_func->getName().str();
                    // make sure the function is not sth meaningless
                    if(func_name.find("__ubsan") != string::npos || func_name.find("llvm.") != string::npos){
                        continue;
                    }
                }
            }
            else if(isa<StoreInst>(used) || isa<CastInst>(used) || isa<BinaryOperator>(used)){
                // we need to further track these operations down
                for(auto user_it = used->user_begin(); user_it != used->user_end();user_it++)
                    use_queue.push(*user_it);
            }else{
                meaningful_uses.push_back(used);
            }
        }
        //if(meaningful_uses.size() == 0){
        //    errs()<<"[err] this UB has no meaningful use\n";
        //}
        for(auto meaningful_use = meaningful_uses.begin();meaningful_use != meaningful_uses.end();meaningful_use++){
            Instruction* use = (*meaningful_use);
            errs()<<"[info]use of sanitized value:"<<*use<<'\n';
            BasicBlock * used_bb = use->getParent();
            //UnrollPath * additional_path = collect_additional_traces(use,ub_def_bb,used_bb);
    
            set<BasicBlock*> reach;
            queue<BasicBlock* > work_queue;
            work_queue.push(startBB);
            reach.insert(startBB);
            bool found_used = false;
            while (!work_queue.empty()) {
                BasicBlock * cur_b = work_queue.front();
                work_queue.pop();

                if (cur_b == used_bb) {
                    found_used = true;
                    break;
                }else {
                    int num_succ = cur_b->getTerminator()->getNumSuccessors();
                    if (num_succ == 0) { continue; }
                    else if (num_succ == 1) {
                        BasicBlock *to_add = cur_b->getSingleSuccessor();
                        //errs()<<(*to_add).getName().str()<<'\n';
                        if (reach.find(to_add) == reach.end()\
                            && isPotentiallyReachable(to_add,used_bb)){
                            reach.insert(to_add);
                            work_queue.push(to_add);
                        }
                    } else if (num_succ == 2) {
                        BasicBlock *to_add1 = cur_b->getTerminator()->getSuccessor(0);
                        BasicBlock *to_add2 = cur_b->getTerminator()->getSuccessor(1);

                        if (reach.find(to_add1) == reach.end() \
                         && isPotentiallyReachable(to_add1,used_bb)) {
                            reach.insert(to_add1);
                            work_queue.push(to_add1);
                        }
                        if (reach.find(to_add2) == reach.end() \
                         && isPotentiallyReachable(to_add2,used_bb)) {
                            reach.insert(to_add2);
                            work_queue.push(to_add2);
                        }
                    }
                }
            }
            if(found_used == false){
                continue;
            }
            Slice new_slice(use,startBB, used_bb,reach);
            if(new_slice.ifAborted()){
                continue;
            }
            LLVMSlice new_wrap(&new_slice);
            SliceOracle new_oracle(new_wrap);
            UnrollPath *unrolled = new_oracle.getUnrolled(&new_wrap.getBasisBlock());
            UnrollPath::iterator it = unrolled->begin(), ie = unrolled->end();
            for(unsigned ctr = 0; it != ie; ++it, ++ctr) {
                
                iseq new_trace;
                blistToIlist_full(old_blks, nullptr, new_trace);

                blistToIlist_full(**it, use, new_trace);

                try{
                    SEGraph postSEG(&new_oracle,new_trace,traceStatus->seg->getConds(),func);
                    __asm__("nop");
                    if(postSEG.abort){
                        traceStatus->postBug = false;
                        break;
                    }
                    if(stillWrong(sanitized_value,&postSEG)){
                        traceStatus->postBug = true;
                        break;
                    }

                }catch(...){
                    traceStatus->postBug = false;
                    break;
                }
            }
            if(traceStatus->postBug){
                break;
            }
        }
    }else{
        // these are the UB that do need to be post-bug analyzed
        errs()<<"[info]"<<*(traceStatus->seed)<<"\ndoesn't need to be post-bug analyzed\n";
        traceStatus->postBug = true;
    }
    return 0;
}

void display_trace(iseq& trace){
    int c = 0;
    int b_c = 0;
    set<BasicBlock*> bbs;
    for(auto each_inst = trace.begin(); each_inst != trace.end(); each_inst++,c++){
        Instruction* i = *each_inst;
        BasicBlock* b = i->getParent();
        if(bbs.find(b) == bbs.end()){
            dbgs()<<"BB("<<b_c++<<")"<<b->getName().str()<<'\n';
            bbs.insert(b);
        }
        dbgs()<<"    trace("<<c<<")"<<*i<<'\n';
    }
}
void SAHandle::analyzeTrace(TraceStatus* traceStatus, SliceOracle &so, iseq & trace ,blist & blks, Slice& old_slice) {

    // convert to inst list

    //display_trace(trace);
    traceStatus->seg = new SEGraph(&so, trace, func);

    SENode* root_node = traceStatus->seg->getNodeProbe(traceStatus->seed);
    assert(root_node != nullptr);


    UB_root* ub_root = new UB_root(root_node,userInput,mo);

    bool normal_finish = ub_root->expandAndFormalize();

    if(!normal_finish){
        traceStatus->traceIncompleteConstraints = true;
        delete ub_root;
        return;
    }

    traceStatus->controledByUser = ub_root->root_controlled_by_user();

    for(auto eachDepArg:ub_root->non_user_args){
        traceStatus->depArgs.insert(eachDepArg->getArgNo());
    }
    //if(traceStatus->controledByUser)
    //    ub_root->displayCache();
    delete ub_root;
    if(traceStatus->controledByUser){

        SymExec sym(mo);
        bool incompleteConstraint = traceStatus->seg->symbolize(sym);
        if(incompleteConstraint){
            traceStatus->traceIncompleteConstraints = true;
            //abort
            return;
        }
        CheckResult initialReach;
        initialReach = sym.checkPathValid();

        if(initialReach == CK_SAT){
            traceStatus->satisfiable = true;

            // locate the SENode:
            /* //for debug purpose(examing the assigned value to an interesting memory makes sense or not)
            SENode* node_to_eval = nullptr;
            for(auto node_it = traceStatus->seg->begin(); node_it != traceStatus->seg->end(); node_it++){
                if(SENodeLoad* load_node = dyn_cast<SENodeLoad>(node_it->second)){
                    if(isa<SENodeGlobal>(*(load_node->depBegin()))){
                        errs()<<*(load_node->getCastedVal());
                        node_to_eval = load_node;
                        break;
                    }
                }
            }
            if(node_to_eval){
                sym.getSatVal(traceStatus->seg,node_to_eval);
            }
            */
        }
        if(traceStatus->satisfiable){
            //then we do a post bug check
            postBugAnalysis(traceStatus,blks,old_slice);
        }
    }
}

void SAHandle::analyzeTraceInter(CallInst *call_site, SliceOracle &so, blist &blks,Slice& old_slice,\
vector<TraceStatus*>& traceStatuses,vector<TraceStatus*>& calleeTraces){
// convert to inst list
    iseq trace;
    blistToIlist(blks, call_site, trace);

    // create SEG
    for(auto calleeTrace = calleeTraces.begin(); calleeTrace != calleeTraces.end(); calleeTrace++){
        // already time-out
        if(time_out){
            break;
        }
        CallInst* seed = (*calleeTrace)->seed;
        if(interPTraces_by_seed.find(seed) != interPTraces_by_seed.end()){
            if(interPTraces_by_seed.at(seed) > NUM_INTER_TRACES){
                // this seeds has saturated
                continue;
            }else{
                interPTraces_by_seed.at(seed) ++;
            }
        }else{
            interPTraces_by_seed.insert(make_pair(seed,1));
        }
        TraceStatus* traceStatus = new TraceStatus(*calleeTrace);
        //traceStatus->seed = (*calleeTrace)->seed;
        traceStatus->seg = new SEGraph(&so, trace, func);
        iseq filt;
        //traceStatus->seg->filterTrace(filt);
        //merge the caller SEG with the callee
        int mergeRet = traceStatus->seg->mergeSEG((*calleeTrace)->seg,call_site,(*calleeTrace)->depArgs);
        if(mergeRet == -1){
            traceStatus->traceIncompleteConstraints = true;
            traceStatuses.push_back(traceStatus);
            continue;
        }

        SENode* seed_node = traceStatus->seg->getNodeProbe(seed);
        if(seed_node == nullptr){
            traceStatus->seg->displayForDeps();
            errs()<<"the seed:"<<*seed<<'\n';
            llvm_unreachable("\"the seed is missing from the merged SEG\"");
        }
        UB_root* ub_root = new UB_root(seed_node,userInput,mo);
        bool normal_finish = ub_root->expandAndFormalize();

        if(! normal_finish){
            traceStatus->traceIncompleteConstraints = true;
            delete ub_root;
            traceStatuses.push_back(traceStatus);
            continue;
        }
        for(auto eachDepArg:ub_root->non_user_args){
            traceStatus->depArgs.insert(eachDepArg->getArgNo());
        }
        traceStatus->controledByUser = ub_root->root_controlled_by_user();

        delete ub_root;
        if(traceStatus->controledByUser){
            CheckResult initialReach = CK_UNDEF;

            //bool incompleteConstraint = traceStatus->seg->unhandledNode();
            SymExec sym(mo);
            bool incompleteConstraint = traceStatus->seg->symbolize(sym);
            if(incompleteConstraint){
                traceStatus->traceIncompleteConstraints = true;
                traceStatuses.push_back(traceStatus);
                continue;
            }
            initialReach = sym.checkPathValid();
            if(initialReach == CK_SAT){
                traceStatus->satisfiable = true;
            }
            if(traceStatus->satisfiable){
                //then we do a post bug check
                postBugAnalysis(traceStatus,blks,old_slice);
            }
        }
        //add to the set
        traceStatuses.push_back(traceStatus);
    }
}
extern const int MAX_NUM_SEEDS_PER_FUNC;
// slice analysis entry point
void SAHandle::run() {
#if defined(KSYM_DEBUG) && defined(KSYM_DEBUG_DRAW)
  SLOG.map("cfg");
  Record::CFG(func, SLOG);
  SLOG.pop();
#endif
    // check if this is a syscall function
    //resolveSyscall();
    // collect seed instruction first
    // also collect fetches, for later reporting purpose.
    collectSeed();
    collectFetch();
    //errs()<<func<<'\n';
    while(seeds.size() > MAX_NUM_SEEDS_PER_FUNC){
        seeds.erase(seeds.begin());
    }
    int numAbort = 0;
    //int count = 0;
    for(auto &i : seeds){
        dbgs()<<"analyzing seeds:"<<*i<<'\n';
        if(!analyze(i))
            numAbort += 1;
    }

    //the bugs that determined to be true or false, no need to go further
    set<CallInst*> trueSeeds = aggregateResult(result,UB_True);
    set<CallInst*> falseSeeds = aggregateResult(result,UB_False);
    remove_seeds_from_interTraces(trueSeeds);
    remove_seeds_from_interTraces(falseSeeds);



    fwriter->writeLine(fmt::format("{}:{} out of {} aborted\n",func.getName().str(), numAbort,seeds.size()));

#ifdef KSYM_DEBUG
  SLOG.pop();
#endif

#if defined(KSYM_DEBUG) && defined(KSYM_DEBUG_FUNC)
  // log results
  SLOG.map("result");

  SLOG.log("total", result.size());
  SLOG.log("error", failed.size());
  SLOG.log("sat", countResult(CK_SAT));
  SLOG.log("uns", countResult(CK_UNSAT));
  SLOG.log("udf", countResult(CK_UNDEF));

  SLOG.pop();
#endif
}

void SAHandle::write_num_interTrace_to_file(){
    errs()<<fmt::format("for current function:{}:\n",func.getName().str());
    errs()<<"inter trace order by dep idx:\n";
    map<CallInst*,int> order_by_call;
    for(auto each_inter_dep_it = interPTraces.begin(); each_inter_dep_it != interPTraces.end();each_inter_dep_it++){
        errs()<<"    idx:    ";
        for(auto each_dep_idx_it = each_inter_dep_it->first.begin(); each_dep_idx_it != each_inter_dep_it->first.end(); each_dep_idx_it++){
            errs()<<*each_dep_idx_it<<',';
        }
        errs()<<"\n";
        errs()<<fmt::format("    size of traces:{}\n",each_inter_dep_it->second.size());
        for(auto each_trace_it = each_inter_dep_it->second.begin();each_trace_it != each_inter_dep_it->second.end();each_trace_it++){
            CallInst* seed = (*each_trace_it)->seed;
            if(order_by_call.find(seed) == order_by_call.end()){
                order_by_call.insert(make_pair(seed,1));
            }else{
                order_by_call.at(seed)++;
            }
        }
    }
    errs()<<"inter trace order by seed :\n";
    for(auto each_seed_it = order_by_call.begin();each_seed_it != order_by_call.end();each_seed_it++){
        errs()<<"seed:"<<*((*each_seed_it).first)<<'\n';
        errs()<<"size:"<<(*each_seed_it).second<<'\n';
    }
}

void SAHandle::write_result_to_file(ModuleOracle& mo, FileWriter*fw,map<CallInst*,UBResult>& summarized){
    string to_write = fmt::format("total seed num:{}\n true seed num:{}\n unsure seed num:{}\n false seed num:{}\n",\
        summarized.size(),SAHandle::aggregateResult(summarized,UB_True).size(),\
        SAHandle::aggregateResult(summarized,UB_Unsure).size(),SAHandle::aggregateResult(summarized,UB_False).size() );
    errs() << to_write;
    fw->writeLine(to_write);

    for(auto eachResult: summarized){
        CallInst* seed = eachResult.first;
        if(eachResult.second == UB_True){
            tuple<int,int,int> indexes = mo.getSeedIndex(seed);
            string confirmed_ub = fmt::format("confirmed UB:{},{},{}\n{}\n",get<0>(indexes),get<1>(indexes),get<2>(indexes),Helper::getValueRepr(seed));
            errs()<<confirmed_ub<<'\n';
            fw->writeLine(confirmed_ub);
        }
        else if(eachResult.second == UB_False){
            fw->writeLine("false UB:"+Helper::getValueRepr(seed)+'\n');
        }
        else if(eachResult.second == UB_Unsure){
            fw->writeLine("unsure UB:"+Helper::getValueRepr(seed)+'\n');
        }
    }
}

// slice analysis entry point
void SAHandle::runInter() {

    // check if this is a syscall function
    //resolveSyscall();
    // collect seed instruction first
    // also collect fetches, for later reporting purpose.

    collectFetch();
    int numAbort = 0;
    //int count = 0;
    for(auto each_callsite : callSites){
        // for each callinst inside the current function
        // that calls the calleeF
        if(time_out){
            break;
        }
        vector<TraceStatus*> possible_callee_traces;
        for(auto trace_it = calleesTraces->begin(); trace_it != calleesTraces->end() ; trace_it++){
            set<int> deps = trace_it->first;
            if(taintSummary->taint_could_satisfied(&deps,each_callsite)){
                possible_callee_traces.insert(possible_callee_traces.end(),trace_it->second.begin(),trace_it->second.end());
            }
        }
        assert(calleeFunc != nullptr);
        if(each_callsite->arg_size() != calleeFunc->arg_size()){
            errs()<<"Err:caller arg size not equal to callee argument size, inaccurate call graph\n";
            fwriter->writeLine("Err:caller arg size not equal to callee argument size, inaccurate call graph\n");
        }else{
            if(possible_callee_traces.size()>0){
                errs()<<"taint satisfied by current callInst\n";
                fwriter->writeLine("taint satisfied by current callInst\n");
                if(!analyzeInter(each_callsite,possible_callee_traces))
                    numAbort += 1;
            }else{
                errs()<<"taint cannot be satisfied by current callInst\n";
                fwriter->writeLine("taint cannot be satisfied by current callInst\n");
            }
        }
    }

    stringstream s;
    fwriter->writeLine("number of traces for each callInst * callee traces:\n");
    assert(numTotalTrace.size() == numCalleeTraces.size());
    for(int i = 0 ; i < numTotalTrace.size();i++)
        s<<numTotalTrace[i]<<"|"<< numCalleeTraces[i]<<' ';
    s<<"\n";
    fwriter->writeLine(s.str());
}

