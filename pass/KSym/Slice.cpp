#include <llvm/IR/IntrinsicInst.h>
#include "Project.h"

// the strategy to collect conditions 
#define KSYM_CONFIG_SLICE_COMPLETE
raw_ostream& operator<<(raw_ostream& os,const LLVMSliceBlock & obj)
{
    obj.printAsOperand(os, true);
    return os;
}
// Pre-procesing
static void collectInsts(set<BasicBlock *> &reach, set<Instruction *> &insts) {
  for(BasicBlock *bb : reach){
    for(Instruction &i : *bb){
      insts.insert(&i);
    }
  }
}
bool Slice::btraceSpecial(CallInst *callinst) {
    bool res = false;
    backs.insert(callinst);
    for(auto arg_it = callinst->arg_begin(); arg_it != callinst->arg_end(); arg_it++){
        if(btrace(*arg_it))
            res = true;
    }
    return res;
}

#define ALL_OTHER_CALLS_BACKTRACE                      \
    int ArgNum = i_call->getNumArgOperands();          \
    if(ArgNum <= 4){                                   \
        for(int arg_c = 0 ; arg_c < ArgNum;arg_c++ ){  \
            if(btrace(i_call->getArgOperand(arg_c)))   \
                res = true;                            \
        }                                              \
    }                                                  \

// Backtracing
bool Slice::btrace(Value *v) {
  if (abortCurSeed)
    return false;
  // test if we have backtraced this value
  if(backs.find(v) != backs.end()){
    return false;
  }
  // check if v is a constant
  if(isa<ConstantData>(v) || isa<ConstantAggregate>(v)){
    return false;
  }

  // mark that we have backtraced this value
  backs.insert(v);

  // check if v is a root
  if(isa<Argument>(v) || isa<GlobalObject>(v) || isa<AllocaInst>(v)){
    return roots.insert(v).second;
  }

  for(Value::user_iterator i = v->user_begin(); i != v->user_end(); ++i){
      if(isa<StoreInst>(*i))
          btrace(*i);
  }

  // further decompose v
  bool res = false;

  if(ConstantExpr *i_cexp = dyn_cast<ConstantExpr>(v)){
    // treat constant expr as regular instructions
    User::op_iterator oi = i_cexp->op_begin(), oe = i_cexp->op_end();
    for(; oi != oe; ++oi){
      if(btrace(*oi)){
        res = true;
      }
    }
  }

  else if(BranchInst *i_brch = dyn_cast<BranchInst>(v)){
    // only trace the condition value
    if(i_brch->isConditional()){
      if(btrace(i_brch->getCondition())){
        res = true;
      }
    }
  }

  else if(CallInst *i_call = dyn_cast<CallInst>(v)){
    // NOTE: CallInst is a black hole which might suck up everything,
    // so take caution not to trace unwanted conditions
    if(i_call->isInlineAsm()){
      InlineAsm *bin = dyn_cast<InlineAsm>(i_call->getCalledValue());
      assert(bin != nullptr);
      Type *reTy = i_call->getFunctionType()->getReturnType();
      std::string fn = bin->getAsmString();
      if(fn.empty()){
          errs() <<"Err:asm call string is empty, abort\n";
          abortCurSeed = true;
          return false;
        //llvm_unreachable("Asm string cannot be empty");
      }

#define ASMCALL_BACK
#include "Asmcall.def"
#undef ASMCALL_BACK

      else {
        // default to ignore the rest
        /*
        errs() 
          << i_call->getParent()->getParent()->getName() << "::" 
          << fn << "\n";
        */
        // TODO handle the rest of the calls
        ALL_OTHER_CALLS_BACKTRACE
      }
    }

    else {

      Function *tar = i_call->getCalledFunction();
      if(tar == nullptr){
        // TODO handle indirect call
      }

      else {
        // direct call, selectively trace the arguments
        std::string fn = tar->getName().str();
        if(tar->isIntrinsic()){
          Helper::convertDotInName(fn);
        }

        if(fn.empty()){
          llvm_unreachable("Function name cannot be empty");
        }

#define LIBCALL_BACK
#include "Libcall.def"
#undef LIBCALL_BACK

        else {
          // default to ignore the rest
          /*
          errs() 
            << i_call->getParent()->getParent()->getName() << "::" 
            << fn << "\n";
          */
          // TODO handle the rest of the calls
          ALL_OTHER_CALLS_BACKTRACE
        }
      }
    }
  }

  else if(isa<ExtractElementInst>(v) || isa<InsertElementInst>(v)){
    // handle these rare cases in a generic way
    Instruction *inst = cast<Instruction>(v);

    User::op_iterator oi = inst->op_begin(), oe = inst->op_end();
    for(; oi != oe; ++oi){
      if(btrace(*oi)){
        res = true;
      }
    }
  }

  else if(BinaryOperator *i_bin = dyn_cast<BinaryOperator>(v)) {
    if(btrace(i_bin->getOperand(0))){
      res = true;
    }

    if(btrace(i_bin->getOperand(1))){
      res = true;
    }
  }

  else if(CastInst *i_cast = dyn_cast<CastInst>(v)){
    if(btrace(i_cast->getOperand(0))){
      res = true;
    }
  }

  else if(CmpInst *i_cmp = dyn_cast<CmpInst>(v)){
    if(btrace(i_cmp->getOperand(0))){
      res = true;
    }

    if(btrace(i_cmp->getOperand(1))){
      res = true;
    }
  }

  else if(ExtractValueInst *i_ext = dyn_cast<ExtractValueInst>(v)){
    if(btrace(i_ext->getAggregateOperand())){
      res = true;
    }
  }

  else if(GetElementPtrInst *i_gep = dyn_cast<GetElementPtrInst>(v)){
    if(btrace(i_gep->getPointerOperand())){
      res = true;
    }

    User::op_iterator oi = i_gep->idx_begin(), oe = i_gep->idx_end();
    for(; oi != oe; ++oi){
      if(btrace(*oi)){
        res = true;
      }
    }
  }

  else if(LoadInst *i_load = dyn_cast<LoadInst>(v)){
    if(btrace(i_load->getPointerOperand())){
      res = true;
    }
  }

  else if(PHINode *i_phi = dyn_cast<PHINode>(v)){
    unsigned num = i_phi->getNumIncomingValues();
    for(int i = 0; i < num; i++){
      if(btrace(i_phi->getIncomingValue(i))){
        res = true;
      }

      Instruction *pt = i_phi->getIncomingBlock(i)->getTerminator();
      if(isa<CallBrInst>(pt)){
          errs()<<"Err: terminater is a callbr instruction,"<<*pt<<'\n';
          abortCurSeed = true;
          return false;
      }
      if(!isa<BranchInst>(pt)){
          errs()<<"Err: phi incoming block terminator is not a brach but:"<<*pt<<'\n';
          abortCurSeed = true;
          return false;
      }

      if(btrace(pt)){
        res = true;
      }
    }
  }

  else if(SelectInst *i_sel = dyn_cast<SelectInst>(v)){
    if(btrace(i_sel->getTrueValue())){
      res = true;
    }

    if(btrace(i_sel->getFalseValue())){
      res = true;
    }

    if(btrace(i_sel->getCondition())){
      res = true;
    }
  }

  else if(StoreInst *i_store = dyn_cast<StoreInst>(v)){
    if(btrace(i_store->getValueOperand())){
      res = true;
    }

    if(btrace(i_store->getPointerOperand())){
      res = true;
    }
  }

  else if(InsertValueInst *i_insert = dyn_cast<InsertValueInst>(v)){
      if(btrace(i_insert->getAggregateOperand())){
          res = true;
      }

      if(btrace(i_insert->getInsertedValueOperand())){
          res = true;
      }
  }
  else {
    // should have enumerated all
    //DUMP.typedValue(v);
    //errs()<<" not handled when slicing\n";
    abortCurSeed = true;
    return false;
    llvm_unreachable("Unknown value type to btrace");
  }

  return res;
}


void Slice::follow(Value *v, set<Value *> &taints){
  if (abortCurSeed)
      return;
  //errs()<<"follow:"<<*v<<'\n';
  Value::user_iterator i = v->user_begin(), ie = v->user_end();
  for(; i != ie; ++i){
    User *u = *i;
    //errs()<<"user:"<<*u<<'\n';
    // skip btraced values
    if(backs.find(u) != backs.end()){
      continue;
    }

    // do not follow cyclic dependency (e.g., phi)
    if(taints.find(u) != taints.end()){
        continue;
    }

    if(ConstantExpr *cexp = dyn_cast<ConstantExpr>(u)){
      taints.insert(cexp);
      follow(cexp, taints);
    }

    else if(isa<ConstantAggregate>(u) || isa<GlobalObject>(u) || isa<BlockAddress>(u) || isa<GlobalValue>(u)){
      // simply ignore them
      continue;
    }

    else if(Instruction *inst = dyn_cast<Instruction>(u)){
      if(scope.find(inst) == scope.end()){
        continue;
      }
      taints.insert(inst);
      follow(inst, taints);
    }
    else {

      DUMP.typedValue(u);
      errs() << "Err:" << *v << " not handled when following\n";
      llvm_unreachable("Unknown value type to follow");
    }
  }
}

SliceBlock *Slice::apply(BasicBlock *bb) {
  // check if block is in the reachable set
  if(reach.find(bb) == reach.end()){
    return nullptr;
  }

  // construct block
  SliceBlock *slice = new SliceBlock(bb);

  for(Instruction &inst : *bb){
    if(backs.find(&inst) != backs.end()){
      slice->addInst(&inst);
    }
  }

  return slice;
}

SliceBlock *Slice::taintCondVar(BasicBlock *bb, list<BasicBlock *> &hists,
                                map<BasicBlock *, SliceBlock *> &dedup, set<Value *> &taints) {

  // if block is not in the reachable set, return null
  // `cache` contains all predecessor bb of the one contains seed inst
  auto i = cache.find(bb);
  if(i == cache.end()){
    // errs()<<"1----------------------------------\n";
    return nullptr;
  }

  // if the block has been visited
  auto j = dedup.find(bb);
  if(j != dedup.end()){
    // errs()<<"2----------------------------------\n return:"<<*j->second->getBlock();
    return j->second;
  }

  // if the visit history forms a loop, follow trace and return the nearest one
  auto x = std::find(hists.begin(), hists.end(), bb);
  if(x != hists.end()){
    for(; x != hists.end(); x++){
      auto y = dedup.find(*x);
      if(y != dedup.end()){
        // errs()<<"3----------------------------------\n return:"<<*y->second->getBlock();
        return y->second;
      }
    }
    // errs()<<"3----------------------------------return null \n";
    return nullptr;
  }

  // errs()<<"Encounter new BB:\n"<<*bb;

  // this is a new block
  hists.push_back(bb);
  SliceBlock *slice = i->second;

  // collect and link succs
  Instruction *term = bb->getTerminator();
  unsigned num = term->getNumSuccessors();

  if(num == 0){
    // reached a return block
    assert(slice->numInsts() != 0);

    // mark it as visited
    dedup.insert(make_pair(bb, slice));
    if(std::find(suits.begin(),suits.end(),slice) == suits.end())
      suits.push_back(slice);

    // return
    hists.pop_back();
    // errs()<<"4----------------------------------\n";
    return slice;
  }

  if(num == 1){
    // unconditional branch
    BasicBlock *succ = term->getSuccessor(0);
    SliceBlock *next;

    if(slice->numInsts() != 0){
      // mark it as visited
      dedup.insert(make_pair(bb, slice));
      if(std::find(suits.begin(),suits.end(),slice) == suits.end())
        suits.push_back(slice);

      // setup links
      taintCondVar(succ, hists, dedup, taints);

      // return
      hists.pop_back();
      // errs()<<"5----------------------------------\n";
      return slice;
    }

    else {
      // follow links
      next = taintCondVar(succ, hists, dedup, taints);
      dedup.insert(make_pair(bb, next));

      // return
      hists.pop_back();
      // errs()<<"6----------------------------------\n";
      return next;
    }
  }

  if(num == 2){
    // conditional branch
    BasicBlock *tval = term->getSuccessor(0), *fval = term->getSuccessor(1);
    SliceBlock *tnxt, *fnxt;

    if(slice->numInsts() != 0){
      // mark it as visited
      dedup.insert(make_pair(bb, slice));
      if(std::find(suits.begin(),suits.end(),slice) == suits.end())
        suits.push_back(slice);

      // setup links
      tnxt = taintCondVar(tval, hists, dedup, taints);
      fnxt = taintCondVar(fval, hists, dedup, taints);

      if(tnxt == fnxt){
        // this condition is irrelevant, so do nothing
        // errs()<<"numinst!=0: tnxt==fnxt\n";
      }

      else {
#ifdef KSYM_CONFIG_SLICE_COMPLETE
        // this condition is relevant
        // (make it a condition as long as tnxt != fnxt)
        if(backs.find(term) == backs.end()){
          // errs()<<"numinst!=0: tnxt!=fnxt, term not in backs:\n";
          // errs()<<*term<<"\n";
          taints.insert(term);
        }else{
          // errs() <<"term in backs already:"<<*term<<"\n";
        }
#else
        // this condition is relevant
        // (make it a condition when tnxt != fnxt AND tnxt/fnxt != null)
        if(tnxt != nullptr && fnxt != nullptr
            && backs.find(term) == backs.end()){
          // errs()<<"numinst!=0: tnxt!=fnxt, term not in backs:\n";
          // errs()<<*term<<"\n";
          taints.insert(term);
        }

        // if(tnxt==nullptr)
        //     errs()<<"numinst!=0: tnxt is null\n"<<*tval;
        // if(fnxt!=nullptr){
        //     errs()<<"numinst!=0: fnxt is not null\n"<<*fval;
        //     errs()<<"numinst!=0: returned  fnxt\n"<<*fnxt->getBlock();
        // }
        // if(fnxt==nullptr)
        //     errs()<<"numinst!=0: fnxt is null\n"<<*fval;
        // if(tnxt!=nullptr){
        //     errs()<<"numinst!=0: tnxt is not null\n"<<*tval;
        //     errs()<<"numinst!=0: returned  tnxt\n"<<*tnxt->getBlock();
        // }
#endif
      }
      // return
      hists.pop_back();
      // errs()<<"7----------------------------------\n";
      return slice;
    }

    else {
      // follow links
      tnxt = taintCondVar(tval, hists, dedup, taints);
      fnxt = taintCondVar(fval, hists, dedup, taints);

      if(tnxt == fnxt){
        // errs()<<"numinst==0: tnxt==fnxt\n";
        // this condition is irrelevant
        dedup.insert(make_pair(bb, tnxt));

        // return
        hists.pop_back();
        // errs()<<"8----------------------------------\n";
        return tnxt;
      }

      else {
#ifdef KSYM_CONFIG_SLICE_COMPLETE
        // this condition is relevant
        // (make it a condition as long as tnxt != fnxt)
        if(backs.find(term) == backs.end()){
          // errs()<<"numinst==0: tnxt!=fnxt, term not in backs:\n";
          // errs()<<*term<<"\n";
          taints.insert(term);
        }

        // mark it as visited
        dedup.insert(make_pair(bb, slice));
        if(std::find(suits.begin(),suits.end(),slice) == suits.end())
          suits.push_back(slice);

        // return
        hists.pop_back();
        return slice;
#else
        if(tnxt == nullptr){
          dedup.insert(make_pair(bb, fnxt));
          // errs()<<"numinst==0: tnxt!=fnxt, tnxt=null:\n";
          // errs()<<*term<<"\n";

          // return
          hists.pop_back();
          // errs()<<"9----------------------------------\n";
          return fnxt;
        }

        if(fnxt == nullptr){
          dedup.insert(make_pair(bb, tnxt));
          // errs()<<"numinst==0: tnxt!=fnxt, fnxt=null:\n";
          // errs()<<*term<<"\n";

          // return
          hists.pop_back();
          // errs()<<"10---------------------------------\n";
          return tnxt;
        }

        // this condition is relevant
        // (make it a condition when tnxt != fnxt AND tnxt/fnxt != null)
        if(backs.find(term) == backs.end()){
          // errs()<<"numinst==0: tnxt!=fnxt, f/tnxt!=null, term not in backs:\n";
          // errs()<<*term<<"\n";
          taints.insert(term);
        }

        // mark it as visited
        dedup.insert(make_pair(bb, slice));
        suits.insert(slice);

        // return
        hists.pop_back();
        // errs()<<"11---------------------------------\n";
        return slice;
#endif
      }
    }
  }
  if(num >= 3){
    //TODO: handles callbr
    //errs()<<"branch successor more than 2:"<<num<<"successor:"<<*term<<'\n';
    abortCurSeed = true;
    hists.pop_back();
    return nullptr;
  }
}

/*
  connect all the SliceBlock with their successors and predecessors.
 */
void Slice::expand(SliceBlock *item) {
  BasicBlock *bb = item->getBlock();

  // init vars
  BasicBlock *base, *cur, *nxt;
  SliceBlock *slice;

  set<BasicBlock *> his;
  queue<BasicBlock *> que;

  // expand preds
  pred_iterator pi = pred_begin(bb), pe = pred_end(bb);
  for(; pi != pe; ++pi){
    base = *pi;
    his.clear();

    his.insert(base);
    que.push(base);

    while(!que.empty()){
      cur = que.front();
      que.pop();

      if(reach.find(cur) == reach.end()){
        continue;
      }

      slice = cache[cur];
      //if(suits.find(slice) != suits.end()){
      if(std::find(suits.begin(),suits.end(),slice) != suits.end()){
        slice->addSucc(item);
        item->addPred(slice);
        item->addPTabEntry(slice, base);
      } else {
        pred_iterator xi = pred_begin(cur), xe = pred_end(cur);
        for(; xi != xe; ++xi){
          nxt = *xi;
          if(his.find(nxt) == his.end()){
            his.insert(nxt);
            que.push(nxt);
          }
        }
      }
    }
  }

  // expand succs
  succ_iterator si = succ_begin(bb), se = succ_end(bb);
  for(; si != se; ++si){
    base = *si;
    his.clear();

    his.insert(base);
    que.push(base);

    while(!que.empty()){
      cur = que.front();
      que.pop();

      if(reach.find(cur) == reach.end()){
        continue;
      }

      slice = cache[cur];
      if(std::find(suits.begin(),suits.end(),slice) != suits.end()){
        slice->addPred(item);
        item->addSucc(slice);
        item->addSTabEntry(slice, base);
      } else {
        succ_iterator xi = succ_begin(cur), xe = succ_end(cur);
        for(; xi != xe; ++xi){
          nxt = *xi;
          if(his.find(nxt) == his.end()){
            his.insert(nxt);
            que.push(nxt);
          }
        }
      }
    }
  }
}

// Slice construction
Slice::Slice(set<BasicBlock *>& pred, Instruction *inst) : reach(pred) {
  // pre-process
  collectInsts(reach, scope);
  BasicBlock *head = &(inst->getParent()->getParent()->getEntryBlock());
  // collect initial taint set
  set<Value *> taints;


  // init data structs
  list<BasicBlock *> hists;
  //list<SliceBlock *> paths;
  map<BasicBlock *, SliceBlock *> dedup;
  // set abort to false
  abortCurSeed = false;
  // recursively expand
  taints.insert(inst);
  //if it's for inter procedural,
  // the seed inst is a call graph, which is not included in the btrace
  // so treat it specially
  int counter = 0;
  while(!taints.empty()){
    // backward and forward trace
    do {
      if(counter == 0){
          btraceSpecial(cast<CallInst>(inst));
          counter++;
      }else{
        for(Value *v : taints){
          //errs()<<"taints:"<<*v<<"\n";
          btrace(v);
        }
      }
      taints.clear();
      // abort as soon as possible
      if (abortCurSeed)
          return;

      for(Value *v : roots){
        follow(v, taints);
      }

      if (abortCurSeed)
          return;
    } while(!taints.empty());

    // collect conditions
    for(auto const &i : cache){
      delete i.second;
    }

    cache.clear();
    suits.clear();
    for(BasicBlock *bb : reach){
      cache.insert(make_pair(bb, apply(bb)));
    }
    dedup.clear();
    hists.clear();
    entry = taintCondVar(head, hists, dedup, taints);
    if (abortCurSeed)
        return;
    assert(hists.empty());
    if(entry == nullptr){
        // under this case, entry being null means that
        // the buggy branch is a deadcode, so just abort
        errs()<<"ERR: deadcode spotted\n";
        abortCurSeed = true;
        return;
    }
    assert(entry != nullptr && std::find(suits.begin(),suits.end(),entry) != suits.end());
  }

  basis = cache[inst->getParent()];
  assert(basis != nullptr && std::find(suits.begin(),suits.end(),basis) != suits.end());

  // link BasicBlock and SliceBlock
  for(SliceBlock *item : suits){
    expand(item);
  }

  // verify the integrity of the slice
  verify();

#if defined(KSYM_DEBUG) && defined(KSYM_DEBUG_STAT)
  float count = 1.0;
  for(Value *v : backs){
    if(isa<Instruction>(v)){
      count++;
    }
  }

  errs() 
    << "Sliced: " 
    << int(count / scope.size() * 100) << "%" 
    << "\n";
#endif
}

Slice::Slice(Instruction* used,BasicBlock* start,BasicBlock*end,set<BasicBlock*>& pred):reach(pred) {
    // pre-process
    abortCurSeed = false;
    for(BasicBlock* bb:reach){
        SliceBlock *slice = new SliceBlock(bb);
        for(Instruction &inst : *bb){
            slice->addInst(&inst);
        }
        cache.insert(make_pair(bb,slice));
      if(std::find(suits.begin(),suits.end(),slice) == suits.end()) {
        suits.push_back(slice);
      }
    }
    basis = cache.at(used->getParent());
    entry = cache.at(start);
    // link BasicBlock and SliceBlock
    for(SliceBlock *item : suits){
        expand(item);
    }
    // verify the integrity of the slice
    verify();
}


// Slice verification
void Slice::verify() {
  for(SliceBlock *item : suits){
    // verify linkage between SliceBlocks
    SliceBlock::linkIter pi = item->predBegin(), pe = item->predEnd();
    for(; pi != pe; ++pi){
      assert((*pi)->hasSucc(item));
      assert(item->inPTab(*pi));
    }

    SliceBlock::linkIter si = item->succBegin(), se = item->succEnd();
    for(; si != se; ++si){
      assert((*si)->hasPred(item));
      assert(item->inSTab(*si));
    }

    // verify linkage between SliceBlock and BasicBlock;
    BasicBlock *bb = item->getBlock();

    pred_iterator bpi = pred_begin(bb), bpe = pred_end(bb);
    for(; bpi != bpe; ++bpi){
      if(!item->inPTab(*bpi)){
        assert(reach.find(*bpi) == reach.end() || std::find(suits.begin(),suits.end(),cache[*bpi]) == suits.end());
      }
    }

    succ_iterator bsi = succ_begin(bb), bse = succ_end(bb);
    for(; bsi != bse; ++bsi){
      if(!item->inSTab(*bsi)){
        assert(
            reach.find(*bsi) == reach.end() ||
            std::find(suits.begin(),suits.end(),cache[*bsi]) == suits.end());
      }
    }
  }
  //errs()<<"wholeBB:";
  //for(SliceBlock *item : suits){
  //    errs()<<item->getBlock()->getName()<<' ';
 // }
 // errs()<<'\n';
}

LLVMSlice::LLVMSlice(Slice *slice) {
  SliceBlock *ptr;
  LLVMSliceBlock *cur;
    
  SliceBlock *entry = slice->getEntry();
  SliceBlock *basis = slice->getBasis();

  // ensure that the entry block is at the front
  cur = new LLVMSliceBlock(this, entry);
  blocks.push_back(cur);
  lookup.insert(make_pair(entry, cur));

  Slice::iterator i = slice->begin(), ie = slice->end();
  for(; i != ie; ++i){
    ptr = *i;
    if(ptr == entry || ptr == basis){
      continue;
    }

    cur = new LLVMSliceBlock(this, ptr);
    blocks.push_back(cur);
    lookup.insert(make_pair(ptr, cur));
  }

  // ensure that the basis block is at the back
  if(basis != entry){
    cur = new LLVMSliceBlock(this, basis);
    blocks.push_back(cur);
    lookup.insert(make_pair(basis, cur));
  }

  // build links
  i = slice->begin(), ie = slice->end();
  for(; i != ie; ++i){
    ptr = *i;
    cur = lookup[ptr];

    SliceBlock::linkIter pi = ptr->predBegin(), pe = ptr->predEnd();
    for(; pi != pe; ++pi){
      cur->addPred(lookup[*pi]);
    }

    SliceBlock::linkIter si = ptr->succBegin(), se = ptr->succEnd();
    for(; si != se; ++si){
      cur->addSucc(lookup[*si]);
    }
  }
}
