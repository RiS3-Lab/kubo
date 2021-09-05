//
// Created by lcm on 1/20/2021.
//

#include "Project.h"
//declaration


UB_root::UB_root(SENode* special_call,set<int> userArgs, ModuleOracle& mo):usrArgs(userArgs),mo(mo){
    CallInst* ubhandler = cast<CallInst>(special_call->getVal());
    Instruction * term = seed2root(ubhandler);
    SEGraph* seg = special_call->getGraph();
    root = seg->getNodeProbe(term);
    if(root == nullptr){
        //seg->displayForDeps();
        errs()<< "the missing terminator "<<*term<<'\n';
        errs()<<"the label of the UB checking branch:"<<term->getParent()->getName().str()<<'\n';
        errs()<<"the label of the UB handling branch:"<<ubhandler->getParent()->getName().str()<<'\n';
        llvm_unreachable("conditional jump missing from segraph");
    }
}

UB_root::~UB_root(){
    for(auto each_map : cache){
        each_map.second.clear();
    }
    cache.clear();
    set<F_AST*> deleted;
    for(auto cached:cached_formula){
        if(deleted.find(cached.second) == deleted.end()){
            delete cached.second;
            deleted.insert(cached.second);
        }
    }
    cached_formula.clear();
}
string UB_root::stringBinOp(F_Op binop) {
    switch (binop){
        case F_Add:
            return "add";
        case F_Sub:
            return "sub";
        case F_Mul:
            return "mul";
        case F_UDiv:
            return "udiv";
        case F_SDiv:
            return "sdiv";
        case F_URem:
            return "urem";
        case F_SRem:
            return "srem";
        case F_Shl:
            return "shl";
        case F_LShr:
            return "lshr";
        case F_AShr:
            return "ashr";
        case F_And:
            return "and";
        case F_Or:
            return "or";
        case F_Xor:
            return "xor";
        case F_Fadd:
            return "fadd";
        case F_Fsub:
            return "fsub";
        case F_Fmul:
            return "fmul";
        case F_Fdiv:
            return "fdiv";
        case F_Frem:
            return "frem";
    }
}

string UB_root::stringVal(val_kind kind){
    switch (kind){
        case F_Boolean:
            return "bool";
        case F_SInt:
            return "sint";
        case F_UInt:
            return "uint";
        case F_Float:
            return "float";
        case F_Double:
            return "double";
        case F_Ptr:
            return "ptr";
        case F_BigInt:
            return "bigint";
        case F_dummy:
            return "dummy";
    }
}

string UB_root::stringKind(formalize_kind kind) {
    switch(kind){
        case F_Para:
            return "para";
        case F_User:
            return "user";
        case F_Glb:
            return "glb";
        case F_Alloc:
            return "alloc";
        case F_Malloc:
            return "malloc";
        case F_Const:
            return "const";
        case F_Call:
            return "call";
        case F_Unknown:
            return "unknown";
        case F_Calculated:
            return "calculated";
        case F_Cmped:
            return "cmped";
    }
}


string UB_root::getASTName(F_AST *astNode) {
    if(F_BinOp* binop = dyn_cast<F_BinOp>(astNode)){
        return fmt::format("{},{}",binop->id,binop->type);
    }else if(F_CastOp* castop = dyn_cast<F_CastOp>(astNode)){
        return fmt::format("{},{},{}",castop->id,stringVal(castop->target_v_kind),castop->target_bitwidth);
    }else if(F_Val* var = dyn_cast<F_Val>(astNode)){
        return fmt::format("{},{},{},{}",var->id,stringVal(var->v_kind),stringKind(var->kind),var->bitWidth);
    }else{
        llvm_unreachable("unknown ast node\n");
    }
}

string UB_root::stringUB_types(UB_types ubt){
    switch(ubt){
        case uadd:
            return "uadd";
        case usub:
            return "usub";
        case umul:
            return "umul";
        case sadd:
            return "sadd";
        case ssub:
            return "ssub";
        case smul:
            return "smul";
        case negate_overflow:
            return "negate_OF";
        case align_err:
            return "return_err";
        case bool_err:
            return "bool_err";
        case array_bounds:
            return "array_bound";
        case enum_err:
            return "enum_err";
        case div_by_0:
            return "divBy0";
        case not_null_err:
            return "not_null";
        case shift_out_of_bound:
            return "shift_err";
        default:
            return "unknown";
    }
}

void UB_root::renderAST(stringstream& ss_t) {
    assert(cached_formula.find(root) != cached_formula.end());
    F_AST*root_ast = cached_formula.at(root);
    ss_t<<"digraph G {\n";
    //fw->writeLine("digraph G {\n");
    queue<F_AST*> work_queue;
    work_queue.push(root_ast);
    while(work_queue.size() > 0){
        F_AST* from = work_queue.front();
        work_queue.pop();

        if(F_BinOp* binop = dyn_cast<F_BinOp>(from)){
            int id = binop->id;
            int leftid = binop->left->id;
            int rightid = binop->right->id;

            string name = getASTName(binop);
            string attr("[shape=triangle,label=\"");

            ss_t << fmt::format("\t{} {}{}\"];\n",id,attr,name);

            ss_t << fmt::format("\t{} -> {};\n",id,leftid);

            ss_t<< fmt::format("\t{} -> {};\n",id,rightid);
            //fw->writeLine(ss.str());

            work_queue.push(binop->left);
            work_queue.push(binop->right);
        }else if(F_CastOp* castop = dyn_cast<F_CastOp>(from)){
            int id = castop->id;
            int srcid = castop->src->id;
            string name = getASTName(castop);
            string attr("[shape=box,label=\"");
            ss_t<< fmt::format("\t{} {}{}\"];\n",id,attr,name);
            ss_t<<fmt::format("\t{} -> {};\n",id,srcid);
            work_queue.push(castop->src);
        }else if(F_Val* var = dyn_cast<F_Val>(from)){
            int id = var->id;
            string name = getASTName(var);
            string attr("[shape=oval,label=\"");
            ss_t<<fmt::format("\t{} {}{}\"];\n",id,attr,name);
        }else{
            llvm_unreachable("unknown ast node\n");
        }
    }
    ss_t<<"}\n";
    //fw->writeLine("}\n");
}

void UB_root::buildConst(SENodeLeaf*leafNode){
    int id = leafNode->getId();
    if(SENodeCInt* intNode = dyn_cast<SENodeCInt>(leafNode)){
        ConstantInt* cInt = intNode->getCastedVal();
        if(cInt->getBitWidth() <= 64){
            U_IntFloat const_var(F_SInt);
            const_var.integer_var = cInt->getSExtValue();
            cached_formula[leafNode] = new F_Val(id,F_Const,cInt->getBitWidth(),F_UInt,const_var);
        }else{
            U_IntFloat const_var(F_BigInt);
            const_var.big_int = cInt->getValue().toString(/*radix=*/10, /*isSigned=*/true);
            cached_formula[leafNode] = new F_Val(id,F_Const,cInt->getBitWidth(),F_BigInt,const_var);
        }
    }else if(SENodeCFloat* floatNode = dyn_cast<SENodeCFloat>(leafNode)){
        ConstantFP* fp = floatNode->getCastedVal();
        uint64_t width = mo.getTypeWidth(fp->getType());
        if(width == 32){
            U_IntFloat const_var(F_Float);
            const_var.float_var = fp->getValueAPF().convertToFloat();
            cached_formula[leafNode] = new F_Val(id,F_Const,width,F_Float,const_var);
        }else if(width == 64){
            U_IntFloat const_var(F_Double);
            const_var.double_var = fp->getValueAPF().convertToDouble();
            cached_formula[leafNode] = new F_Val(id,F_Const,width,F_Double,const_var);
        }
    }else if(SENodeCNull* nullNode = dyn_cast<SENodeCNull>(leafNode)){
        U_IntFloat const_var(F_Ptr);
        const_var.integer_var = 0;
        cached_formula[leafNode] = new F_Val(id,F_Const,mo.getPointerWidth(),F_Ptr,const_var);
    }else{
        llvm_unreachable("unsupported leaf nodes when building Consts\n");
    }
}
void UB_root::initialize_leaves() {
    SEGraph* seg = root->getGraph();
    for(auto leafIt = seg->nodes.begin(); leafIt != seg->nodes.end();leafIt++){
        SENode* eachleaf = leafIt->second;

        if(!isa<SENodeLeaf>(eachleaf) && !isa<SENodeVar>(eachleaf))
            continue;
        if(isa<SENodeLocal>(eachleaf)){
            insert_cache(eachleaf,-1,F_Alloc);
        }else if(SENodeParam * nodeParam = dyn_cast<SENodeParam>(eachleaf)){
            Argument * a = nodeParam->getCastedVal();
            int argNo = a->getArgNo();
            // if the argNo is in userArg, then it's user
            if(usrArgs.find(argNo) != usrArgs.end()){
                insert_cache(eachleaf,-1,F_User);
            }
            else{
                insert_cache(eachleaf,-1,F_Para);
                non_user_args.insert(a);
            }
        }else if(SENodeGlobal* glbNode =  dyn_cast<SENodeGlobal>(eachleaf)){
            SENodeInst* user = nullptr;
            set<SENode*> work_queue;
            work_queue.insert(glbNode);
            bool found = false;
            while(work_queue.size() > 0 && !found){
                SENode* cur_node = *(work_queue.begin());
                work_queue.erase(cur_node);
                if(SENodeInst* inst_node = dyn_cast<SENodeInst>(cur_node)){
                    user = inst_node;
                    found = true;
                }else{
                    for(auto user_it = cur_node->usrBegin(); user_it != cur_node->usrEnd(); user_it++){
                        work_queue.insert(*user_it);
                    }
                }
            }

            if(user == nullptr){
                // just mark it as glb, since it's not used by any instruction, so cannot be affected the UB.
                insert_cache(eachleaf,-1,F_Glb);
                continue;
            }
            GlobalValue* glb = glbNode->getCastedVal();
            pair<formalize_kind,struct SysctlInfo*> is_sysctl = mo.issysctl(glb,cast<Instruction>(user->getVal())->getFunction());
            if(is_sysctl.first == F_User){
                // it's purely userspace input
                insert_cache(eachleaf,-1,F_User);
            }else if(is_sysctl.first == F_Const){
                insert_cache(eachleaf,-1,F_Const);
            }
            else{
                // the glbs used by functions should not be considered as relevant glbs
                // it's mainly to deal with the __ubsan functions which use global variables to annotate the buggy location
                // since it's only for debugging purpose
                // also global variables should not be passed into function as parameters after all
                // they're globals for god's sake.
                Type* glb_ty = glbNode->getVal()->getType();
                if(GlobalVariable* glb_var = dyn_cast<GlobalVariable>(glb)){
                    if(glb_var->isConstant()) {
                        insert_cache(eachleaf, -1, F_Const);
                        continue;
                    }
                }
                if(PointerType * ptr_tp = dyn_cast<PointerType>(glb_ty)){
                    StructType* ptr_by_ty = dyn_cast<StructType>(ptr_tp->getElementType());
                    GlobalVariable* glb_var = dyn_cast<GlobalVariable>(glb);
                    queue<pair<StructType*,ConstantStruct*> > work_queue;
                    if(ptr_by_ty && glb_var && glb_var->hasInitializer()) {
                        ConstantStruct* initializer_struct = dyn_cast<ConstantStruct>(glb_var->getInitializer());
                        if(initializer_struct != nullptr){
                            // check if it's zero initializer
                            // the thing (debug global variable) we're looking for don't have zero initializer
                            work_queue.push(make_pair(ptr_by_ty,initializer_struct));
                        }
                        // find an element named "@.src.(xxx)" or "@.str.(xxx)", if the struct has this , then this is
                        // just debug global variable, which should not be considered as meaningful global variable
                        bool found_dbg_symbol = false;
                        while(!work_queue.empty()) {
                            StructType* stc_ty = work_queue.front().first;
                            ConstantStruct* stc_var = work_queue.front().second;
                            work_queue.pop();
                            int num_elements = stc_ty->getNumElements();

                            for(int element_idx = 0 ; element_idx < num_elements;element_idx++){
                                Constant* tmp_con = stc_var->getAggregateElement(element_idx);
                                Type* tmp_con_ty = tmp_con->getType();
                                if(StructType* new_stc_ty = dyn_cast<StructType>(tmp_con_ty)){
                                    if(ConstantStruct* new_con_str = dyn_cast<ConstantStruct>(tmp_con)){
                                        work_queue.push(make_pair(new_stc_ty,new_con_str));
                                    }
                                }else{
                                    string tmp_con_name = tmp_con->getName().str();
                                    if(tmp_con_name.find(".src") != string::npos || tmp_con_name.find(".str") != string::npos){
                                        found_dbg_symbol = true;
                                        while(!work_queue.empty()){
                                            work_queue.pop();
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                        if(found_dbg_symbol){
                            insert_cache(eachleaf,-1,F_Const);
                        }else{
                            insert_cache(eachleaf,-1,F_Glb);
                            non_user_glbs.insert(glb);
                        }
                    }else {
                        insert_cache(eachleaf,-1,F_Glb);
                        non_user_glbs.insert(glb);
                    }
                }else{
                    insert_cache(eachleaf,-1,F_Glb);
                    non_user_glbs.insert(glb);
                }
            }
        }else if(SENodeLeaf* leafNode = dyn_cast<SENodeLeaf>(eachleaf)){
            Type*ty = leafNode->getVal()->getType();
            if(StructType* stc_ty = dyn_cast<StructType>(ty)){
                unsigned num_elements = stc_ty->getNumElements();
                for(int each_element = 0 ; each_element < num_elements ; each_element++){
                    insert_cache(eachleaf,each_element,F_Const);
                }
            }
            else if(ArrayType* arr_ty = dyn_cast<ArrayType>(ty)){
                unsigned num_elements = arr_ty->getNumElements();
                for(int each_element = 0 ; each_element < num_elements ; each_element++){
                    insert_cache(eachleaf,each_element,F_Const);
                }
            }
            else{
                insert_cache(eachleaf,-1,F_Const);
            }

        }else{
            assert(isa<SENodeUndef>(eachleaf));
            insert_cache(eachleaf,-1,F_Unknown);
            continue;
        }
    }
}

bool UB_root::collectScope(){
    queue<SENode*> workqueue;
    workqueue.push(root);
    nodes.insert(make_pair(root->getId(),root));
    while(workqueue.size() > 0){
        SENode* cur = workqueue.front();
        workqueue.pop();
        for(auto depIt = cur->depBegin(); depIt != cur->depEnd(); depIt++){
            SENode* cur = *depIt;
            //change dep if it's ext val
            if(SENodeExtVal* extNode = dyn_cast<SENodeExtVal>(cur)){
                ExtractValueInst* extInst = extNode->getCastedVal();
                unsigned extidx= extInst->getIndices()[0];
                if(extidx != 0){
                    return false;
                }
                assert(extNode->numDeps() == 1);
                SENodeInsertVal* insertNodeLater = cast<SENodeInsertVal>(*(extNode->depBegin()));
                SENodeInsertVal* insertNodeBefore = cast<SENodeInsertVal>(root->getGraph()->getNodeProbe(insertNodeLater->getCastedVal()->getAggregateOperand()));
                assert(insertNodeBefore!= nullptr);
                InsertValueInst* insertValueInst = insertNodeBefore->getCastedVal();
                Value* realValue = insertValueInst->getInsertedValueOperand();
                SENode* realNode = root->getGraph()->getNodeProbe(realValue);
                assert(realValue != nullptr);
                aliased.insert(make_pair(realNode,extNode));
                cur = realNode;
                nodes.insert(make_pair(realNode->getId(),realNode));
            }else if(isa<SENodeSelect>(cur)){
                //TODO:support select instruction
                return false;
            }else{
                nodes.insert(make_pair((*depIt)->getId(),*depIt));
            }
            //push cur into work queue
            if(isa<SENodeVar>(*depIt) || isa<SENodeLeaf>(*depIt)){
                assert((*depIt)->numDeps() == 0);
            }else{
                workqueue.push(cur);
            }
        }
    }
    return true;
}

void collectDepChains(SENode* curNode,vector<SENode*>& depChain,vector<vector<SENode*>>* depChains){
    depChain.push_back(curNode);
    if(curNode->numUsrs() == 0){
        vector<SENode*>tmp(depChain);
        depChains->push_back(tmp);
    } else {
        for(auto usrIt = curNode->usrBegin(); usrIt != curNode->usrEnd(); ++usrIt){
            collectDepChains(*usrIt,depChain,depChains);
        }
    }
    depChain.pop_back();
}

void collectUseChains(SENode* curNode,vector<SENode*>& useChain,vector<vector<SENode*>>* useChains){
    useChain.push_back(curNode);
    if(curNode->numUsrs() == 0){
        vector<SENode*>tmp(useChain);
        useChains->push_back(tmp);
    } else {
        for(auto usrIt = curNode->usrBegin(); usrIt != curNode->usrEnd(); ++usrIt){
            collectUseChains(*usrIt,useChain,useChains);
        }
    }
}
// end will not be reached
bool pointerPropogation(vector<SENode*>::iterator begin, vector<SENode*>::iterator end){
    SENode* ptrNode = *begin;
    vector<SENode*>::iterator cur = begin;
    cur++;
    for(;cur != end;cur++){
        if(SENodeCast* castInst = dyn_cast<SENodeCast>(*cur)){
            //cast pointer to other pointer(or non pointer?)
            continue;
        }
        else if(SENodeGEP* gepInst = dyn_cast<SENodeGEP>(*cur)){
            //GEP just pass on
            continue;
        }
        else if(SENodePhi * phiInst = dyn_cast<SENodePhi>(*cur)){
            //phi just duplicate this pointer
            continue;
        }else{
            // the other Instructions are meaningful use
            return false;
        }
    }
    return true;
}

void mergeStatusAndFormula(UB_root* another){

}

bool UB_root::memDepAna(SENode* ptrNode){
    vector<vector<SENode*> >depChains;
    vector<SENode*> depChain;
    collectDepChains(ptrNode,depChain,&depChains);
    //filter out the chains that has root
    for(auto chainIt = depChains.begin(); chainIt != depChains.end(); ){
        if(find(chainIt->begin(),chainIt->end(),root) != chainIt->end()){
            chainIt = depChains.erase(chainIt);
        }else{
            chainIt++;
        }
    }
    map<int,vector<SENode*>> sortedChains;
    for(auto eachChain = depChains.begin();eachChain != depChains.end();eachChain++) {
        sortedChains.insert(make_pair((*eachChain).back()->getId(), *eachChain));
    }
    bool changed = false;
    //look for store/callInst/AsmCall/BranchInst that can potentially change the status
    for(auto eachChain = sortedChains.begin();eachChain != sortedChains.end();eachChain++){
        auto start = eachChain->second.begin();
        auto eachNodeIt = eachChain->second.begin();
        eachNodeIt++;
        for(;eachNodeIt != eachChain->second.end();eachNodeIt++){
            SENode* usrNode = *eachNodeIt;
            SENodeCall* callNode = dyn_cast<SENodeCall>(usrNode);
            SENodeAsm * asmNode = dyn_cast<SENodeAsm>(usrNode);
            if(callNode || asmNode){
                CallInst* callInst = callNode != nullptr ? callNode->getCastedVal() : asmNode->getCastedVal();
                if(pointerPropogation(start,eachNodeIt)){
                    // just pointer propogation
                    SENode* callDirectDep = *(eachNodeIt - 1);
                    changed = true;
                    if(const FetchDef * fetchDef = Fetch::isfetch(callInst)){
                        int dstIdx = fetchDef->dst;
                        if(dstIdx == -1){
                            //dst is the return value
                            insert_cache(usrNode,-1,F_User);
                            //annotate user input(since it's passed to fetch function as source)
                            for(auto tmpIt = start;tmpIt != eachNodeIt;tmpIt++){
                                insert_cache(*tmpIt,-1,F_User);
                            }
                        }else{
                            SENode* fetchDstNode = root->getGraph()->getNodeProbe(callInst->getArgOperand(dstIdx));
                            SENode* fetchSrcNode = root->getGraph()->getNodeProbe(callInst->getArgOperand(fetchDef->src));
                            assert(fetchDstNode != nullptr);
                            if(fetchDstNode == callDirectDep){
                                // this pointer is used as fetched dst
                                for(auto tmpIt = start;tmpIt != eachNodeIt;tmpIt++){
                                    insert_cache(*tmpIt,-1,F_User);
                                }
                                insert_cache(fetchSrcNode,-1,F_User);
                            }else if(fetchSrcNode == callDirectDep){
                                for(auto tmpIt = start;tmpIt != eachNodeIt;tmpIt++){
                                    insert_cache(*tmpIt,-1,F_User);
                                }
                                insert_cache(fetchSrcNode,-1,F_User);
                            }
                        }
                    }else{
                        // it's other function which accept this pointer as a parameter, this might change the content, so mark it
                        for(auto tmpIt = start;tmpIt != eachNodeIt;tmpIt++){
                            insert_cache(*tmpIt,-1,F_Call);
                        }
                    }
                }
                //no need to proceed further
                break;
            }else if(SENodeStore* storeNode = dyn_cast<SENodeStore>(usrNode)){
                SENode* callDirectDep = *(eachNodeIt - 1);
                if(pointerPropogation(start,eachNodeIt)){
                    changed = true;
                    SENode* ptrOprand = root->getGraph()->getNodeProbe(storeNode->getCastedVal()->getPointerOperand());
                    SENode* valueOprand = root->getGraph()->getNodeProbe(storeNode->getCastedVal()->getValueOperand());
                    assert(ptrOprand != nullptr && valueOprand != nullptr);
                    if(callDirectDep == valueOprand){
                        // store the content from the ptr to another pointer
                        // we are not interested in this
                        break;
                    }
                    // now this pointer is being overwritten
                    assert(callDirectDep == ptrOprand);
                    if(cache.find(valueOprand) == cache.end()){
                        // if the value operand is missing in the cache
                        UB_root* value_root = new UB_root(valueOprand,usrArgs,mo);
                        //if formalization failed
                        if(!value_root->expandAndFormalize()){
                            insert_cache(valueOprand,-1,F_Unknown);
                        }
                            //if formalization success
                        else{
                            mergeStatusAndFormula(value_root);
                        }
                    }else{
                        for(auto tmpIt = start;tmpIt != eachNodeIt;tmpIt++){
                            insert_cache(*tmpIt,cache.at(valueOprand));
                        }
                    }
                }
                break;
            }
        }
    }
    return changed;
}
void UB_root::processCast(SENodeCast* castNode){
    //ignore CastTO ptr for now
    SEOpv* op = castNode->getSingleOpv();
    CastInst* castInst = castNode->getCastedVal();
    Type * src_ty = castInst->getSrcTy();
    Type * dst_type = castInst->getDestTy();
    int id = castNode->getId();

    assert(castNode->numDeps() == 1);
    SENode* depNode = *(castNode->depBegin());
    assert(cache.find(depNode) != cache.end());
    cache[castNode] = cache.at(depNode);
    return;
}

void UB_root::buildMem(SENode*node,formalize_kind kind) {
    Value* inst = node->getVal();
    Type * result_ty = inst->getType();
    U_IntFloat dummy_var(F_dummy);
    int id = node->getId();
    assert(!result_ty->isPointerTy());
    if(result_ty->isIntegerTy()){
        int bitwidth = mo.getTypeWidth(result_ty);
        if(bitwidth <= 64){
            cached_formula[node] = new F_Val(id,kind,bitwidth,F_UInt,dummy_var);
        }else{
            cached_formula[node] = new F_Val(id,kind,bitwidth,F_BigInt,dummy_var);
        }
    }else if(result_ty->isPointerTy()){
        int bitwidth = mo.getTypeWidth(result_ty);
        if(bitwidth == 32){
            cached_formula[node] = new F_Val(id,kind,bitwidth,F_Float,dummy_var);
        }else if(bitwidth == 64){
            cached_formula[node] = new F_Val(id,kind,bitwidth,F_Double,dummy_var);
        }else{
            llvm_unreachable("float longer than double!\n");
        }
    }else{
        llvm_unreachable("unsupported loaded mem type");
    }
}

void UB_root::processLoad(SENodeLoad *loadNode) {
    //sanity check
    assert(loadNode->numDeps() == 1);
    SENode* depNode = *(loadNode->depBegin());
    assert(cache.find(depNode) != cache.end());
    cache[loadNode] = cache.at(depNode);


    assert(loadNode->numDeps() == 1);
    SENode* ptrNode = *(loadNode->depBegin());
    //ptr must be assigned with a status
    assert(cache.find(ptrNode) != cache.end());
    //ptr must no appear has a AST node
    formalize_kind ptrStatus = cache.at(ptrNode).at(-1);
    if(loadNode->getCastedVal()->getType()->isPointerTy()){
        // this is loading a ptr from a double ptr
        // so just propagate the status
        cache[loadNode] = cache.at(ptrNode);
        //defer the cache formula
        return;
    }
}
void UB_root::processCalc(SENodeCalc* calcNode){
    SEOpv* op = calcNode->getSingleOpv();
    assert(op->numDeps() == 2);
    SENode* lhs = op->at(0);
    SENode* rhs = op->at(1);
    int id = calcNode->getId();
    if( !(cache.find(lhs) != cache.end() && cache.find(rhs) != cache.end())){
        //calcNode->getGraph()->displayDepForNode(calcNode);
        displayNodes();
        //errs()<<"cur:"<<*calcNode->getCastedVal()->getFunction();
        assert(false);
    }
    formalize_kind ret = merge_kind(cache.at(lhs).at(-1), cache.at(rhs).at(-1));
    insert_cache(calcNode,-1,ret);
}

void UB_root::displayNodes(){
    for(auto eachNode:nodes){
        eachNode.second->display();
    }
}

void UB_root::updateStrip(SENode*node,formalize_kind f){
    Value* v = node->getVal();
    assert(v->getType()->isPointerTy());
    Value* stripped = v->stripPointerCasts();
    if(stripped != v){
        SENode* strippedNode = node->getGraph()->getNodeProbe(stripped);
        insert_cache(strippedNode,-1,f,true);
    }
}
void UB_root::processCall(SENode* call_node){
    CallInst* callInst = cast<CallInst>(call_node->getVal());
    const FetchDef* fth = Fetch::isfetch(callInst);
    // update the parameters
    if(fth != nullptr){
        // a fetch function
        if(fth->dst == -1){
            //if return value is the fetched data
            insert_cache(call_node,-1,F_User,true);
            insert_cache(call_node,0,F_User,true);
            insert_cache(call_node,1,F_Const,true);
            insert_cache(call_node,2,F_Const,true);
        }else{
            SENode* fetchedDataNode = call_node->getGraph()->getNodeProbe(callInst->getArgOperand(fth->dst));
            assert(fetchedDataNode != nullptr);
            insert_cache(fetchedDataNode,-1,F_User,true);
            updateStrip(fetchedDataNode,F_User);
            //deal with return value, assume, fetch function always succeeds
            insert_cache(call_node,-1,F_Const,true);
        }
    }
    else{
        // a normal call
        Function * func = callInst->getCalledFunction();
        // deal with the parameters if they're passed in as a pointer
        if(func == nullptr ||( func != nullptr && func->getName().str().find("__ubsan") == string::npos )){
            //if(func != nullptr and func->getName().str() == "vfs_statfs"){
            //    __asm__("nop");
            //}
            int argNum = callInst->getNumArgOperands();
            for(int i = 0 ; i < argNum ; i++){
                Value* operand = callInst->getArgOperand(i);
                SENode*operandNode = root->getGraph()->getNodeProbe(operand);
                if(isa<SENodeUnhandledAsm>(call_node) || isa<SENodeUnhandledCall>(call_node)){
                    assert(operandNode != nullptr);
                }
                if(operandNode == nullptr){
                    continue;
                }
                // if pass a pointer
                if(operandNode->getVal()->getType()->isPointerTy()){
                    //displayCache();
                    insert_cache(operandNode,-1,F_Call,true);
                    //displayCache();
                }
            }
        }
        //deal with return value
        if(callInst->getType()->isPointerTy()){
            Function* called = callInst->getCalledFunction();
            if(called != nullptr){
                string funcName = called->getName().str();
                if(funcName=="__kmalloc" \
                    // if it's malloc function
                   || funcName == "__kmalloc_track_caller" \
                  || funcName == "__vmalloc" \
                  || funcName =="kmem_cache_alloc"\
                  || funcName == "krealloc"\
                  || funcName == "kvmalloc_node" \
                  || funcName == "vmalloc"){
                    insert_cache(call_node,-1,F_Malloc);
                }
                    //if it's memcpy
                else if(funcName == "memcpy"){
                    SENode* srcNode = call_node->getGraph()->getNodeProbe(callInst->getArgOperand(1));
                    SENode* dstNode = call_node->getGraph()->getNodeProbe(callInst->getArgOperand(0));
                    assert(srcNode && dstNode);
                    insert_cache(dstNode,cache.at(srcNode));
                    insert_cache(call_node,-1,F_Call);
                }else{
                    insert_cache(call_node,-1,F_Call);
                }
            }else{
                insert_cache(call_node,-1,F_Call);
            }
        }else{
            Type* return_type = callInst->getType();
            if(StructType* struct_ty = dyn_cast<StructType>(return_type)){
                unsigned num_elements = struct_ty->getNumElements();
                for(int i = 0 ; i < num_elements;i++){
                    insert_cache(call_node,i,F_Call);
                }
            }
            else if(ArrayType* array_ty = dyn_cast<ArrayType>(return_type)){
                uint64_t num_elements = array_ty->getNumElements();
                for(int i = 0 ; i < num_elements;i++){
                    insert_cache(call_node,i,F_Call);
                }
            }
            else{
                insert_cache(call_node,-1,F_Call);
            }
        }
    }
}

void UB_root::processInsertVal(SENodeInsertVal* node_insert){
    InsertValueInst * inst_insert = node_insert->getCastedVal();
    Value * inserted_val = inst_insert->getInsertedValueOperand();
    SENode* inserted_node = node_insert->getGraph()->getNodeProbe(inserted_val);
    formalize_kind target_kind = cache.at(inserted_node).at(-1);
    assert(inserted_node != nullptr);
    auto idx_it = inst_insert->idx_begin();

    assert(inst_insert->getNumIndices() == 1);
    int idx = *idx_it;

    Value * agg_val = inst_insert->getAggregateOperand();
    SENode* agg_node = node_insert->getGraph()->getNodeProbe(agg_val);
    assert(agg_node != nullptr);

    if(cache.find(agg_node) != cache.end()){
        insert_cache(node_insert,cache.at(agg_node));
        //overwrite the result
        insert_cache(node_insert,idx,target_kind,true);
    }else{
        formalize_kind target_kind = cache.at(inserted_node).at(-1);
        insert_cache(node_insert,idx,target_kind);
    }
}

void UB_root::processExtVal(SENodeExtVal* node_ext_val){
    ExtractValueInst* ext_val_inst = node_ext_val->getCastedVal();
    Value * agg_val = ext_val_inst->getAggregateOperand();
    SENode* agg_node = node_ext_val->getGraph()->getNodeProbe(agg_val);
    assert(agg_node != nullptr);

    assert(ext_val_inst->getNumIndices() == 1);
    int idx = *(ext_val_inst->idx_begin());

    assert(cache.find(agg_node) != cache.end());
    if(cache.at(agg_node).find(idx) == cache.at(agg_node).end()){
        displayCache();
        errs()<<"[Err] idx:"<<idx <<" is missing from agg node"<<*agg_val<<'\n';
        llvm_unreachable("missing idx from agg node\n");
    }
    formalize_kind target_kind = cache.at(agg_node).at(idx);
    insert_cache(node_ext_val,-1,target_kind);
}

formalize_kind UB_root::merge_kind(formalize_kind k1,formalize_kind k2){

    int score_k1 = mapped_kind.at(k1);
    int score_k2 = mapped_kind.at(k2);

    if(score_k1 >= score_k2){
        return k1;
    }
    else{
        return k2;
    }

}
void UB_root::insert_cache(SENode*node,int idx,formalize_kind k,bool over_write){
    bool in_cache = (cache.find(node) != cache.end());
    if(in_cache){
        if(not over_write){
            if(cache.at(node).find(idx) != cache.at(node).end()){
                displayCache();
                errs()<<"buggy:"<<*(node->getVal())<<'\n';
                assert(false);
            }
            auto ret = cache.at(node).insert(make_pair(idx,k));
            assert(ret.second);
        }else{
            cache.at(node)[idx] = k;
        }

    }else{
        auto ret = cache.insert(make_pair(node,map<int,formalize_kind>({{idx,k}})));
        assert(ret.second);
    }
}
void UB_root::insert_cache(SENode*node,map<int,formalize_kind> m){
    bool in_cache = (cache.find(node) != cache.end());
    if(in_cache){
        for(auto each_idx_it = m.begin(); each_idx_it != m.end();each_idx_it++){
            cache.at(node).insert(make_pair(each_idx_it->first,each_idx_it->second));
        }
    }else{
        cache.insert(make_pair(node, m));
    }
}

void UB_root::processSelectNode(SENodeSelect* select_node){

    SelectInst * select_inst = select_node->getCastedVal();
    Value * cond_val = select_inst->getCondition();
    Value * f_val = select_inst->getFalseValue();
    Value * t_val = select_inst->getTrueValue();
    SENode* cond_node = select_node->getGraph()->getNodeProbe(cond_val);
    SENode* f_node = select_node->getGraph()->getNodeProbe(f_val);
    SENode* t_node = select_node->getGraph()->getNodeProbe(t_val);
    assert(cond_node && f_node && t_node);
    formalize_kind ret = F_Const;
    ret = merge_kind(ret,cache.at(cond_node).at(-1));
    ret = merge_kind(ret,cache.at(f_node).at(-1));
    ret = merge_kind(ret,cache.at(t_node).at(-1));
    insert_cache(select_node,-1,ret);
}

void UB_root::processBrchNode(SENodeBranch* brch_node){
    int num_deps = brch_node->numDeps();
    if(num_deps == 1){
        SENode* dep = *(brch_node->depBegin());
        // branch instruction's dependent (cmp instruction)
        // should not have different fileds
        formalize_kind t = cache.at(dep).at(-1);
        insert_cache(brch_node,-1,t);
    }else if(num_deps == 0){
        // unconditional jump
    }else{
        // num deps bigger than 1f for a branch inst?
        llvm_unreachable("brch instruction's dep should not be over 1");
    }
}

void UB_root::processSeedNode(SENodeSpecialCall* seed_node){
    CallInst * seed_inst = seed_node->getCastedVal();
    string handler_name = seed_inst->getCalledFunction()->getName().str();

    if(handler_name == "__ubsan_handle_add_overflow" || \
        handler_name == "__ubsan_handle_sub_overflow" || \
        handler_name == "__ubsan_handle_mul_overflow" || \
        handler_name == "__ubsan_handle_divrem_overflow" || \
        handler_name == "__ubsan_handle_shift_out_of_bounds" || \
        handler_name == "handle_overflow"){
        Value * left_operand = seed_inst->getArgOperand(1);
        Value * right_operand = seed_inst->getArgOperand(2);
        SENode* left_node = seed_node->getGraph()->getNodeProbe(left_operand);
        SENode* right_node = seed_node->getGraph()->getNodeProbe(right_operand);
        if(!(left_node && right_node)){
            errs()<<"left:"<<*left_operand<<"|right operand:"<<*right_operand<<'\n';
            seed_node->getGraph()->displayForDeps();
            seed_node->getGraph()->getNodeProbe(right_operand);
            llvm_unreachable("nodes missing from the seg");
        }
        formalize_kind k1 = cache.at(left_node).at(-1);
        formalize_kind k2 = cache.at(right_node).at(-1);
        formalize_kind merged = merge_kind(k1,k2);
        insert_cache(seed_node,-1,merged);
    }else if(handler_name == "__ubsan_handle_negate_overflow" || \
            handler_name == "__ubsan_handle_out_of_bounds"){
        Value * operand = seed_inst->getArgOperand(1);
        SENode* op_node = seed_node->getGraph()->getNodeProbe(operand);
        assert(op_node);
        formalize_kind k1 = cache.at(op_node).at(-1);
        insert_cache(seed_node,-1,k1);
    }else{
        llvm_unreachable("unhandled UBSan handler.");
    }
}

void UB_root::processGEP(SENodeGEP* gepNode){
    GetElementPtrInst* gepInst = gepNode->getCastedVal();
    SENode* ptrNode = root->getGraph()->getNodeProbe(gepInst->getPointerOperand());
    Type* ptrTy = gepInst->getPointerOperand()->getType();

    string str;
    raw_string_ostream os(str);
    ptrTy->print(os);
    //TODO:test this
    if(os.str().find("%sk_buff") == 0){
        User::op_iterator i = gepInst->idx_begin();
        //second idx
        i++;
        if(ConstantInt* idx = dyn_cast<ConstantInt>(i)){
            int64_t value = idx->getSExtValue();
            if(value == 41)
            {
                insert_cache(gepNode,-1,F_User);
                return;
            }
        }

    }
    insert_cache(gepNode,cache.at(ptrNode));
}

void UB_root::processCmp(SENode* node){
    formalize_kind merged;
    int dep_count = 0;
    for(auto it = node->deps.begin(); it != node->deps.end();dep_count++, it++){
        if(cache.find(*it) == cache.end()){
            displayCache();
            errs() << *((*it)->getVal())<<"is missing in the cache\n";
            assert(false);
        }
        formalize_kind dep_kind = cache.at(*it).at(-1);
        if(dep_count == 0){
            // loop being executed the first time, initialize merged.
            merged = dep_kind;
        }else{
            merged = merge_kind(merged,dep_kind);
        }
    }
    if(mapped_kind.at(merged) == 2){
        //if the merged state is not F_User,
        // it means, F_User might be compared with higher state
        // need to update the F_User variable.
        for(auto it = node->deps.begin(); it != node->deps.end(); it++){
            if(cache.at(*it).at(-1) == F_User){
                cache.at(*it).at(-1) = F_Cmped;
                if((*it)->getVal()->getType()->isPointerTy()){
                    updateStrip(*it,F_Cmped);
                }
            }
        }
        insert_cache(node,-1,F_Cmped);
    }else if(mapped_kind.at(merged) < 2){
        insert_cache(node,-1,merged);
    }
}

void UB_root::processStore(SENodeStore* storeNode){
    StoreInst* storeInst = storeNode->getCastedVal();
    Value* ptrOp = storeInst->getPointerOperand();
    Value* valOp = storeInst->getValueOperand();
    SENode* valNode = root->getGraph()->getNodeProbe(valOp);
    assert(valNode);
    formalize_kind valState = cache.at(valNode).at(-1);
    SENode* ptrNode = root->getGraph()->getNodeProbe(ptrOp);
    assert(ptrNode);
    insert_cache(ptrNode,-1,valState,true);
    Value* strippedPtr = ptrOp->stripPointerCasts();
    if(strippedPtr != ptrOp){
        SENode* strippedPtrNode = root->getGraph()->getNodeProbe(strippedPtr);
        assert(strippedPtrNode);
        insert_cache(strippedPtrNode,-1,valState,true);
    }
}

void UB_root::processPhi(SENodePhi* phiNode){
    assert(phiNode->numDeps() == 1);
    SENode* depNode = *(phiNode->depBegin());
    if(cache.find(depNode) == cache.end()){
        displayCache();
        errs()<<"[err]"<<*(depNode->getVal())<<" is missing from the cache\n";
        assert(false);
    }
    insert_cache(phiNode,cache.at(depNode));
}
void UB_root::displayCache(){
    map<int,SENode*> id_map;
    for(auto each_node = cache.begin();each_node != cache.end();each_node++) {
        id_map.insert(make_pair(each_node->first->getId(),each_node->first));
    }
    for(auto it = id_map.begin(); it != id_map.end(); it++){
        errs()<<it->first<<","<<*(it->second->getVal())<<'\n';
        SENode* node = it->second;
        for(auto each_idx = cache.at(node).begin();each_idx != cache.at(node).end();each_idx++){
            errs()<<'('<<each_idx->first<<','<< stringKind(each_idx->second)<<")";
        }
        errs()<<'\n';
    }
    errs()<<"root:"<<*(root->getVal())<<'\n';
}

bool UB_root::root_controlled_by_user(){
    if(cache.find(root) == cache.end()){
        displayCache();
        errs()<<"seed is:"<<*(root->getVal())<<'\n';
        llvm_unreachable("root not in the cache.");
    }
    if(cache.at(root).at(-1) == F_User)
        return true;
    else
        return false;
}

bool UB_root::expandAndFormalize() {
    //bool carry_on = collectScope();
    //if(!carry_on)
    //    return false;
    //initialize the status

    initialize_leaves();

    // propogate the status
    for(auto eachNodeIt = root->getGraph()->nodes.begin();eachNodeIt != root->getGraph()->nodes.end();eachNodeIt++){
        SENode* eachNode = eachNodeIt->second;
        //errs()<<"processing:"<<*eachNode->getVal()<<'\n';
        if(isa<SENodeVar>(eachNode)|| isa<SENodeLeaf>(eachNode)){
            if(!isa<SENodeUndef>(eachNode))
                assert(cache.find(eachNode) != cache.end());
            continue;
        }

        // then just propogate the status from the right-val to the left-val
        if(isa<SENodeAsm>(eachNode) || isa<SENodeCall>(eachNode) || \
            isa<SENodeUnhandledAsm>(eachNode) || isa<SENodeUnhandledCall>(eachNode)){
            //errs()<<*(eachNode->getVal())<<'\n';
            processCall(eachNode);
        }else if(SENodeGEP * gepNode = dyn_cast<SENodeGEP>(eachNode)){
            processGEP(gepNode);
        }else if(SENodePhi* phiNode = dyn_cast<SENodePhi>(eachNode)){
            processPhi(phiNode);
        }else if(SENodeLoad * loadNode = dyn_cast<SENodeLoad>(eachNode)){
            processLoad(loadNode);
        }else if(SENodeCast* castNode = dyn_cast<SENodeCast>(eachNode)){
            processCast(castNode);
        }
            // if it's calcInstruction, then note it as a computation node
        else if(SENodeCalc* calcNode = dyn_cast<SENodeCalc>(eachNode)){
            processCalc(calcNode);
        }
        else if(SENodeStore* storeNode = dyn_cast<SENodeStore>(eachNode)){
            processStore(storeNode);
        }
        else if(isa<SENodeLocal>(eachNode)){
            // it must not be found
            assert(cache.find(eachNode) == cache.end());
            insert_cache(eachNode,-1,F_Alloc);
        }
        else if(isa<SENodeFCmp>(eachNode) || isa<SENodeICmp>(eachNode)){
            processCmp(eachNode);
        }
        else if(SENodeInsertVal * insert_val_node = dyn_cast<SENodeInsertVal>(eachNode)){
            processInsertVal(insert_val_node);
        }
        else if(SENodeExtVal* ext_val_node =  dyn_cast<SENodeExtVal>(eachNode)){
            auto deps = ext_val_node->deps;
            assert(deps.size() == 1);
            SENode* depNode = *(deps.begin());
            Value* dep_inst = depNode->getVal();
            Value* ext_inst = ext_val_node->getVal();
            if(SENodeAsm* dep_asm = dyn_cast<SENodeAsm>(depNode)){
                InlineAsm* target = cast<InlineAsm>(dep_asm->getCastedVal()->getCalledValue());
                string called_str = target->getAsmString();
                assert(called_str == "call __get_user_${4:P}");
            }
            processExtVal(ext_val_node);
        }
        else if(SENodeSpecialCall* seed_node = dyn_cast<SENodeSpecialCall>(eachNode)){
            processSeedNode(seed_node);
        }
        else if(SENodeBranch* brch_node =  dyn_cast<SENodeBranch>(eachNode)){
            processBrchNode(brch_node);
            continue;
        }
        else if(SENodeSelect* select_node = dyn_cast<SENodeSelect>(eachNode)){
            processSelectNode(select_node);
        }
        else if(isa<SENodeUndef>(eachNode)){
            //undef node should never appear
            llvm_unreachable("Undef should be here\n");
        }
        else if(isa<SENodeUnknown>(eachNode)){
            //this trace is affected by unhandled instruction,
            // e.g.  function call instructions whose parameters are more than 4
            // simply return false
            return false;
        }
        else{
            errs() <<"unhandled "<<*(eachNode->getVal())<<'\n';
            displayCache();
            eachNode->getGraph()->displayForDeps();
            llvm_unreachable("what?");
        }
        if(aliased.find(eachNode) != aliased.end()){
            if(cache.find(eachNode) != cache.end())
                insert_cache(aliased.at(eachNode),cache.at(eachNode));
        }
    }
    //errs()<<"[dbg] end of a trace\n";
    // for dbg purpose
    //displayCache();
    //renderAST(formula);
    //__asm__("nop");
    return true;
}
