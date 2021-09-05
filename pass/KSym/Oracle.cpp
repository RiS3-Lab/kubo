#include "Project.h"
// reachability
void FuncOracle::getReachBlocks(BasicBlock *cur, set<BasicBlock *> &blks) {
  if(blks.find(cur) != blks.end()){
    return;
  }
  blks.insert(cur);

  pred_iterator pi = pred_begin(cur), pe = pred_end(cur);
  for(; pi != pe; ++pi){
    getReachBlocks(*pi, blks);
  }
}

// dominance
LLVMSliceBlock *SliceOracle::getIDom(LLVMSliceBlock *bb) {
  SliceDomTreeNode *node = dt.getNode(bb);
  assert(node != nullptr);

  SliceDomTreeNode *idom = node->getIDom();
  if(idom == nullptr){
    return nullptr;
  }

  return idom->getBlock();
}

// loop info
LLVMSliceLoop *SliceOracle::getOuterLoopInScope(LLVMSliceLoop *scope, 
    LLVMSliceBlock *bb) {

  LLVMSliceLoop *l = li.getLoopFor(bb);
  LLVMSliceLoop *c = nullptr;

  while(l != scope){
    c = l;
    l = l->getParentLoop();
  }

  return c;
}
static inline void ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
        return !std::isspace(ch);
    }));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
    ltrim(s);
    rtrim(s);
}

void ModuleOracle::insert_user_para(Function*func,int arg_idx,string arg_name){
    if(user_args.find(func) == user_args.end()){
        set<pair<int,string> > toInsert;
        toInsert.insert(make_pair(arg_idx,arg_name));
        user_args[func] = toInsert;
    }else{
        user_args[func].insert(make_pair(arg_idx,arg_name));
    }
}
bool ModuleOracle::parseUserInputArg(string& path){
    FileReader fr(path);
    string eachLine;
    string funcStarter("func:");

    //funcMap must be available
    assert(funcMap.size() != 0);
    while(fr.readLine(&eachLine)){
        //starts with "func:"
        if(eachLine.find(funcStarter) == 0){
            rtrim(eachLine);
            string funcName = eachLine.substr(5);
            // read the __user argument
            assert(fr.readLine(&eachLine));
            rtrim(eachLine);
            int seperator = eachLine.find('|');
            assert(seperator != string::npos);
            int idx = stoi(eachLine.substr(0,seperator));
            string argName;
            if(seperator == eachLine.length())
                argName = "";
            else
                argName = eachLine.substr(seperator+1);
            
            //find func
            auto funcIt = func_str_map.find(funcName);
            if(funcIt == func_str_map.end()){
                continue;
            }
            Function* func = funcIt->second;
            insert_user_para(func,idx,argName);
        }
    }
}

set<int> ModuleOracle::getUserSpaceArgs(Function* func)
{
    //__user annotated
    set<int> ret;
    if(user_args.find(func) != user_args.end()){
        auto funcArg= user_args.at(func);
        // for each annotated parameter
        for(auto eachArg = funcArg.begin() ; eachArg != funcArg.end(); eachArg ++){
            int index =  eachArg->first;
            ret.insert(index);
        }
    }
    // syscall
    // then insert all arguments
    if(issyscall(func)){
        for(auto eachFuncArg = func->args().begin(); eachFuncArg != func->args().end(); eachFuncArg++){
            ret.insert(eachFuncArg->getArgNo());
        }
    }
    return ret;
}

bool ModuleOracle::issyscall(Function* f){
    if(syscallIoctls.find(f) != syscallIoctls.end())
        return true;
    else
        return false;
}


vector<string> splitStr(string& s, string delim){
    auto start = 0U;
    auto end = s.find(delim);
    vector<string> ret;
    while (end != string::npos)
    {
        ret.push_back(s.substr(start, end - start));
        start = end + delim.length();
        end = s.find(delim, start);
    }
    ret.push_back(s.substr(start, end));
    return ret;
}
void ModuleOracle::processCallerUnit(std::vector<std::string> &processUnit){
    //sanity check
    assert(processUnit[0].find('F') == 0);

    std::string calledFuncName;
    int scopeIndicator = processUnit[0].rfind("/");
    if( scopeIndicator == std::string::npos){
        //no scope name
        calledFuncName = processUnit[0].substr(4);
        trim(calledFuncName);
    }else{
        // this function has scope name
        std::vector<std::string> splitted = splitStr(processUnit[0],"/");
        bool found = false;
        for(int idx = (splitted.size() - 1); idx >= 0 ; idx--){
            if(!isdigit(splitted[idx][0])){
                calledFuncName = splitted[idx];
                rtrim(calledFuncName);
                found = true;
                break;
            }
        }
        assert(calledFuncName[0] != 'F' && found);
    }
    if(CallersTxtMap.find(calledFuncName) == CallersTxtMap.end()){
        CallersTxtMap.insert(std::make_pair(calledFuncName,std::set<std::pair<std::string,std::string> >()));
    }
    // process each indirect call
    for(unsigned idx = 1; idx < processUnit.size();idx++){
        int left_bracket = processUnit[idx].find('(');
        int seperator = processUnit[idx].find(')');
        assert(left_bracket != std::string::npos && seperator != std::string::npos);
        std::string idc_func = processUnit[idx].substr(left_bracket+1,seperator-(left_bracket+1));
        int scopeIndicator = idc_func.rfind("/");
        if(scopeIndicator == std::string::npos){
            //no scope name, do nothing
        }else{
            std::vector<std::string> splitted = splitStr(idc_func,"/");
            bool found = false;
            for(int idx = (splitted.size() - 1); idx >= 0 ; idx--){
                if(!isdigit(splitted[idx][0])){
                    idc_func = splitted[idx];
                    found = true;
                    break;
                }
            }
            //there won't be a function name that only has one charator, right?
            assert(idc_func[1] != '/' && found);
        }
        // indirect call itself
        string idc = processUnit[idx].substr(seperator+1);
        trim(idc);
        //insert
        CallersTxtMap[calledFuncName].insert(make_pair(idc_func,idc));
    }
}

/*
void ModuleOracle::processCalleeUnit(vector<string>& calleeUnit){
    //sanity checks
    assert(calleeUnit.size() == 2);
    assert(calleeUnit[0].find("CS") == 0);
    assert(calleeUnit[1].find("Callees") == 0);
    string callsiteFuncName;
    string callsiteInst;

    //callsite inst and its function
    int left_bracket = calleeUnit[0].find('(');
    int right_bracket = calleeUnit[0].find(')');
    callsiteFuncName = calleeUnit[0].substr(left_bracket + 1, right_bracket-(left_bracket+1));
    callsiteInst = calleeUnit[0].substr(right_bracket+1);
    trim(callsiteInst);
    assert(left_bracket != string::npos && right_bracket != string::npos);
    pair<string,string> callsite = make_pair(callsiteFuncName,callsiteInst);
    if(CalleesTxtMap.find(callsite) == CalleesTxtMap.end()){
        CalleesTxtMap[callsite] = set<string>();
    }
    // process each calleedump_callees
    string pure_callees = calleeUnit[1].substr(9);//excluding the space
    vector<string> callees = splitStr(calleeUnit[1],"::");
    assert(callees.size() > 0);
    if(callees[0].find("!!EMPTY") == string::npos)
        return;
    for(auto calleeIter = callees.begin();calleeIter != callees.end() ; calleeIter++){
        //insert
        CalleesTxtMap[callsite].insert(*calleeIter);
    }
}*/

void ModuleOracle::processCalleeUnit_new(vector<string>& calleeUnit){
    //sanity checks
    assert(calleeUnit.size() == 2);
    assert(calleeUnit[0].find("CS") == 0);
    assert(calleeUnit[1].find("Callees") == 0);
    string callsite_func_id;
    string callsite_block_id;
    string callsite_inst_id;

    //callsite inst and its function
    size_t left_bracket = calleeUnit[0].find('(');
    size_t right_bracket = calleeUnit[0].find(')');
    assert(left_bracket != string::npos && right_bracket != string::npos);
    callsite_func_id = calleeUnit[0].substr(left_bracket + 1, right_bracket-(left_bracket+1));

    left_bracket = calleeUnit[0].find('(',right_bracket);
    assert(left_bracket != string::npos);
    size_t comma = calleeUnit[0].find(',',left_bracket);
    assert(comma != string::npos);
    right_bracket = calleeUnit[0].find(')',comma);

    callsite_block_id = calleeUnit[0].substr(left_bracket+1,comma-(left_bracket+1));
    callsite_inst_id = calleeUnit[0].substr(comma+1,right_bracket-(comma+1));


    assert(left_bracket != string::npos && right_bracket != string::npos);
    tuple<int,int,int> callsite = make_tuple(stoi(callsite_func_id),stoi(callsite_block_id),stoi(callsite_inst_id));

    assert(CalleesTxtMap.find(callsite) == CalleesTxtMap.end());
    CalleesTxtMap[callsite] = set<int>();

    // process each calleedump_callees
    string pure_callees = calleeUnit[1].substr(9);//excluding the space
    vector<string> callees = splitStr(calleeUnit[1],"::");
    assert(callees.size() > 0);
    if(callees[0].find("!!EMPTY") == string::npos)
        return;
    for(auto calleeIter = callees.begin();calleeIter != callees.end() ; calleeIter++){
        //insert
        CalleesTxtMap[callsite].insert(stoi(*calleeIter));
    }
}

bool ModuleOracle::parseCallGraph(string cg_path){
    FileReader fr(cg_path);
    string eachLine;
    string CalleeStarter("[dumpCallees]");
    string CallerStarter("[dumpCallers]");
    string callsiteStarter("CS:");
    string calleesStarter("Callees:");

    vector<string> calleeUnit;
    while(fr.readLine((&eachLine))){
        rtrim(eachLine);
        if(eachLine.find(callsiteStarter) == 0){
            if(calleeUnit.size() > 0){
                processCalleeUnit_new(calleeUnit);
                calleeUnit.clear();
            }
            calleeUnit.push_back(eachLine);
        }
        else if(eachLine.find(calleesStarter) == 0)
            calleeUnit.push_back(eachLine);
        else if(eachLine.find(CallerStarter) == 0)
            // now comes to the caller session, break
            break;
    }
    if(CalleesTxtMap.size()>0){
        assert(calleeUnit.size() > 0);
        processCalleeUnit_new(calleeUnit);
    }

    /*
    //process caller map
    string callerFuncStarter("F : ");
    string calleeInstStarter("\t");
    vector<string> processUnit;
    while(fr.readLine(&eachLine)){
        rtrim(eachLine);
        if(eachLine.find(callerFuncStarter) == 0){
            if(processUnit.size() > 0){
                // new caller func, process the old one first
                // parse the processUnit and write to CallersTxtMap
                processCallerUnit(processUnit);
                processUnit.clear();
            }
            processUnit.push_back(eachLine);
        }
        else if(eachLine.find(calleeInstStarter) == 0)
            processUnit.push_back(eachLine);
    }
    if(CallersTxtMap.size()>0){
        //deal with the remaining text that's not processed into CallerTxtMap yet
        assert(processUnit.size() > 0);
        processCallerUnit(processUnit);
    }
     */

    // this is the most heavy part
    renderCallGraph();

    // free the space that won't be used later
    for(auto callerIt = CallersTxtMap.begin(); callerIt != CallersTxtMap.end(); callerIt ++){
        callerIt->second.clear();
    }
    CallersTxtMap.clear();
    for(auto calleeIt =  CalleesTxtMap.begin(); calleeIt != CalleesTxtMap.end(); calleeIt ++){
        calleeIt->second.clear();
    }
    CalleesTxtMap.clear();
    return true;
}

pair<formalize_kind,struct SysctlInfo*> ModuleOracle::issysctl(GlobalValue* glb_value,Function* cur_func){
    struct SysctlInfo* sysctl = nullptr;
    StringRef sec_opt = cur_func->getSection();
    for(auto each_sysctl =  sysctls.begin(); each_sysctl != sysctls.end(); each_sysctl++){
        if(glb_value == each_sysctl->val){
            sysctl = &*each_sysctl;
            if (sec_opt == ".text.init"){
                return make_pair(F_Const,sysctl);
            }else{
                return make_pair(F_User,sysctl);
            }
        }
    }
    return make_pair(F_Glb,sysctl);
}


int64_t syscal_vals_to_int(Value* v){
    if(ConstantExpr* const_expr = dyn_cast<ConstantExpr>(v)){
        Instruction* inst = const_expr->getAsInstruction();
        if(GetElementPtrInst* gep_inst = dyn_cast<GetElementPtrInst>(inst)){
            Value* from = gep_inst->getOperand(0);
            Value* glb_val = from->stripPointerCasts();

            if(! glb_val->hasName())
                return 0;
            string con_name = glb_val->getName().str();
            if(con_name == "sysctl_vals"){
                int64_t offset = cast<ConstantInt>(gep_inst->getOperand(1))->getSExtValue();
                if(offset == 4){
                    //systal_one
                    return 1;
                }else if(offset == 8){
                    //sysctl_int_max
                    return INT_MAX;
                }
            }
        }else if(BitCastInst* bit_cast_con = dyn_cast<BitCastInst>(inst)){
            Value* from = bit_cast_con->getOperand(0);
            Value* glb_val = from->stripPointerCasts();
            dbgs()<<*glb_val<<'\n';
            if(! glb_val->hasName())
                return 0;
            string con_name = glb_val->getName().str();
            if( GlobalVariable* casted_glb_val = dyn_cast<GlobalVariable>(glb_val)){
                if(casted_glb_val->hasInitializer()){
                    Constant * glb_con = casted_glb_val->getInitializer();
                    errs()<<*glb_con<<'\n';
                    errs()<<get_constant_subclass(glb_con)<<'\n';
                    if(ConstantInt * glb_con_int = dyn_cast<ConstantInt>(glb_con)){
                        int64_t initializer =  glb_con_int->getSExtValue();
                        return initializer;
                    }else if(ConstantDataArray * glb_con_array = dyn_cast<ConstantDataArray>(glb_con)){
                        int num_ops = glb_con_array->getNumElements();
                        uint64_t array_elem = glb_con_array->getElementAsInteger(0);
                        return (int64_t)array_elem;
                    }else if(ConstantDataVector * glb_con_vec = dyn_cast<ConstantDataVector>(glb_con)){
                        int num_ops = glb_con_vec->getNumElements();
                        uint64_t  vec_elem = glb_con_vec->getElementAsInteger(0);
                        return (int64_t)vec_elem;
                    }
                    else{
                        return 0;
                    }
                }else{
                    return 0;
                }
            }
            else{
                return 0;
            }
        }
    }else if (isa<ConstantPointerNull>(v)){
        return -1;
    }else{
        dbgs()<<*v<<'\n';
        llvm_unreachable("unhandled extra");
    }
}

tuple<GlobalVariable* ,bool ,int64_t> parseSysctlVariableInfo(Value* kernel_variable){
    Value* user_glb = kernel_variable->stripPointerCasts();
    if (GlobalVariable* glb_var = dyn_cast<GlobalVariable>(user_glb)){
        bool has_init = glb_var->hasInitializer();
        int64_t initializer_int = 0;
        if(has_init){
            ConstantInt * initializer = dyn_cast<ConstantInt>(glb_var->getInitializer());
            if(initializer == nullptr){
                has_init = false;
            }else{
                initializer_int = initializer->getSExtValue();
            }
        }
        return make_tuple(glb_var,has_init, initializer_int);
    }else{
        ConstantExpr* expr_const = dyn_cast<ConstantExpr>(user_glb);
        Instruction * con_expr_inst = expr_const->getAsInstruction();
        if(GetElementPtrInst* cast_con_inst = dyn_cast<GetElementPtrInst>(con_expr_inst)){
            Value * gep_src = con_expr_inst->getOperand(0)->stripPointerCasts();
            Value * gep_off = con_expr_inst->getOperand(1)->stripPointerCasts();
            int64_t con_off = cast<ConstantInt>(gep_off)->getSExtValue();
            if(GlobalVariable* glb_var_cast = dyn_cast<GlobalVariable>(gep_src) ){
                bool has_init = glb_var_cast->hasInitializer();
                int64_t initializer_int = 0;
                if(has_init){
                    ConstantInt * initializer = dyn_cast<ConstantInt>(glb_var_cast->getInitializer());
                    if(initializer == nullptr){
                        has_init = false;
                    }else{
                        initializer_int = initializer->getSExtValue();
                    }
                }
                return make_tuple(glb_var_cast,has_init, initializer_int);
            }else{
                return make_tuple(nullptr,false,0);
            }
        }
        else if(isa<IntToPtrInst >(con_expr_inst)){
            // cast a constant int to a pointer
            // not useful for sysctl
        }
        else{
            errs()<<*con_expr_inst<<'\n';
            llvm_unreachable("unhandled handling of kernel sysctl variable.");
        }
    }
}

tuple<int,int,int> ModuleOracle::getSeedIndex(CallInst* call_inst){
    Function* cur_func = call_inst->getFunction();
    return seed_map.at(cur_func).at(call_inst);
}
void ModuleOracle::constructSeedMap(){
    int func_id = 0;
    int blk_id = 0;
    int inst_id = 0;
    for(auto func_it = mo.begin(); func_it != mo.end(); func_it ++, func_id++ ){
        Function * cur_func = &(*func_it);
        //collect all the seeds within this function
        set<CallInst*> seeds;
        for(auto blk_it = func_it->begin(); blk_it != func_it->end(); blk_it++){
            BasicBlock & cur_blk = *blk_it;
            if(isSanitizerBlock(cur_blk)){
                BranchInst *binst = cast<BranchInst>(cur_blk.getTerminator());
                CallInst* seed = getSanHandlerAsSeed(binst);
                if(seed != nullptr)
                    seeds.insert(seed);
            }
        }

        blk_id = 0;
        for(auto blk_it = func_it->begin(); blk_it != func_it->end(); blk_it++,blk_id++) {
            BasicBlock &cur_blk = *blk_it;
            inst_id = 0;
            for (auto inst_it = blk_it->begin(); inst_it != blk_it->end(); inst_it++, inst_id++) {
                Instruction *cur_inst = &*inst_it;
                CallInst *cal_inst = dyn_cast<CallInst>(cur_inst);
                if (cal_inst == nullptr) {
                    continue;
                }
                if(seeds.find(cal_inst) == seeds.end()){
                    continue;
                }
                // now this seed has been located
                if(seed_map.find(cur_func) ==  seed_map.end()){
                    map<CallInst*,tuple<int,int,int> > indexes;
                    seed_map.insert(make_pair(cur_func, indexes));
                }
                seed_map.at(cur_func).insert(make_pair(cal_inst,make_tuple(func_id,blk_id,inst_id) ));
            }
        }
    }
}

void ModuleOracle::parseSysctl(){
    Module::GlobalListType &currGlobalList = mo.getGlobalList();
    for(Module::global_iterator gstart = currGlobalList.begin(), gend = currGlobalList.end(); gstart != gend; gstart++) {
        GlobalVariable *currGlobal = &(*gstart);
        string sysctl_st("struct.ctl_table");
        Type* targetType = currGlobal->getType();
        string typeStr;
        raw_string_ostream rso(typeStr);
        targetType->print(rso);
        //must have struct.ctl
        if(rso.str().find(sysctl_st) == string::npos){
            continue;
        }

        PointerType* ptrTy = dyn_cast<PointerType>(targetType);
        assert(ptrTy);

        if(!currGlobal->hasInitializer()){
            //errs() <<"no initializer for: "<< * currGlobal<<'\n';
            continue;
        }
        Constant *initializer = currGlobal->getInitializer();
        assert(initializer);

        ConstantArray* array = dyn_cast<ConstantArray>(initializer);
        if(!array){
            continue;
        }
        ArrayType* arrayTy = array->getType();
        int numElement = arrayTy->getArrayNumElements();

        for(int i = 0 ; i < numElement;i++){
            Value * v = array->getOperand(i);
            ConstantStruct* gv_struct = dyn_cast<ConstantStruct>(v);
            if(!gv_struct){
                //errs()<<"must be zero-initializor:"<<*v<<'\n';
                continue;
            }
            Value* kernel_variable = gv_struct->getOperand(1);
            Value* handler = gv_struct->getOperand(5);
            Value* extra1 = gv_struct->getOperand(7);
            Value* extra2 = gv_struct->getOperand(8);

            if(!isa<ConstantPointerNull>(kernel_variable)  && !isa<ConstantPointerNull>(handler)){
                string handler_name = handler->getName().str();

                if(handler_name == "proc_dointvec"){
                    tuple<GlobalVariable*,bool,int64_t> sysctl_variable = parseSysctlVariableInfo(kernel_variable);
                    if(get<0>(sysctl_variable) == nullptr){
                        continue;
                    }
                    struct SysctlInfo sysctl_info(get<0>(sysctl_variable),get<1>(sysctl_variable),get<2>(sysctl_variable),false,false,0,0);
                    sysctls.push_back(sysctl_info);
                }
                else if(handler_name == "proc_dointvec_minmax"){
                    bool has_low_bound = true, has_up_bound = true;
                    int64_t ext1_val = syscal_vals_to_int(extra1);
                    if(ext1_val == -1){
                        has_low_bound = false;
                    }
                    int64_t ext2_val = syscal_vals_to_int(extra2);
                    if(ext2_val == -1){
                        has_up_bound = false;
                    }
                    tuple<GlobalVariable*,bool,int64_t> sysctl_variable = parseSysctlVariableInfo(kernel_variable);
                    if(get<0>(sysctl_variable) == nullptr){
                        continue;
                    }
                    struct SysctlInfo sysctl_info(get<0>(sysctl_variable),get<1>(sysctl_variable),get<2>(sysctl_variable),has_low_bound,has_up_bound,ext1_val,ext2_val);
                    sysctls.push_back(sysctl_info);
                }
            }
        }
    }
    __asm__("nop");
}

void ModuleOracle::updateCallersAndCallees(Function * f, CallInst* ci){
    if(CallersMap.find(f) == CallersMap.end()){
        CallersMap[f] = CallInstSet();
        CallersMap[f].insert(ci);
    }else{
        CallersMap[f].insert(ci);
    }
    //if(CalleesMap.find(ci) == CalleesMap.end()){
    //    CalleesMap[ci] = FunctionSet();
    //    CalleesMap[ci].insert(f);
    //}else{
    //    CalleesMap[ci].insert(f);
    //}
}


Instruction* ModuleOracle::getInstByIndex(Function * f ,int t_block_id, int t_inst_id){
    int func_id = 0;
    int block_id = 0;
    int inst_id = 0;
    for (auto each_block_it = f->begin();
         each_block_it != f->end(); block_id++, each_block_it++) {
        if(block_id != t_block_id)
            continue;
        inst_id = 0;
        for (auto each_inst_it = each_block_it->begin();
             each_inst_it != each_block_it->end(); inst_id++, each_inst_it++) {
            if(inst_id != t_inst_id)
                continue;
            else
                return &*each_inst_it;
        }
    }
    return nullptr;
}

//for the call graph that has been parsed
// transfer them into real function and instructions within this module
bool ModuleOracle::renderCallGraph(){
    int func_id = 0;
    //render direct call
    for(auto F = mo.begin();F != mo.end();func_id++,F++){
        Function* f = &*F;
        std::string fName = f->getName().str();
        funcMap[func_id] = f;
        func_str_map[f->getName().str()] = f;
        for(auto B = F->begin();B != F->end(); B++){
            for(auto I = B->begin(); I != B->end();I++){
                Instruction * i = &*I;
                if(CallInst * ci = dyn_cast<CallInst>(i)){
                    Value* v = ci->getCalledOperand();
                    if(v->getName().str().find("llvm.") != std::string::npos || v->getName().str().find("__ubsan_handle") != std::string::npos)
                        continue;
                    if(Function * f1 = ci->getCalledFunction()){
                        updateCallersAndCallees(f1,ci);
                    }
                }
            }
        }
    }
    //render indirect call from callgraph analysis
    for(auto txtCI = CalleesTxtMap.begin();txtCI != CalleesTxtMap.end();txtCI++) {
        int cs_func_id = get<0>(txtCI->first);
        int cs_block_id = get<1>(txtCI->first);
        int cs_inst_id = get<2>(txtCI->first);
        Instruction* inst = getInstByIndex(funcMap.at(cs_func_id),cs_block_id,cs_inst_id);
        CallInst* ci = dyn_cast<CallInst>(inst);
        if(ci == nullptr){
            errs()<<*inst<<'\n';
            llvm_unreachable("uncompitable call inst");
        }
        for(auto eachCallee = txtCI->second.begin(); eachCallee != txtCI->second.end() ; eachCallee++){
            int t_func_id = *eachCallee;
            updateCallersAndCallees(funcMap.at(t_func_id),ci);
        }
    }
    __asm__("nop");
}



void ModuleOracle::parseTaintSummary(string taint_path, bool partial){
    FileReader fr(taint_path);
    string funcStart("Func:");
    string funcEnd("end of summary");
    string callinstStart("CallInst:");
    string callParaIdx("callee idx:");
    string sourceStarter("sources:");
    string timeOutLine("did not finish");

    string eachLine;
    FunctionTaintSummary* currFuncTaintSum;
    map<string,CallInst*> curFuncCIMap;

    Function* cur_func = nullptr;
    CallInst* curCI;
    int curCalleeParaIdx;

    fr.readLine(&eachLine);
    while(true){
        if(eachLine.find(funcStart) == 0){

            string func_str = eachLine.substr(funcStart.size());
            rtrim(func_str);
            int cur_func_id = stoi(func_str);
            cur_func = funcMap.at(cur_func_id);
            currFuncTaintSum = new FunctionTaintSummary(cur_func);
        }
        else if(eachLine.find(callinstStart) == 0){
            string callInstStr = eachLine.substr(callinstStart.size());
            trim(callInstStr);
            size_t left_bracket = callInstStr.find('(');
            size_t comma = callInstStr.find(',');
            size_t right_bracket = callInstStr.find(')');
            assert(left_bracket != string::npos && comma != string::npos && right_bracket != string::npos);
            string blk_id_str = callInstStr.substr(left_bracket+1, comma-(left_bracket+1));
            string inst_id_str = callInstStr.substr(comma + 1, right_bracket - (comma + 1));
            assert(cur_func != nullptr);
            Instruction * cur_inst = getInstByIndex(cur_func,stoi(blk_id_str),stoi(inst_id_str));
            curCI = cast<CallInst>(cur_inst);
        }
        else if(eachLine.find("callee idx:") == 0){
            string calleeParaIdxStr = eachLine.substr(callParaIdx.size());
            rtrim(calleeParaIdxStr);
            //stoi can handle negative number
            curCalleeParaIdx = stoi(calleeParaIdxStr);
        }
        else if(eachLine.find(funcEnd) == 0){
            moduleTaintSummary.push_back(currFuncTaintSum);
            currFuncTaintSum = nullptr;
            curFuncCIMap.clear();
            cur_func = nullptr;
            curCI = nullptr;
            curCalleeParaIdx = -1;
        }
        else if(eachLine.find(sourceStarter) == 0){
            while(true){
                fr.readLine(&eachLine);
                int colonIdx = eachLine.find(':');
                if(colonIdx == string::npos)
                    break;
                string sourceIdxStr = eachLine.substr(0,colonIdx);
                if(sourceIdxStr[0] == '-' || isdigit(sourceIdxStr[0])){
                    int sourceIdx = stoi(sourceIdxStr);
                    //since taint analysis is inaccurate anyway, so no need to record the actually taint source
                    // just the index should be OK
                    if(curCI == nullptr && partial)
                        continue;
                    currFuncTaintSum->insertTaintMap(curCI,curCalleeParaIdx,sourceIdx);
                }else{
                    break;
                }
            }
            // do not go to read another line
            // the current line is not processed yet
            continue;
        }
        else if(eachLine == "" || eachLine.find(timeOutLine) != string::npos){

        }
        else{
            llvm_unreachable("Parse taint summary failed\n");
        }
        bool remain = fr.readLine(&eachLine);
        if(!remain)
            break;
    }
    //clear the unnecessary memory
    for(auto eachFunc = callsiteMap.begin(); eachFunc != callsiteMap.end(); eachFunc++){
        eachFunc->second.clear();
    }
    callsiteMap.clear();
}

static const vector<string> _builtin_syscall_prefix ={
        "compat_SyS_",
        "compat_sys_",
        "SyS_",
        "sys_",
        "__x64_sys",
        "__x32_compat_sys_",
        "__ia32_sys_",
        "__ia32_compat_sys_"
    };
void ModuleOracle::parseSyscallIoctl(string filename){
    FileReader fr(filename);
    string eachLine;
    assert(func_str_map.size() !=0 );
    while(fr.readLine(&eachLine)){
        if(eachLine == "")
            continue;
        vector<string> splitted = splitStr(eachLine,":");
        assert(splitted.size() == 2);
        string funcname = splitted[1];
        if(isdigit(splitted[0][0]) || splitted[0] == "DEVSHOW"){
            continue;
        }
        //get func pointer
        Function * func = nullptr;
        map<string,Function*>::iterator f_it = func_str_map.find(funcname);
        if(f_it != func_str_map.end()){
            func = f_it->second;
        }
        if(splitted[0] == "IOCTL" && func != nullptr){
            insert_user_para(func,func->arg_size() - 1, "");
        }else if( (splitted[0] == "FileRead" || splitted[0] == "FileWrite") && func != nullptr){
            insert_user_para(func,1, "");
            insert_user_para(func,2, "");
        }
        else{
            for (auto each_sys_prefix : _builtin_syscall_prefix){
                if(funcname.rfind(each_sys_prefix) == 0){
                    syscallIoctls.insert(f_it->second);
                    break;
                }
            }
        }
    }

    //funcMap will not be used anymore
    funcMap.clear();
    func_str_map.clear();
}
