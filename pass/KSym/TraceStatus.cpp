//
// Created by lcm on 1/20/2021.
//
#include "Project.h"

enum UB_types seed2UBType(CallInst *ub_handler) {
    string funcName = ub_handler->getCalledFunction()->getName().str();
    assert(!funcName.empty() && \
    (funcName.find("ubsan") != string::npos||funcName.find("handle_overflow") != string::npos) );

    UB_types ret = ub_notype;
    BasicBlock* errBranch = ub_handler->getParent();
    BasicBlock* ubCheckBlock = errBranch->getSinglePredecessor();
    if(ubCheckBlock == nullptr)
    {
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
    if(funcName == "__ubsan_handle_add_overflow" || \
      funcName == "__ubsan_handle_sub_overflow" || \
      funcName == "__ubsan_handle_mul_overflow" || \
      funcName == "handle_overflow")
    {
        Instruction* term = ubCheckBlock->getTerminator();
        ExtractValueInst * conVar = dyn_cast<ExtractValueInst>(term->getOperand(0));
        if(conVar == nullptr){
            errs()<<"Err: UBSAN place is wrong\n";
            return ret;
        }
        InsertValueInst* sanitized_stuct = dyn_cast<InsertValueInst>(conVar->getOperand(0));
        if(sanitized_stuct == nullptr){
            errs()<<"Err:UB not clear\n";
            return ret;
        }
        if(sanitized_stuct->getMetadata("kuboT_uadd")!=NULL )
            ret = uadd;
        else if(sanitized_stuct->getMetadata("kuboT_usub")!=NULL)
            ret = usub;
        else if(sanitized_stuct->getMetadata("kuboT_umul")!=NULL)
            ret = umul;
        else if(sanitized_stuct->getMetadata("kuboT_sadd")!=NULL)
            ret = sadd;
        else if(sanitized_stuct->getMetadata("kuboT_ssub")!=NULL)
            ret = ssub;
        else if(sanitized_stuct->getMetadata("kuboT_smul")!=NULL)
            ret = smul;
        else
            llvm_unreachable("intrisic doesn't have a metadata");
        return ret;
    }
    else if(funcName == "__ubsan_handle_negate_overflow"){
        ret = negate_overflow;
    }
    else if(funcName == "__ubsan_handle_divrem_overflow"){
        // for these UBSANis missing from agg node, no need to collect extra constraints
        ret = div_by_0;
    }else if(funcName == "__ubsan_handle_shift_out_of_bounds"){
        ret = shift_out_of_bound;
    }else if(funcName == "__ubsan_handle_out_of_bounds"){
        ret = array_bounds;
    }
    else if(funcName == "__ubsan_handle_implicit_conversion") {
        ret = ub_notype;
    }else if(funcName=="ubsan_type_mismatch_common" ||
             funcName.find("ubsan_handle_type_mismatch") != string::npos){

    }
    else{
        errs()<<"unhandled ub handler function:"<<funcName<<'\n';
        llvm_unreachable("ub unhandled\n");
    }
    return ret;
}
_TraceStatus::_TraceStatus(_TraceStatus * other){
    traceIncompleteConstraints = false;
    satisfiable = false;
    controledByUser = false;
    postBug = other->postBug;
    seed = other->seed;
    bugType = other->bugType;
    seg = nullptr;
}

_TraceStatus::_TraceStatus(CallInst * ubhandler, Instruction* i){
    traceIncompleteConstraints = false;
    satisfiable = false;
    controledByUser = false;
    postBug = false;
    seed = ubhandler;
    root = i;
    bugType = seed2UBType(seed);
    seg = nullptr;
}


_TraceStatus::~_TraceStatus(){
    if(seg != nullptr){
    //errs()<<"Deleting seg:"<< seg<<'\n';
        delete seg;
        seg = nullptr;
    }
    depArgs.clear();
}


