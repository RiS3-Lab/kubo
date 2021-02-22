#include "Project.h"

string Helper::getValueName(Value *v) {
  if(!v->hasName()){
    return to_string(reinterpret_cast<uintptr_t>(v));
  } else {
    return v->getName().str();
  }
}

string Helper::getValueType(Value *v) {
  if(Instruction *inst = dyn_cast<Instruction>(v)){
    return string(inst->getOpcodeName());
  } else {
    return string("value " + to_string(v->getValueID()));
  }
}

string Helper::getValueRepr(Value *v) {
  string str;
  raw_string_ostream stm(str);

  v->print(stm);
  stm.flush();

  return str;
}

string Helper::getCtypeRepr(Type *t) {
  string str;
  raw_string_ostream stm(str);

  t->print(stm);
  stm.flush();

  return str;
}

string Helper::getExprType(Z3_context ctxt, Z3_ast ast) {
  return string(Z3_sort_to_string(ctxt, Z3_get_sort(ctxt, ast)));
}

string Helper::getExprRepr(Z3_context ctxt, Z3_ast ast) {
  return string(Z3_ast_to_string(ctxt, ast));
}

void Helper::convertDotInName(string &name) {
  std::replace(name.begin(), name.end(), '.', '_');
}

void Dumper::valueName(Value *val) {
  errs() << Helper::getValueName(val) << "\n";
}

void Dumper::typedValue(Value *val) {
  errs() 
    << "[" << Helper::getValueType(val) << "]"
    << Helper::getValueRepr(val)
    << "\n";
}

void Dumper::ctypeValue(Value *val) {
  errs()
    << "[" << Helper::getCtypeRepr(val->getType()) << "]"
    << Helper::getValueRepr(val)
    << "\n";
}

void Dumper::typedExpr(Z3_context ctxt, Z3_ast ast) {
  errs() 
    << "[" << Helper::getExprType(ctxt, ast) << "]"
    << Helper::getExprRepr(ctxt, ast)
    << "\n";
}
FileWriter::FileWriter(string filename) :filename(filename){
    out = new ofstream(filename,ios::out);
}
FileWriter::~FileWriter(){ out->close(); delete out;}
// write helper
void FileWriter::writeLine(int64_t i) { *out<<i<<"\n"; }
void FileWriter::writeLine(string s) { *out<<s; }

FileReader::FileReader(string filename) :filename(filename){
    infile = new ifstream(filename,ios::in);
    assert(infile->is_open());
}

FileReader::~FileReader(){
    infile->close();
    delete infile;
}



bool FileReader::readLine(string * str){
    if(!infile->eof()){
        getline(*infile,*str);
        return true;
    }else{
        return false;
    }
}

bool FileReader::ok(){
    return infile->is_open();
}

bool isSanitizerBlock(BasicBlock &BB) {
    return BB.getTerminator()->getMetadata("kubo_edge_san") != NULL;
}

CallInst *getSanHandlerAsSeed(BranchInst *bi) {
    int succ = -1;
    CallInst * ret = nullptr;
    if (starts_with(bi->getSuccessor(0)->getName(), "handler.")) {
        succ = 0;
    }
    else if (starts_with(bi->getSuccessor(1)->getName(), "handler.")) {
        succ = 1;
    }else{
        errs() << "WARN: can not find sanitizer branch\n";
        return ret;
    }
    BasicBlock* handlerBB = bi->getSuccessor(succ);
    //errs()<<"handlerBB:"<<handlerBB->getName().str()<<'\n';
    for(auto i = handlerBB->begin();i != handlerBB->end();i++){
        if(CallInst* callInst = dyn_cast<CallInst>(&*i)){
            Function * calleeF = callInst->getCalledFunction();
            if(calleeF== nullptr)
                continue;
            if(starts_with(calleeF->getName().str(),"__ubsan_handle") ||\
               starts_with(calleeF->getName().str(),"handle_")||\
               starts_with(calleeF->getName().str(),"ubsan_")){
                ret = callInst;
                return ret;
            }
        }
    }
    if(ret == nullptr){
        errs()<<*handlerBB<<"\n ub handler BB doesn't have a ub handler";
        //llvm_unreachable("ub handler BB basic block doesn't have a handler\n");
    }
    return ret;
}

string get_constant_subclass(Constant* con){
    if(isa<BlockAddress>(con)){
        return "BlockAddress";
    }else if(isa<ConstantArray>(con)){
        return "ConstantArray";
    }else if(isa<ConstantStruct>(con)){
        return "ConstantStruct";
    }else if(isa<ConstantVector>(con)){
        return "ConstantVector";
    }else if(isa<ConstantAggregateZero>(con)){
        return "ConstantAggregateZero";
    }else if(isa<ConstantDataArray>(con)){
        return "ConstantDataArray";
    }else if(isa<ConstantDataVector>(con)){
        return "ConstantDataVector";
    }else if(isa<ConstantFP>(con)){
        return "ConstantFP";
    }else if(isa<ConstantInt>(con)){
        return "ConstantInt";
    }else if(isa<ConstantPointerNull>(con)){
        return "ConstantPointerNull";
    }else if(isa<ConstantTokenNone>(con)){
        return "ConstantTokenNone";
    }else if(isa<UndefValue>(con)){
        return "UndefValue";
    //}else if(isa<PoisonValue>(con)){
    //    return "PoisonValue";
    }else if(isa<ConstantExpr>(con)){
        return "ConstantExpr";
    }else if(isa<GlobalValue>(con)){
        return "GlobalValue";
    }else{
        llvm_unreachable("unhandled constant subclass");
    }
}