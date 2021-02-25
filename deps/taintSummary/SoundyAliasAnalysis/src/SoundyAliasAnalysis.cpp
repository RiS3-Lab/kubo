#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Analysis/CFGPrinter.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/CallGraph.h>

#include "AliasAnalysisVisitor.h"
#include "PointsToUtils.h"
#include "ModuleState.h"
#include "KernelFunctionChecker.h"
#include "TaintAnalysisVisitor.h"
#include "GlobalVisitor.h"
#include "RangeAnalysis.h"
#include "bug_detectors/BugDetectorDriver.h"
extern "C" {
  #include "SCC.h"
}

#include <FunctionChecker.h>
#include <CFGUtils.h>
#include <AliasFuncHandlerCallback.h>
#include <TaintUtils.h>
#include <chrono>
#include <ctime>
#include <future>
#include <AliasObject.h>
#include <iostream>
using namespace llvm;



namespace DRCHECKER {

//#define DEBUG_SCC_GRAPH
//#define DEBUG_TRAVERSAL_ORDER
//#define DEBUG_GLOBAL_VARIABLES

#define NETDEV_IOCTL "NETDEVIOCTL"
#define READ_HDR "FileRead"
#define WRITE_HDR "FileWrite"
#define IOCTL_HDR "IOCTL"
#define DEVATTR_SHOW "DEVSHOW"
#define DEVATTR_STORE "DEVSTORE"
#define V4L2_IOCTL_FUNC "V4IOCTL"

    typedef std::set<CallInst*> CallInstSet;
    typedef std::set<Function*> FunctionSet;
    typedef std::map<CallInst*,FunctionSet> CS2Callees;
    typedef std::map<Function*,CallInstSet> Func2Callers;

    std::map<Value *, std::set<PointerPointsTo*>*> GlobalState::globalVariables;
    std::map<Function *, std::set<BasicBlock*>*> GlobalState::loopExitBlocks;

    FileWriter * fw;
    FunctionHandlerCallback* AliasAnalysisVisitor::callback = new AliasFuncHandlerCallback();
    FunctionHandler* AliasAnalysisVisitor::functionHandler = new FunctionHandler(new KernelFunctionChecker());
    FunctionChecker* TaintAnalysisVisitor::functionChecker = nullptr;

    //static cl::opt<std::string> entryFile("entryFile",cl::Required,
    //                                          cl::desc("txt file that containts call graph"),
    //                                          cl::value_desc("full path to the call graph"), cl::init(""));

    static cl::opt<std::string> callgraphFile("callgraphFile",cl::Required,
                                              cl::desc("txt file that containts call graph"),
                                              cl::value_desc("full path to the call graph"), cl::init(""));

    //static cl::opt<std::string> targetFile("targetFile",cl::Required,
    //                                       cl::desc("txt file that has the target functions and its parameters"),
    //                                       cl::value_desc("full path to the targetfile"), cl::init(""));


    static cl::opt<std::string> outputFile("outputFile",cl::Required,
                                            cl::desc("Path to the output file, where all the taint information should be stored."),
                                            cl::value_desc("Path of the output file."), cl::init(""));

    class FileReader{
    public:
        FileReader(std::string fname) :filename(fname){
            infile = new std::ifstream(filename,std::ios::in);
            //assert(infile->is_open());
        }
        ~FileReader(){ infile->close();delete infile;}
        bool readLine(std::string * str){
            if(!infile->eof()){
                getline(*infile,*str);
                return true;
            }else{
                return false;
            }
        }
        bool ok(){
            return infile->is_open();
        }
    private:
        std::ifstream  *infile;
        std::error_code EC;
        std::string filename;
    };



    struct SAAPass: public ModulePass {
    public:
        static char ID;
        //GlobalState moduleState;

        //maps function name to its caller and the function where the caller reside
        //func ->(caller's function, callsite)
        std::map<std::string,std::set<std::pair<std::string,std::string> > > CallersTxtMap;
        //maps indirect call to its called functions set
        std::map<std::tuple<int,int,int>,std::set<int> > CalleesTxtMap;

        std::set<size_t> CallChain;
        CS2Callees CalleesMap;
        //Func2Callers CallersMap;
        FunctionChecker *currFuncChecker;

        std::vector<Function*> targetFuncs;
        SAAPass() : ModulePass(ID) {
            currFuncChecker = new KernelFunctionChecker();
        }

        ~SAAPass() {
            //delete(currFuncChecker);
        }



        void getAllInterestingInitFunctions(Module &m, std::string currFuncName,std::set<Function*> interestingInitFuncs) {
            /*
             * Get all init functions that should be analyzed to analyze provided init function.
             */
            Module::GlobalListType &currGlobalList = m.getGlobalList();
            std::set<llvm::GlobalVariable*> currFuncGlobals;
            bool is_used_in_main;
            std::set<Function*> currFuncs;
            for(Module::global_iterator gstart = currGlobalList.begin(), gend = currGlobalList.end();
                    gstart != gend; gstart++) {
                llvm::GlobalVariable *currGlob = &*gstart;
                currFuncs.clear();
                is_used_in_main = false;
                for(auto cu:currGlob->users()) {
                    Instruction *currInst = dyn_cast<Instruction>(cu);
                    if(currInst != nullptr && currInst->getParent() && currInst->getParent()->getParent()) {
                        Function *currFunc = currInst->getParent()->getParent();
                        if(currFunc->hasName()) {
                            if(currFunc->getName() == currFuncName) {
                                is_used_in_main = true;
#ifdef DEBUG_GLOBAL_VARIABLES
                                dbgs() << "Global variable:" << *gstart << " used in function:" << currFuncName << "\n";
#endif
                            } else {
                                if(currFuncChecker->is_init_function(currFunc)) {
                                    currFuncs.insert(currFunc);
                                }
                            }
                        }
                    }
                }
                if(is_used_in_main && currFuncs.size()) {
                    for(auto cg:currFuncs) {
                        if(interestingInitFuncs.find(cg) != interestingInitFuncs.end()) {
                            interestingInitFuncs.insert(cg);
                        }
                    }
                }
            }
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
        std::vector<std::string> splitStr(std::string& s, std::string& delim){
            auto start = 0U;
            auto end = s.find(delim);
            std::vector<std::string> ret;
            while (end != std::string::npos)
            {
                ret.push_back(s.substr(start, end - start));
                start = end + delim.length();
                end = s.find(delim, start);
            }
            ret.push_back(s.substr(start, end));
            return ret;
        }
        void processCallerUnit(std::vector<std::string>& processUnit){
            //sanity check
            assert(processUnit[0].find('F') == 0);

            std::string calledFuncName;
            size_t scopeIndicator = processUnit[0].rfind("/");
            if( scopeIndicator == std::string::npos){
                //no scope name
                calledFuncName = processUnit[0].substr(4);
                trim(calledFuncName);
            }else{
                // this function has scope name
                std::string deli("/");
                std::vector<std::string> splitted = splitStr(processUnit[0],deli);
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
                size_t left_bracket = processUnit[idx].find('(');
                size_t seperator = processUnit[idx].find(')');
                assert(left_bracket != std::string::npos && seperator != std::string::npos);
                std::string idc_func = processUnit[idx].substr(left_bracket+1,seperator-(left_bracket+1));
                size_t scopeIndicator = idc_func.rfind("/");
                if(scopeIndicator == std::string::npos){
                    //no scope name, do nothing
                }else{
                    std::string deli("/");
                    std::vector<std::string> splitted = splitStr(idc_func,deli);
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
                std::string idc = processUnit[idx].substr(seperator+1);
                trim(idc);
                //insert
                CallersTxtMap.at(calledFuncName).insert(make_pair(idc_func,idc));
            }
        }
        void processCalleeUnit(std::vector<std::string>& calleeUnit){
                //sanity checks
            assert(calleeUnit.size() == 2);
            assert(calleeUnit[0].find("CS") == 0);
            assert(calleeUnit[1].find("Callees") == 0);
            std::string callsite_func_id;
            std::string callsite_block_id;
            std::string callsite_inst_id;

            //callsite inst and its function
            size_t left_bracket = calleeUnit[0].find('(');
            size_t right_bracket = calleeUnit[0].find(')');
            assert(left_bracket != std::string::npos && right_bracket != std::string::npos);
            callsite_func_id = calleeUnit[0].substr(left_bracket + 1, right_bracket-(left_bracket+1));

            left_bracket = calleeUnit[0].find('(',right_bracket);
            assert(left_bracket != std::string::npos);
            size_t comma = calleeUnit[0].find(',',left_bracket);
            assert(comma != std::string::npos);
            right_bracket = calleeUnit[0].find(')',comma);

            callsite_block_id = calleeUnit[0].substr(left_bracket+1,comma-(left_bracket+1));
            callsite_inst_id = calleeUnit[0].substr(comma+1,right_bracket-(comma+1));


            assert(left_bracket != std::string::npos && right_bracket != std::string::npos);
            std::tuple<int,int,int> callsite = std::make_tuple(std::stoi(callsite_func_id),std::stoi(callsite_block_id),std::stoi(callsite_inst_id));

            assert(CalleesTxtMap.find(callsite) == CalleesTxtMap.end());
            CalleesTxtMap[callsite] = std::set<int>();

            // process each calleedump_callees
            std::string pure_callees = calleeUnit[1].substr(9);//excluding the space
            std::string callee_sep("::");
            std::vector<std::string> callees = splitStr(calleeUnit[1],callee_sep);
            assert(callees.size() > 0);
            if(callees[0].find("!!EMPTY") == std::string::npos)
                return;
            for(auto calleeIter = callees.begin();calleeIter != callees.end() ; calleeIter++){
                //insert
                CalleesTxtMap[callsite].insert(stoi(*calleeIter));
            }
        }

        Instruction* getInstByIndex(Module* m,int t_func_id,int t_block_id, int t_inst_id){
            int func_id = 0;
            int block_id = 0;
            int inst_id = 0;
            for(auto each_func_it = m->begin(); each_func_it != m->end();func_id++, each_func_it++) {
                if(func_id != t_func_id)
                    continue;
                block_id = 0;
                for (auto each_block_it = each_func_it->begin();
                     each_block_it != each_func_it->end(); block_id++, each_block_it++) {
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
            }
            return nullptr;
        }

        bool parseCallGraph(std::string path,Module*m){
            FileReader fr(path);
            std::string eachLine;
            std::string CalleeStarter("[dumpCallees]");
            std::string CallerStarter("[dumpCallers]");
            std::string callsiteStarter("CS:");
            std::string calleesStarter("Callees:");
            std::vector<std::string> calleeUnit;
            while(fr.readLine((&eachLine))){
                rtrim(eachLine);
                if(eachLine.find(callsiteStarter) == 0){
                    if(calleeUnit.size() > 0){
                        processCalleeUnit(calleeUnit);
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
                processCalleeUnit(calleeUnit);
            }
            renderCallGraph(m);
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
        void updateCallersAndCallees(Function * f, CallInst* ci){
            //if(CallersMap.find(f) == CallersMap.end()){
            //    CallersMap[f] = CallInstSet();
            //    CallersMap[f].insert(ci);
            //}else{
            //    CallersMap[f].insert(ci);
            //}
            if(CalleesMap.find(ci) == CalleesMap.end()){
                CalleesMap[ci] = FunctionSet();
                CalleesMap[ci].insert(f);
            }else{
                CalleesMap[ci].insert(f);
            }
        }
        //for the call graph that has been parsed
        // transfer them into real function and instructions within this module
        void renderCallGraph(Module *m){
            std::map<int,Function*> func_id_map;
            int func_id = 0;
            //render direct call
            for(auto F = m->begin();F!=m->end();F++){
                Function* f = &*F;
                std::string fName = f->getName().str();
                func_id_map[func_id] = f;
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
            for(auto txtCI = CalleesTxtMap.begin();txtCI != CalleesTxtMap.end();txtCI++) {
                int cs_func_id = std::get<0>(txtCI->first);
                int cs_block_id = std::get<1>(txtCI->first);
                int cs_inst_id = std::get<2>(txtCI->first);
                Instruction* inst = getInstByIndex(m,cs_func_id,cs_block_id,cs_inst_id);
                CallInst* ci = cast<CallInst>(inst);
                for(auto eachCallee = txtCI->second.begin(); eachCallee != txtCI->second.end() ; eachCallee++){
                    int t_func_id = *eachCallee;
                    updateCallersAndCallees(func_id_map.at(t_func_id),ci);
                }
            }
        }
        /*
        std::map<std::string,std::string> renderEntryFunc(Module*m) {
            std::vector<std::string> realFuncs;
            for (auto f = m->begin(); f != m->end();f++){
                if(!(f->isDeclaration()))
                   realFuncs.emplace_back((*f).getName().str());
            }
            FileReader fr(entryFile.c_str());
            std::string eachLine;
            std::map<std::string,std::string> ret;
            while (fr.readLine((&eachLine))) {
                rtrim(eachLine);
                if (eachLine == "")
                    continue;
                std::vector<std::string> splittedStr = splitStr(eachLine,":");
                assert(splittedStr.size() == 2);
                if(find(realFuncs.begin(),realFuncs.end(),splittedStr[1]) != realFuncs.end())
                    ret[splittedStr[1]] = splittedStr[0];
            }
            return ret;
        }*/
        /*
        void parseTarget(std::string targetFile,Module *m){
            std::map<std::string,Function*> funcMap;
            for(auto F = m->begin();F!=m->end();F++) {
                Function *f = &*F;
                std::string fName = f->getName().str();
                trim(fName);
                funcMap[fName] = f;
            }
            FileReader fr(targetFile);
            std::string buffer;
            while(fr.readLine(&buffer)){
                if(buffer == "")
                    continue;
                assert(buffer.find("func:") == 0);
                std::string funcName = buffer.substr(5);
                assert(funcMap.find(funcName) != funcMap.end());
                Function* tar = funcMap[funcName];
                if(find(targetFuncs.begin(),targetFuncs.end(),tar) == targetFuncs.end())
                    targetFuncs.push_back(tar);
                //skip the paras and indexes
                fr.readLine(&buffer);
            }
        }*/
        /*
        void processOneCallSite(std::string buffer,std::string & callSite, std::string & funcName){
            int seperator = buffer.rfind("|");
            assert(seperator != std::string::npos);
            funcName = buffer.substr(seperator+1);
            rtrim(funcName);
            int comma_pos = buffer.find(":");
            assert(comma_pos != std::string::npos);
            callSite = buffer.substr(comma_pos+1,seperator-(comma_pos+1));
        }*/

        /*Function * locateFunc(std::string funcName, std::map<std::string,Function*> * funcMap){
            std::map<std::string,Function*>::iterator it= funcMap->find(funcName);
            assert(it != funcMap->end());
            return it->second;
        }
        Instruction * locateInst(std::string instName, Function * f,std::map<std::string,std::map<std::string,CallInst*> >* callSiteMap){
            if(callSiteMap != nullptr){
                std::map<std::string,std::map<std::string,CallInst*> >::iterator funcIt = callSiteMap->find(f->getName().str());
                assert(funcIt != callSiteMap->end());
                trim(instName);
                std::map<std::string,CallInst*>::iterator callsiteIt = funcIt->second.find(instName);
                assert(callsiteIt != funcIt->second.end());
                return callsiteIt->second;
            }else{
                Instruction* entry_inst = f->getEntryBlock().getFirstNonPHIOrDbg();
                //std::string firstInstName;
                //raw_string_ostream rso(firstInstName);
                //sentry_inst->print(rso);
                //assert(instName == firstInstName);
                return entry_inst;
            }
            return nullptr;
        }*/
        std::vector<CallInst *> collectDependentCalls(Instruction * inst){
            std::vector<Value *> worklist;
            std::vector<CallInst *> ret;
            int numOperand = inst->getNumOperands();
            for(int i = 0 ; i < numOperand ; i ++){
                worklist.push_back(inst->getOperand(i));
            }
            while(worklist.size() > 0){
                Value * v = worklist.back();
                worklist.pop_back();
                if(isa<ConstantData>(v) || isa<ConstantAggregate>(v) || isa<Argument>(v)|| isa<GlobalVariable>(v) || isa<AllocaInst>(v) ||isa<Function>(v) || isa<BasicBlock>(v)) {
                    // if this value is constant or argument or global or Alloca
                    // it means that we should not trance anymore
                    continue;
                }
                else if(CallInst *  callInst = dyn_cast<CallInst>(v)){
                    ret.push_back(callInst);
                    if(ret.size() >= 3)
                        return ret;
                }else if(BinaryOperator *i_bin = dyn_cast<BinaryOperator>(v)) {
                    worklist.push_back(i_bin->getOperand(0));
                    worklist.push_back(i_bin->getOperand(1));
                }else if(GetElementPtrInst *i_gep = dyn_cast<GetElementPtrInst>(v)) {
                    worklist.push_back(i_gep->getPointerOperand());
                }else if(LoadInst *i_load = dyn_cast<LoadInst>(v)){
                    worklist.push_back(i_load->getPointerOperand());
                }else if(StoreInst *i_store = dyn_cast<StoreInst>(v)){
                    worklist.push_back(i_store->getValueOperand());
                    worklist.push_back(i_store->getPointerOperand());
                }else if(CastInst *i_cast = dyn_cast<CastInst>(v)){
                    worklist.push_back(i_cast->getOperand(0));
                }
            }
            return ret;
        }

#define NUM_CALLEES_PER_DEPENDET_CALLSITE 3

        void printCallStack(std::vector<std::pair<Instruction *,Function*> > callstack){
            for( auto eachcall : callstack){
                dbgs()<<(*eachcall.first)<<"|"<<eachcall.second->getName().str()<<'\n';
            }
        }
        Function * locateFunc(int offset, std::map<int,Function*> *funcCache,Module* m){
            Function* ret = nullptr;
            //look from the cache
            if(funcCache->find(offset) != funcCache->end()){
                return (*funcCache)[offset];
            }
            else{
                // no cache
                int off = 0;
                //locate the function
                for(auto fIt = m->begin(); fIt != m->end();fIt++,off++){
                    if(off == offset){
                        ret = &(*fIt);
                        break;
                    }
                }
                assert(ret != nullptr);
                (*funcCache)[offset] = ret;
                return ret;
            }
        }
        Instruction * locateInstruction(int offsetBB,int offsetInst,Function * f){
            int bbOff = 0;
            int instOff = 0;
            for(auto bbIt = f->begin(); bbIt != f->end(); bbIt++,bbOff++){
                for(auto instIt = bbIt->begin() ; instIt != bbIt->end();instIt++,instOff++){
                    if(offsetBB == bbOff){
                        if(instOff == offsetInst)
                            return &(*instIt);
                    }
                    __asm__("nop");
                }
            }
            return nullptr;
        }
        void processACallChain(std::vector<std::string > buffer,
                std::map<int,Function*> *funcCache,std::map<std::tuple<int,int,int>,Instruction*> *instCache,
                Module * m){
            //transform each line into <callInst, func>
            std::string entryLine = buffer.front();

            int funcOff;
            int instFuncOff, instBBOff, instInstOff;
            int placeHolder;
            std::sscanf(entryLine.c_str(),"CS%d:%d,%d,%d|%d,",&placeHolder,&instFuncOff,&instBBOff,&instInstOff,&funcOff);
            Function * func = nullptr;
            Instruction * inst = nullptr;

            func = locateFunc(funcOff,funcCache,m);

            std::tuple<int,int,int> instTuple = std::make_tuple(instFuncOff,instBBOff,instInstOff);
            if(instCache->find(instTuple) != instCache->end()){
                inst = (*instCache)[instTuple];
            }
            else{
                Function * instFunc = locateFunc(instFuncOff,funcCache,m);
                inst = locateInstruction(instBBOff,instInstOff,instFunc);
                assert(inst!= nullptr);
                (*instCache)[instTuple] = inst;
            }
            std::vector<std::pair<Instruction *,Function*> > callStack;
            //starting from the entry func

            callStack.push_back(std::make_pair(inst,func));
            CallChain.insert(hashCallChain(&callStack));
            for(unsigned i = 1; i < buffer.size() ; i++){
                // locate the func and inst
                std::string bufLIne = buffer[i];
                std::sscanf(bufLIne.c_str(),"CS%d:%d,%d,%d|%d,",&placeHolder,&instFuncOff,&instBBOff,&instInstOff,&funcOff);

                func = locateFunc(funcOff,funcCache,m);

                std::tuple<int,int,int> instTuple = std::make_tuple(instFuncOff,instBBOff,instInstOff);
                if(instCache->find(instTuple) != instCache->end()){
                    inst = (*instCache)[instTuple];
                }
                else{
                    Function * instFunc = locateFunc(instFuncOff,funcCache,m);
                    inst = locateInstruction(instBBOff,instInstOff,instFunc);
                    assert(inst!= nullptr);
                    (*instCache)[instTuple] = inst;
                }
                // get dependent call sites for the current call instruction
                std::vector<CallInst*> dependentCallSites = collectDependentCalls(inst);
                for(auto eachDependentCall = dependentCallSites.begin(); eachDependentCall != dependentCallSites.end();eachDependentCall++){
                    std::vector<Function*> callees;
                    if(CalleesMap.find(*eachDependentCall) != CalleesMap.end()){
                        int counter = 0;
                        for(auto calleeIt = CalleesMap[*eachDependentCall].begin(); calleeIt != CalleesMap[*eachDependentCall].end() && counter<NUM_CALLEES_PER_DEPENDET_CALLSITE;calleeIt++,counter++){
                            callees.push_back(*calleeIt);
                        }
                    }
                    for(auto eachCallee = callees.begin(); eachCallee != callees.end() ; eachCallee++){
                        std::vector<std::pair<Instruction *,Function*> > shadowCallStack(callStack);
                        //shadowCallStack.insert(shadowCallStack.end(),callStack.begin(),callStack.end());
                        shadowCallStack.push_back(std::make_pair(*eachDependentCall,*eachCallee));
                        std::size_t hashed = hashCallChain(&shadowCallStack);
                        CallChain.insert(hashed);
                    }
                }
                //push this call site
                callStack.push_back(std::make_pair(inst,func));
                std::size_t hashed = hashCallChain(&callStack);
                CallChain.insert(hashed);
            }
        }
        void parserCallChain(std::string callchainFile, Module* m){
            /*
            std::map<std::string,Function*> funcMap;
            for(auto F = m->begin();F!=m->end();F++) {
                //cache func map
                Function *f = &*F;
                std::string fName = f->getName().str();
                trim(fName);
                funcMap[fName] = f;
            }*/
            FileReader fr(callchainFile);
            std::string buffer;
            std::string starter("CS0");
            std::string ender("end of callchain");
            int callChainCount = 0;
            std::map<int,Function*> funcCache;
            std::map<std::tuple<int,int,int>,Instruction*> instCache;
            auto start = std::chrono::system_clock::now();
            auto end = std::chrono::system_clock::now();
            while(fr.readLine(&buffer)){
                if(buffer=="")
                {
                    //void call chain file
                    return;
                }
                if(callChainCount % 2000 == 0){
                    dbgs() <<"call chain:("<<callChainCount<<"|";
                    end = std::chrono::system_clock::now();
                    std::chrono::duration<double> elapsed_seconds = end-start;
                    dbgs()<<(int)elapsed_seconds.count()<<")";
                    start = end;
                }
                std::string callSiteStr;
                std::string funcNameStr;
                //entry func
                std::vector<std::string > processUnit;
                //fr.readLine(&buffer1);

                //processOneCallSite(buffer,callSiteStr,funcNameStr);
                processUnit.push_back(buffer);
                while(fr.readLine((&buffer))){
                    if(buffer.find(ender) == 0){
                        processACallChain(processUnit,&funcCache,&instCache,m);
                        callChainCount++;
                        break;
                    }else{
                        if(buffer == ""){
                            //unfinished callchain file, just break this callchain
                            break;
                        }
                        //processOneCallSite(buffer,callSiteStr,funcNameStr);
                        processUnit.push_back(buffer);
                    }
                }
            }
        }

        void parseCheckPoint(std::string path,std::vector<std::string>& checkPoint){
            std::string buffer;
            FileReader fr = FileReader(path);
            if(!fr.ok())
                return;
            std::string funcStarter("Func:");
            while(fr.readLine(&buffer))
            {
                if(buffer.find(funcStarter) == 0)
                {
                    std::string fName = buffer.substr(funcStarter.size());
                    rtrim(fName);
                    checkPoint.push_back(fName);
                }
            }
            return;
        }

        struct SCC_ENT ** build_SCC_ent(std::map<Function*,uint32_t> & FuncMap2Int,std::map<Function*,std::set<Function*> >& mergedCalleeMap){
            struct SCC_ENT ** ret = (struct SCC_ENT **)malloc(FuncMap2Int.size() * sizeof(struct SCC_ENT*));
            int ret_idx = 0;
            for(auto mergedIt = mergedCalleeMap.begin(); mergedIt != mergedCalleeMap.end();mergedIt++){
                struct SCC_ENT * ent = (struct SCC_ENT*)malloc(sizeof(struct SCC_ENT));
                Function * caller = mergedIt->first;
                ent->id = FuncMap2Int.at(caller);
                ent->count = mergedIt->second.size();
                ent->successors = (uint32_t*) malloc(ent->count * sizeof(uint32_t));
                int callee_idx = 0;
                for(auto callee_it = mergedIt->second.begin(); callee_it != mergedIt->second.end(); callee_it++){
                    Function* callee = *callee_it;
                    ent->successors[callee_idx++] = FuncMap2Int.at(callee);
                }
                ret[ret_idx++] = ent;
            }
            return ret;
        }
        void free_SCC_ENTS(size_t size,struct SCC_ENT ** ents){
            for(uint32_t i= 0 ; i < size;i++){
                free(ents[i]->successors);
                free(ents[i]);
            }
            free(ents);
        }
        void render_SCC_group(int numEnt,uint32_t ** result,std::vector<std::vector<uint32_t> >& rendered){
            for(int i = 0 ; i < numEnt;i++){
                std::vector<uint32_t> tmp;
                uint32_t* ptr = result[i];
                if(ptr == NULL){
                    return;
                }
                uint32_t numMem = ptr[0];
                for(uint32_t memIdx = 0 ; memIdx < numMem; memIdx++){
                    tmp.push_back(ptr[memIdx+1]);
                }
                rendered.push_back(tmp);
                free(ptr);
            }
            free(result);
        }

        std::vector<Function*> find_ready(std::map<Function*,std::set<Function*> > & mergedCallerMap,std::set<Function*> & visited){
            // found the functions which has not been visited, but its callers have been 
            std::vector<Function*> ret;
            for(auto each_caller_ent_it = mergedCallerMap.begin(); each_caller_ent_it != mergedCallerMap.end(); each_caller_ent_it++){
                if(visited.find(each_caller_ent_it->first) == visited.end()  ){
                    bool ready = true;
                    for(auto each_caller_it = each_caller_ent_it->second.begin(); each_caller_it != each_caller_ent_it->second.end(); each_caller_it++){
                        if(visited.find(*each_caller_it) == visited.end()){
                            ready = false;
                        }
                    }
                    if(ready){
                        ret.push_back(each_caller_ent_it->first);
                    }
                }
            }
            return ret;
        }

        void refine_taint(Function* & f,std::set<Function*>& callers,std::map<Function*,FunctionTaintSummary*>& total_func_summary,std::vector<Function*>& unfinished_funcs){
            std::set<int> tainted_paras;
            
        }
        void whole_kernel_taint_refinement(std::map<Function*,FunctionTaintSummary*>& total_func_summary,std::vector<Function*>& unfinished_funcs){
            std::map<Function*,std::set<Function*> > mergedCalleeMap;
            for(auto calleeMapIt = CalleesMap.begin();calleeMapIt != CalleesMap.end(); calleeMapIt++){
                CallInst* ci = calleeMapIt->first;
                Function* f_caller = ci->getFunction();
                if(mergedCalleeMap.find(f_caller) == mergedCalleeMap.end()){
                    std::set<Function*> tmp;
                    tmp.insert(calleeMapIt->second.begin(),calleeMapIt->second.end());
                    mergedCalleeMap.insert(std::make_pair(f_caller,tmp));
                }else{
                    mergedCalleeMap.at(f_caller).insert(calleeMapIt->second.begin(),calleeMapIt->second.end());
                }
            }
            std::map<Function*,uint32_t> FuncMap2Int;
            uint32_t funcCount = 0;
            for(auto mergedIt = mergedCalleeMap.begin(); mergedIt != mergedCalleeMap.end();mergedIt++){
                if(FuncMap2Int.find(mergedIt->first) == FuncMap2Int.end()){
                    FuncMap2Int.insert(std::make_pair(mergedIt->first,funcCount++));
                }
                for(auto calleeIt = mergedIt->second.begin();calleeIt != mergedIt->second.end();calleeIt++){
                    Function* callee = *calleeIt;
                    if(FuncMap2Int.find(callee) == FuncMap2Int.end()){
                        FuncMap2Int.insert(std::make_pair(callee,funcCount++));
                    }
                }

            }
            struct SCC_ENT ** SCC_ents = build_SCC_ent(FuncMap2Int,mergedCalleeMap);
            std::string scc_out_name = outputFile+".scc";
            FILE* SCC_out = fopen(scc_out_name.c_str(),"w");
            if(SCC_out == NULL){
                printf("cannot open %s error code:%s\n",scc_out_name.c_str(),strerror(errno));
                assert(false);
            }
            uint32_t** SCC_result = (uint32_t**)malloc((int)FuncMap2Int.size() * sizeof(uint32_t*));
            memset(SCC_result,'\0',(int)mergedCalleeMap.size() * sizeof(uint32_t*));
            analyze_scc((int)mergedCalleeMap.size(),SCC_ents,false,SCC_out,SCC_result);
            fclose(SCC_out);
            free_SCC_ENTS((int)mergedCalleeMap.size(),SCC_ents);

            std::vector<std::vector<uint32_t> > SCC_groups;
            render_SCC_group((int)mergedCalleeMap.size(),SCC_result,SCC_groups);

            std::map<Function*,std::set<Function*> > mergedCallerMap;
            for(auto each_callee_ent = mergedCalleeMap.begin(); each_callee_ent != mergedCalleeMap.end();each_callee_ent++){
                // locate the key for caller map first
                Function * caller = each_callee_ent->first;
                for(auto each_callee = each_callee_ent->second.begin();each_callee != each_callee_ent->second.end();each_callee++){
                    Function* callee = *each_callee;
                    if(mergedCallerMap.find(callee) == mergedCallerMap.end()){
                        // this function has not been processed yet;
                        std::set<Function*> callers;
                        callers.insert(caller);
                        auto callee_ent_it = each_callee_ent;
                        for(;callee_ent_it != mergedCalleeMap.end();callee_ent_it++){
                            if(callee_ent_it->second.find(callee) != callee_ent_it->second.end()){
                                callers.insert(callee_ent_it->first);
                            }
                        }
                        mergedCallerMap.insert(std::make_pair(callee,callers));
                    }
                }
            }
            std::set<Function*> visited;
            size_t total_num = mergedCalleeMap.size();
            for(auto each_caller_ent = mergedCallerMap.begin();each_caller_ent != mergedCallerMap.end();each_caller_ent++){
                if(each_caller_ent->second.size() == 0){
                    visited.insert(each_caller_ent->first);
                }
            }
            /*
            while(visited.size() < total_num){
                std::vector<Function*> ready = find_ready(mergedCallerMap,visited);
                if(ready.size() == 0){
                    for(auto each_caller_ent =  mergedCallerMap.begin();each_caller_ent != mergedCallerMap.end();each_caller_ent++){
                        if(visited.find(each_caller_ent->first) == visited.end()){
                            visited.insert(each_caller_ent->first);
                        }
                    }
                }
                for(auto each_ready = ready.begin();each_ready != ready.end();each_ready++){
                    refine_taint(*each_ready,mergedCallerMap.at(*each_ready),total_func_summary,unfinished_funcs);
                }
            }*/
        }

        bool runOnModule(Module &m) override {
            
            // create data layout and other for the current module
            DataLayout *currDataLayout = new DataLayout(&m);
            //std::map<std::string,std::string> toCheckFunctions = renderEntryFunc(&m);
            {
                //so callsiteMap is alive only in this curly bracket
                dbgs()<<"rendering callgraph..";
                parseCallGraph(callgraphFile,&m);
                dbgs()<<"done\n";

                //dbgs()<<"rendering call chain..";
                //parserCallChain(callchainFile,&m);
                //dbgs()<<"done\n";

                //parseTarget(targetFile,&m);
            }
            InterProceduralRA<CropDFS> &range_analysis = getAnalysis<InterProceduralRA<CropDFS> >();
            //InterProceduralRA<CropDFS> * range_analysis = nullptr;
            assert(outputFile != "");

            fw = new FileWriter(outputFile.c_str());

            // Setup aliases for global variables.
            setupGlobals(m);

            std::map<Function*,FunctionTaintSummary*> total_func_summary;
            std::vector<Function*> unfinished_funcs;
            int total_func_num = 0;
            for(auto funcIt = m.begin(); funcIt != m.end();total_func_num++, funcIt++){}
            int func_id = 0;
            for(auto funcIt = m.begin(); funcIt != m.end();func_id++, funcIt++){
                std::string checkFunctionName = (*funcIt).getName().str();
                Function * curFunc = &*funcIt;
                if(curFunc->isDeclaration())
                    continue;
                //errs()<<"start analyzing "<<curFunc->getName().str()<<"\n";
                //std::string tarFunc("des3_ede_decrypt");
                //if(checkFunctionName != tarFunc)
                //    continue;
                //std::string functionType = eachEntryFunc->second;
                //int totalFnCount = 0;
                std::vector<std::pair<Instruction *,Function*> > * callSites = new std::vector<std::pair<Instruction *,Function*> >();
                //dbgs()<<*curFunc<<'\n';
                callSites->push_back(std::make_pair(curFunc->getEntryBlock().getFirstNonPHIOrDbg(),curFunc));
                FunctionChecker* targetChecker = new KernelFunctionChecker();

                GlobalState currState(&range_analysis, currDataLayout);
                // set pointer to global state
                AliasAnalysisVisitor::callback->setPrivateData(&currState);
                // setting function checker(s).
                TaintAnalysisVisitor::functionChecker = targetChecker;
                AliasAnalysisVisitor::callback->targetChecker = targetChecker;

                std::vector<VisitorCallback *> allCallBacks;
                setupFunctionArgs(curFunc, callSites,currState);
                addAllVisitorAnalysis(currState, curFunc,callSites,&allCallBacks);
                std::vector<std::vector<BasicBlock *> *> *traversalOrder = BBTraversalHelper::getSCCTraversalOrder(*curFunc);

                GlobalVisitor *vis = new GlobalVisitor(currState,curFunc, func_id,callSites, traversalOrder, allCallBacks);

                std::future<void> fut = std::async(std::launch::async,&GlobalVisitor::analyze,vis);
                std::chrono::system_clock::time_point one_hundred_seconds = std::chrono::system_clock::now() + std::chrono::seconds(100);
                if(fut.wait_until(one_hundred_seconds) == std::future_status::ready){
                    //vis->summaries->dump(fw);
                    errs()<<curFunc->getName().str()<<" finished\n";
                    total_func_summary.insert(std::make_pair(curFunc,vis->summaries));
                }
                else{
                    errs()<<curFunc->getName().str()<<" didn't finish\n";
                    unfinished_funcs.push_back(curFunc);
                    vis->stop = true;
                    fut.get();
                }
                dbgs()<<func_id<<" out of "<< total_func_num<<'\n';
                //clean up
                delete(vis);
                delete((KernelFunctionChecker*)targetChecker);
            }
            errs()<<"refining\n";
            whole_kernel_taint_refinement(total_func_summary,unfinished_funcs);
            for( auto analysis_it = total_func_summary.begin(); analysis_it != total_func_summary.end();analysis_it++ ){
                analysis_it->second->dump(fw);
            }
            for( auto analysis_it = total_func_summary.begin(); analysis_it != total_func_summary.end();analysis_it++ ){
                delete(analysis_it->second);
            }
            errs()<<"refining finished\n";

            // clean up.
            delete(currDataLayout);
            delete(fw);
            return false;
        }
        void addAllVisitorAnalysis(GlobalState &targetState,
                                   Function *toAnalyze,
                                   std::vector<std::pair<Instruction *,Function*> > *srcCallSites,
                                   std::vector<VisitorCallback *> *allCallbacks) {

            // This function adds all analysis that need to be run by the global visitor.
            // it adds analysis in the correct order, i.e the order in which they need to be
            // run.

            VisitorCallback *currVisCallback = new AliasAnalysisVisitor(targetState, toAnalyze, srcCallSites);

            // first add AliasAnalysis, this is the main analysis needed by everyone.
            allCallbacks->insert(allCallbacks->end(), currVisCallback);

            currVisCallback = new TaintAnalysisVisitor(targetState, toAnalyze, srcCallSites);

            // next add taint analysis.
            allCallbacks->insert(allCallbacks->end(), currVisCallback);
        }

        void getAnalysisUsage(AnalysisUsage &AU) const override {
            AU.setPreservesAll();
            AU.addRequired<InterProceduralRA<CropDFS>>();
            AU.addRequired<CallGraphWrapperPass>();
            AU.addRequired<LoopInfoWrapperPass>();
        }

    private:

        void setupGlobals(Module &m) {
            /*
             * Set up global variables.
             */

            // map that contains global variables to AliasObjects.
            std::map<Value*, AliasObject*> globalObjectCache;
            std::vector<llvm::GlobalVariable*> visitorCache;
            visitorCache.clear();
            // first add global functions.
            for(Module::iterator mi = m.begin(), ei = m.end(); mi != ei; mi++) {
                GlobalState::addGlobalFunction(&(*mi), globalObjectCache);
            }

            Module::GlobalListType &currGlobalList = m.getGlobalList();
            for(Module::global_iterator gstart = currGlobalList.begin(), gend = currGlobalList.end(); gstart != gend; gstart++) {
                // ignore constant immutable global pointers
                if((*gstart).isConstant()) {
                    continue;
                }
                GlobalState::addGlobalVariable(visitorCache, &(*gstart), globalObjectCache);
#ifdef DEBUG_GLOBAL_VARIABLES
                (*gstart).print(dbgs());
                    dbgs() << " NUM USES:" << (*gstart).getNumUses() << ", TYPE:";
                    (*gstart).getType()->print(dbgs());
                    //op1->print(dbgs());
                    dbgs() << "\n";

                dbgs() << "For:";
                dbgs() << (*gstart).getName() << ":";
                dbgs() << " of type (" << (*gstart).getType()->getContainedType(0)->isStructTy() << ","
                       << (*gstart).getType()->isPointerTy() << "," << (*gstart).getType()->isArrayTy() << "):";
                (*gstart).getType()->print(dbgs());
                dbgs() << ":";
                if((*gstart).hasInitializer()) {
                    Constant *initializationConst = (*gstart).getInitializer();
                    initializationConst->getType()->print(dbgs());
                    dbgs() << ", Struct Type:" << initializationConst->getType()->isStructTy();
                    if(initializationConst->getType()->isStructTy() &&
                            !initializationConst->isZeroValue()) {
                        ConstantStruct *constantSt = dyn_cast<ConstantStruct>(initializationConst);
                        dbgs() << " Num fields:" << constantSt->getNumOperands() << "\n";
                        for (int i = 0; i < constantSt->getNumOperands(); i++) {
                            dbgs() << "Operand (" << i + 1 << ") :";
                            Function *couldBeFunc = dyn_cast<Function>(constantSt->getOperand(i));
                            dbgs() << "Is Function:" << (couldBeFunc != nullptr) << "\n";
                            if(!couldBeFunc)
                                constantSt->getOperand(i)->print(dbgs());
                            dbgs() << "\n";
                        }
                    }
                    dbgs() << "\n";
                } else {
                    dbgs() << "No initializer\n";
                }
#endif
                // sanity
                assert(visitorCache.empty());
            }
            globalObjectCache.clear();

            // OK get loop info of all the functions and store them for future use.
            // get all loop exit basic blocks.
            for(Module::iterator mi = m.begin(), ei = m.end(); mi != ei; mi++) {
                Function &currFunction = *mi;
                if(!currFunction.isDeclaration()) {
                    LoopInfoWrapperPass &p = getAnalysis<LoopInfoWrapperPass>(currFunction);
                    LoopInfo &funcLoopInfo = p.getLoopInfo();
                    SmallVector<BasicBlock *, 1000> allExitBBs;
                    allExitBBs.clear();
                    for (auto a:funcLoopInfo) {
                        a->getExitingBlocks(allExitBBs);
                        GlobalState::addLoopExitBlocks(&currFunction, allExitBBs);
                        allExitBBs.clear();
                    }
                }
            }

        }

        void setupFunctionArgs(Function *targetFunction,std::vector<std::pair<Instruction *,Function*> > *callSites, GlobalState &targetState) {
            /*
             * Set up the function args for the main entry function(s).
             */
            targetState.getOrCreateContext(callSites);

            // arguments which are tainted and passed by user
            std::set<unsigned long> taintedArgs;     // What is the difference between taintArgs and taintArgData??????
            // arguments which contain tainted data
            std::set<unsigned long> taintedArgData;
            // arguments which are pointer args
            std::set<unsigned long> pointerArgs;
            bool is_handled = false;
            for(auto argIt = targetFunction->arg_begin(); argIt != targetFunction->arg_end() ; argIt++) {
                Argument* arg = &(*argIt);
                int i = argIt->getArgNo();
                Type * tyArg = arg->getType();
                if(tyArg->isPointerTy()){
                    PointerType * ptrTyArg = dyn_cast<PointerType>(tyArg);
                    if(ptrTyArg->getElementType()->isStructTy()){
                        taintedArgData.insert(i);
                    }else{
                        taintedArgs.insert(i);
                    }
                }
                else{
                    taintedArgs.insert(i);
                }
            }

            int counter = 0;
            for(auto arg_it =  targetFunction->arg_begin(); arg_it != targetFunction->arg_end();arg_it++ ,counter++){
                if ((*arg_it).getType()->isPointerTy())
                    pointerArgs.insert(counter);
            }
            std::map<Value *, std::set<PointerPointsTo*>*> *currPointsTo = targetState.getPointsToInfo(callSites);
            unsigned long arg_no=0;
            for(Function::arg_iterator arg_begin = targetFunction->arg_begin(), arg_end = targetFunction->arg_end(); arg_begin != arg_end; arg_begin++) {
                Value *currArgVal = &(*arg_begin);
                if(taintedArgs.find(arg_no) != taintedArgs.end()) {
                    TaintFlag *currFlag = new TaintFlag(currArgVal, true);
                    currFlag->addToTrace(currArgVal);
                    std::set<TaintFlag*> *currTaintInfo = new std::set<TaintFlag*>();
                    currTaintInfo->insert(currFlag);
                    TaintUtils::updateTaintInfo(targetState, callSites, currArgVal, currTaintInfo);
                }
                if(pointerArgs.find(arg_no) != pointerArgs.end()) {
                    PointerPointsTo *newPointsTo = new PointerPointsTo();
                    AliasObject *obj = new FunctionArgument(currArgVal, currArgVal->getType(), targetFunction,
                                                            callSites);
                    newPointsTo->targetPointer = currArgVal;
                    newPointsTo->fieldId = 0;
                    newPointsTo->dstfieldId = 0;
                    newPointsTo->targetObject = obj;
                    if(taintedArgData.find(arg_no) != taintedArgData.end()) {
                        TaintFlag *currFlag = new TaintFlag(currArgVal, true);
                        // what's the different between this and above??
                        currFlag->addToTrace(currArgVal);
                        obj->taintAllFields(currFlag);
                    }
                    std::set<PointerPointsTo *> *newPointsToSet = new std::set<PointerPointsTo *>();
                    newPointsToSet->insert(newPointsToSet->end(), newPointsTo);
                    (*currPointsTo)[currArgVal] = newPointsToSet;
                } else {
                    assert(taintedArgData.find(arg_no) == taintedArgData.end());
                }
                arg_no++;
            }
        }
    };

    char SAAPass::ID = 0;
    static RegisterPass<SAAPass> x("dr_checker", "Soundy Driver Checker", false, true);
}

