//
// Created by machiry on 12/4/16.
//

#ifndef PROJECT_VISITORCALLBACK_H
#define PROJECT_VISITORCALLBACK_H

#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/Analysis/AliasSetTracker.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CFG.h"
#include <set>

using namespace llvm;

namespace DRCHECKER {

    /*
     *  All the flow analysis techniques which wish to use the
     *  global visitor should implement this call back.
     */
    class FileWriter {
    public:
        FileWriter(std::string filename):filename(filename){
            out = new std::ofstream(filename,std::ios::out | std::ios::app);
            assert(out->is_open());
        }
        ~FileWriter() {delete out;}

        // write helper
        void writeLine(int64_t i) { *out<<i<<"\n"; }
        void writeLine(std::string s) { *out<<s; }
        void flush(){
            //make sure the the data were flushed into the disk on time
            out->close();
            delete out;
            out = new std::ofstream(filename,std::ios::out | std::ios::app);
        }

    private:
        std::ofstream  *out;
        std::error_code EC;
        std::string filename;
    };

    class FunctionTaintSummary{
        // -1 represent from fetch
        std::map<CallInst*,std::vector<Function*> > callees;
        //in this callInst, map the nth para of this callee to the taint source
        // if taint source is greater or equal to 0, then it's from
        std::map<CallInst*,std::map<int,std::vector<std::pair<int,Value*>> > > taintMapping;
        Function* func;
        int func_id;
    public:
        FunctionTaintSummary(Function*f,int id): func(f),func_id(id){}
        ~FunctionTaintSummary(){
            for(auto callee = callees.begin(); callee != callees.end();callee++){
                callee->second.clear();
                assert(callee->second.empty());
                callee->second.shrink_to_fit();
            }
            callees.clear();
            for(auto eachCallMap = taintMapping.begin();eachCallMap != taintMapping.end();eachCallMap++){
                for(auto eachValueMap = eachCallMap->second.begin();eachValueMap != eachCallMap->second.end(); eachValueMap++ ){
                    eachValueMap->second.clear();
                    assert(eachValueMap->second.empty());
                    eachValueMap->second.shrink_to_fit();
                }
                eachCallMap->second.clear();
            }
            taintMapping.clear();
        }


        void insertCallee(CallInst* ci,Function*callee){
            if(callees.find(ci) == callees.end()){
                std::vector<Function*> vF;
                vF.push_back(callee);
                callees.insert(std::make_pair(ci,vF));
            }
            else{
                if(std::find(callees[ci].begin(),callees[ci].end(),callee) == callees[ci].end())
                    callees[ci].push_back(callee);
            }
        }
        void insertTaintMap(CallInst* ci,int calleeArg,Value* taintSource,int taintArg){
            if(taintMapping.find(ci) == taintMapping.end()){
                std::map<int,std::vector<std::pair<int,Value*>> > paratoSource;
                paratoSource[calleeArg] = std::vector<std::pair<int,Value*>>();
                paratoSource[calleeArg].push_back(std::make_pair(taintArg,taintSource));
                taintMapping.insert(std::make_pair(ci,paratoSource));
            }else{
                if(taintMapping[ci].find(calleeArg) == taintMapping[ci].end()){
                    std::vector<std::pair<int,Value*> > taintSources;
                    taintSources.push_back(std::make_pair(taintArg,taintSource));
                    taintMapping[ci].insert(std::make_pair(calleeArg,taintSources));
                }else{
                    std::pair<int,Value*> taintPair = std::make_pair(taintArg,taintSource);
                    if(std::find(taintMapping[ci][calleeArg].begin(),taintMapping[ci][calleeArg].end(),taintPair) == taintMapping[ci][calleeArg].end())
                        taintMapping[ci][calleeArg].push_back(taintPair);
                }
            }
        }
        std::pair<int,int> get_block_inst_id(Function * f,Instruction* inst){
            int block_id = 0;
            int inst_id = 0;
            BasicBlock* b = inst->getParent();
            for(auto each_block_it = f->begin(); each_block_it != f->end();block_id++, each_block_it ++) {
                inst_id = 0;
                BasicBlock * iter_b = &*each_block_it;
                if(b != iter_b){
                    continue;
                }
                for (auto each_inst_it = each_block_it->begin();
                     each_inst_it != each_block_it->end(); inst_id++, each_inst_it++) {
                    Instruction * iter_inst = &*each_inst_it;
                    if(inst == iter_inst){
                        return std::make_pair(block_id,inst_id);
                    }
                }
            }
            assert(false);
        }

        void dump(FileWriter*fw){
            std::stringstream s;
            s<<"Func:"<<func_id<<'\n';
            fw->writeLine(s.str());
            for(std::map<CallInst*,std::map<int,std::vector<std::pair<int,Value*>> > >::iterator eachCallInst = taintMapping.begin(); eachCallInst != taintMapping.end();eachCallInst++){
                std::pair<int,int> block_inst_id = get_block_inst_id(func,eachCallInst->first);
                std::stringstream s;
                s<<"CallInst:("<<block_inst_id.first<<','<<block_inst_id.second<<")\n";
                fw->writeLine(s.str());
                for(std::map<int,std::vector<std::pair<int,Value*>> >::iterator eachPara = eachCallInst->second.begin();eachPara != eachCallInst->second.end();eachPara++){
                    fw->writeLine("callee idx:"+itostr(eachPara->first)+"\n");
                    fw->writeLine("sources:\n");
                    for(std::vector<std::pair<int,Value*>>::iterator eachSource = eachPara->second.begin(); eachSource != eachPara->second.end(); eachSource++){
                        std::string str;
                        llvm::raw_string_ostream llvm_ss (str);
                        eachSource->second->print(llvm_ss);
                        fw->writeLine(itostr(eachSource->first)+":"+str+"\n");
                    }
                }
            }
            fw->writeLine("end of summary\n");
            fw->flush();
        }
    };

    class VisitorCallback {
    public:

        /*
         *  Function which will be called by the GlobalVisitor
         *  to indicate that the analysis is within loop
         * @param inside_loop true/false which indicates
         *                    that the analysis is within loop.
         */
        virtual void setLoopIndicator(bool inside_loop) {

        }

        virtual void visit(Instruction &I) {
        }

        virtual void visitBinaryOperator(BinaryOperator &I) {

        }

        virtual void visitPHINode(PHINode &I) {

        }

        virtual void visitSelectInst(SelectInst &I) {

        }

        virtual void visitLoadInst(LoadInst &I) {

        }

        virtual void visitStoreInst(StoreInst &I) {

        }

        virtual void visitGetElementPtrInst(GetElementPtrInst &I) {

        }

        virtual void visitAllocaInst(AllocaInst &I) {

        }

        virtual void visitVAArgInst(VAArgInst &I) {

        }

        virtual void visitVACopyInst(VACopyInst &I) {

        }

        virtual void visitCastInst(CastInst &I) {

        }

        virtual void visitICmpInst(ICmpInst &I) {

        }

        virtual void visitBranchInst(BranchInst &I) {

        }

        /*
         *  Visit the call instruction, this function should setup a new CallBack
         *  which will be used the GlobalVisitor to analyze the corresponding function.
         * @param I Call instruction.
         * @param targetFunction Function which is called by the provided call instruction.
         * @param oldFuncCallSites Context of the caller.
         * @param currFuncCallSites Context, basically list of call instructions.
         * @return VisitorCallback which should be used to analyze the targetFunction.
         */
        virtual VisitorCallback* visitCallInst(CallInst &I, Function *targetFunction,
                                               std::vector<std::pair<Instruction *,Function*> > *oldFuncCallSites,
                                               std::vector<std::pair<Instruction *,Function*> > *currFuncCallSites,
                                               std::map<Function*,std::vector<int> > * globalTaintInfo,std::vector<Function*>* targetFuncs,FunctionTaintSummary*) {
            return nullptr;
        }

        /*
         * This function provides the VisitorCallback an opportunity to stitch back the result of function callback
         *  with the original callback.
         * @param I Callinstruction handled by the childCallback
         * @param childCallback Visitor which handled the call instruction (I)
         */
        virtual void stitchChildContext(CallInst &I, VisitorCallback *childCallback) {

        }

        virtual void visitReturnInst(ReturnInst &I) {

        }

        bool* stop;
        void visit(BasicBlock *BB) {

        }
    protected:
        // instructions where the warning has been generated.
        std::set<Instruction *> warnedInstructions;
    };
}

#endif //PROJECT_VISITORCALLBACK_H
