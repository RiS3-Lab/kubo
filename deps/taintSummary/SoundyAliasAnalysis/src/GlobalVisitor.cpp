//
// Created by machiry on 12/4/16.
//
#include <CFGUtils.h>
#include "PointsToUtils.h"
#include "GlobalVisitor.h"
//#define DEBUG_INSTRUCTION_TRACES
namespace DRCHECKER {

    size_t hashCallChain(std::vector<std::pair<Instruction *,Function*> > *callSites){

        std::hash<std::string> str_hash;
        std::string output;
        for(auto it = callSites->begin();it!= callSites->end() ; it++) {
            std::string sig;
            llvm::raw_string_ostream rso(sig);
            Instruction *i = (*it).first;
            Function *f = (*it).second;
            i->print(rso);

            output = output + rso.str() + f->getName().str();
        }
        return str_hash(output);
    }

//#define DEBUG_GLOBAL_ANALYSIS
//#define DEBUG_CALL_INSTR
#define CARE_COMPLETION
//these number doesn't play any role
#define MAX_CALLSITE_DEPTH 20
#define MAX_FUNC_PTR 5
#define SMART_FUNCTION_PTR_RESOLVING
//#define DEBUG_BB_VISIT
//#define DEBUG_CALL_INSTR

    // Basic visitor functions.
    // call the corresponding function in the child callbacks.
    void GlobalVisitor::visitAllocaInst(AllocaInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit alloc:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitAllocaInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitCastInst(CastInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit cast:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitCastInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitBinaryOperator(BinaryOperator &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit bin:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitBinaryOperator(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitPHINode(PHINode &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit phi:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitPHINode(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitSelectInst(SelectInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit Select:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitSelectInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitGetElementPtrInst(GetElementPtrInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit gep:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitGetElementPtrInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitLoadInst(LoadInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit load:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitLoadInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitStoreInst(StoreInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit store:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitStoreInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitVAArgInst(VAArgInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit va arg:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitVAArgInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitVACopyInst(VACopyInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit va copy:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitVACopyInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitReturnInst(ReturnInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit return:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitReturnInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitICmpInst(ICmpInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit icmp:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitICmpInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void GlobalVisitor::visitBranchInst(BranchInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit branch:" << &I<<"\n";
#endif
        for(VisitorCallback *currCallback:allCallbacks) {
            currCallback->visitBranchInst(I);
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }

    void printCallStack(std::vector<std::pair<Instruction *,Function*> > callstack){
        for( auto eachcall : callstack){
            dbgs()<<(*eachcall.first)<<"|"<<eachcall.second->getName().str()<<'\n';
        }
        dbgs()<<"finish\n";
    }

    void GlobalVisitor::processCalledFunction(CallInst &I, Function *currFunc) {
        std::vector<std::pair<Instruction *,Function*> > * newCallContext = new std::vector<std::pair<Instruction *,Function*> >();
        newCallContext->insert(newCallContext->end(), this->currFuncCallSites->begin(), this->currFuncCallSites->end());
        newCallContext->insert(newCallContext->end(), std::make_pair(&I,currFunc));

        this->currState.getOrCreateContext(newCallContext);

        // new callbacks that handles the current function.
        std::vector<VisitorCallback *> newCallBacks;

        // map of the parent visitor to corresponding child visitor.
        std::map<VisitorCallback *, VisitorCallback *> parentChildCallBacks;

        for(VisitorCallback *currCallback:allCallbacks) {
            VisitorCallback *newCallBack = currCallback->visitCallInst(I, currFunc, this->currFuncCallSites, newCallContext,globalTaintInfo,TargetFuncs,summaries);
            if(newCallBack != nullptr) {
                newCallBacks.insert(newCallBacks.end(), newCallBack);
                parentChildCallBacks[currCallback] = newCallBack;
            }
        }

        for(std::map<VisitorCallback *, VisitorCallback *>::iterator iter = parentChildCallBacks.begin();iter != parentChildCallBacks.end();++iter)
        {
            VisitorCallback *parentCallback = iter->first;
            VisitorCallback *childCallback = iter->second;
            parentCallback->stitchChildContext(I, childCallback);
            delete(childCallback);
        }
#ifdef DEBUG_CALL_INSTR
        dbgs() << "Analyzing new function:" << currFuncName << " Call depth:" << newCallContext.size() << "\n";
#endif
    }

    // Visit Call Instruction.
    void GlobalVisitor::visitCallInst(CallInst &I) {
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"visit callinst:" << &I<<"\n";
#endif
        if(this->inside_loop) {
#ifdef DEBUG_CALL_INSTR
            dbgs() << "Function inside loop, will be analyzed at last iteration\n";
#endif
            return;
        }

        Function *currFunc = I.getCalledFunction();
        if(currFunc == nullptr) {
            // this is to handle casts.
            currFunc = dyn_cast<Function>(I.getCalledValue()->stripPointerCasts());
        }

        // ignore only if the current function is an external function
        if(currFunc == nullptr || !currFunc->isDeclaration()) {
            // check if the call instruction is already processed?
            if (this->visitedCallSites.find(std::make_pair((Instruction*)&I,currFunc) ) != this->visitedCallSites.end()) {
                // this function has already been analyzed
                // just abort
                return;
            }
            if(std::find(this->currFuncCallSites->begin(), this->currFuncCallSites->end(), std::make_pair((Instruction*)&I,currFunc) ) != this->currFuncCallSites->end()) {
                // this function has already been analyzed
                // just abort
                return;
            }
        }
        // insert into visited call sites.
        this->visitedCallSites.insert(this->visitedCallSites.end(), std::make_pair(&I,currFunc)  );



        if(currFunc != nullptr) {
            if(currFunc->getName().str().find("llvm.") != std::string::npos || currFunc->getName().str().find("__ubsan_handle_") != std::string::npos){
                //intrinsic function, not interested
                return;
            }
            this->processCalledFunction(I, currFunc);
        } else {
            // if this is inline assembly, ignore the call instruction.
            if(I.isInlineAsm()) {
                return;
            }
#ifdef DEBUG_CALL_INSTR
            dbgs() << "Visiting Indirect call instruction.";
#endif
            Value *calledValue = I.getCalledValue();

            // get points to information of calledValue and look for only functions.
            std::vector<Function*> targetFunctions;
            targetFunctions.clear();
            bool hasTargets = PointsToUtils::getTargetFunctions(this->currState, this->currFuncCallSites,
                                                                calledValue, targetFunctions);
#ifdef SMART_FUNCTION_PTR_RESOLVING
            if(!hasTargets) {
                hasTargets = PointsToUtils::getPossibleFunctionTargets(I, targetFunctions);
#ifdef DEBUG_CALL_INSTR
                if(targetFunctions.size() > 0) {
                    dbgs() << "Function Pointer targets:" << targetFunctions.size() << "\n";
                }
#endif
                std::vector<Function *> filteredFunctions;
                for(unsigned i=0; i<MAX_FUNC_PTR && i<targetFunctions.size(); i++) {
                    filteredFunctions.push_back(targetFunctions[i]);
                }
#ifdef DEBUG_CALL_INSTR
                if(filteredFunctions.size() != targetFunctions.size()) {
                    dbgs() << "Ignoring Target Functions, Doing:"
                           << filteredFunctions.size()
                           << ", Got:" << targetFunctions.size() << "\n";
                }
#endif
                targetFunctions.clear();
                targetFunctions.insert(targetFunctions.end(), filteredFunctions.begin(), filteredFunctions.end());
            }
#endif
            // get potential target function from a given pointer.
            if(hasTargets) {
                assert(targetFunctions.size() > 0);
#ifdef DEBUG_CALL_INSTR
                dbgs() << "There are:" << targetFunctions.size() << " Target Functions.\n";
#endif
                for(Function *currFunction:targetFunctions) {
                    this->processCalledFunction(I, currFunction);
                }

            } else {

#ifdef DEBUG_CALL_INSTR
                dbgs() << "Function pointer does not point to any functions:";
                calledValue->print(dbgs());
                dbgs() << ", So Ignoring\n";
#endif
            }
        }
#ifdef DEBUG_INSTRUCTION_TRACES
        dbgs()<<"finish\n";
#endif
    }



    void GlobalVisitor::visit(BasicBlock *BB) {
      if(stop)
          return;
      _super->visit(BB->begin(), BB->end());
    }

    void GlobalVisitor::analyze() {
        // the traversal order should not be null
        assert(this->traversalOrder != nullptr);
        for(unsigned int i = 0; i < this->traversalOrder->size(); i++){
            // current strongly connected component.
            std::vector<BasicBlock *> *currSCC = (*(this->traversalOrder))[i];
            if(currSCC->size() == 1) {
                this->inside_loop = false;
                for(VisitorCallback *currCallback:allCallbacks) {
                    currCallback->setLoopIndicator(false);
                }
                //Analyzing single basic block.
                for(unsigned int j=0; j < currSCC->size(); j++) {
                    BasicBlock* currBB = (*currSCC)[j];
                    this->visit(currBB);
                }

            } else {
                //unsigned long opt_num_to_analyze = BBTraversalHelper::getNumTimesToAnalyze(currSCC);
                //if (opt_num_to_analyze > 2)
                //    opt_num_to_analyze = 2;
                unsigned long opt_num_to_analyze = 1;
#ifdef DEBUG_GLOBAL_ANALYSIS
                dbgs() << "Analyzing Loop BBS for:" << opt_num_to_analyze <<" number of times\n";
#endif
                this->inside_loop = true;

                for(VisitorCallback *currCallback:allCallbacks) {
                    currCallback->setLoopIndicator(true);
                }
                //dbgs()<<"inside:"<< currFuncCallSites->back().second->getName().str()<<'\n';
                auto start = std::chrono::system_clock::now();
                auto end = std::chrono::system_clock::now();
                for(unsigned int l=0; l <= (opt_num_to_analyze-1); l++) {
                    //dbgs()<< "current SCC:"<<l<<" out of "<< opt_num_to_analyze<<"|";
                    end = std::chrono::system_clock::now();
                    std::chrono::duration<double> elapsed_seconds = end-start;
                    //dbgs()<<" costed:"<<(int)elapsed_seconds.count()<<"\n";
                    start = end;

                    for (unsigned int j = 0; j < currSCC->size(); j++) {
                        BasicBlock *currBB = (*currSCC)[j];
                        this->visit(currBB);
                    }
                    // ensure that loop has been analyzed minimum number of times.
                    if(l >= (opt_num_to_analyze-1)) {
                        this->inside_loop = false;
                        for(VisitorCallback *currCallback:allCallbacks) {
                            currCallback->setLoopIndicator(false);
                        }
                    }
                }
                end = std::chrono::system_clock::now();
                std::chrono::duration<double> elapsed_seconds = end-start;
                //dbgs()<<"total costed:"<<(int)elapsed_seconds.count()<<"\n";

#ifdef DEBUG_GLOBAL_ANALYSIS
                dbgs() << "Analyzing Loop BBS END\n";
#endif
                //Analyzing loop.
            }
        }
    }
}

