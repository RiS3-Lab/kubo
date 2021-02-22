//
// Created by machiry on 2/1/17.
//

#ifndef PROJECT_TAINTEDLOOPBOUNDDETECTOR_H
#define PROJECT_TAINTEDLOOPBOUNDDETECTOR_H

#include <FunctionChecker.h>
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/Analysis/AliasSetTracker.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/CFG.h"
#include "../VisitorCallback.h"
#include "../ModuleState.h"

using namespace llvm;

namespace DRCHECKER {

    /*
     * This detector detects if a tainted value is used as loop bound.
     * TLBD
     *
     */
    class TaintedLoopBoundDetector: public VisitorCallback {
    public:
        GlobalState &currState;
        Function *targetFunction;
        // context of the analysis, basically list of call sites
        std::vector<std::pair<Instruction *,Function*> > *currFuncCallSites;
        std::set<BasicBlock*> *currFuncExitBlocks;

        TaintedLoopBoundDetector(GlobalState &targetState, Function *toAnalyze,
                                  std::vector<std::pair<Instruction *,Function*> > *srcCallSites,
                                  FunctionChecker *currChecker);

        // only function which we the current checker is interested in.
        virtual void visitBranchInst(BranchInst &I);
        virtual VisitorCallback* visitCallInst(CallInst &I, Function *targetFunction,
                                               std::vector<std::pair<Instruction *,Function*> > *oldFuncCallSites,
                                               std::vector<std::pair<Instruction *,Function*> > *currFuncCallSites,
                                               std::map<Function*,std::vector<int> > * globalTaintInfo,std::vector<Function*>* targetFuncs,FunctionTaintSummary*);
    private:
        std::string TAG;

    };
}

#endif //PROJECT_TAINTEDLOOPBOUNDDETECTOR_H
