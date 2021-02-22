#ifndef ORACLE_H_
#define ORACLE_H_

#include "Project.h"

enum formalize_kind{
    F_Para,
    F_User,
    F_Glb,
    F_Alloc,
    F_Malloc,
    F_Const,
    F_Call,
    F_Unknown,
    //to annotate the result of computation
    F_Calculated,
    // modified because of cmp
    F_Cmped
};

class SliceOracle {
  public:
    SliceOracle(LLVMSlice &s) : dt(s), li(dt), dag() {
      dag.recalculate(s, this);

      SliceBlock *sb;
      for(LLVMSliceBlock &lb : s){
        sb = lb.getSliceBlock(); 

        LLVMSliceBlock::inst_iterator 
          ii = lb.inst_begin(), ie = lb.inst_end();

        for(; ii != ie; ++ii){
          insts.insert(make_pair(*ii, sb));
        }
      }
    }

    ~SliceOracle() {}

    // mapping
    SliceBlock *getSliceHost(Instruction *inst) {
      auto i = insts.find(inst);
      if(i == insts.end()){
        return nullptr;
      } else {
        return i->second;
      }
    }

    // dominance
    bool dominates(LLVMSliceBlock *dom, LLVMSliceBlock *bb) {
      return dt.dominates(dom, bb);
    }

    LLVMSliceBlock *getIDom(LLVMSliceBlock *bb); 

    // loops
    LLVMSliceLoop *getOuterLoopInScope(LLVMSliceLoop *scope, 
        LLVMSliceBlock *bb);

    LLVMSliceLoop *getInnerLoop(LLVMSliceBlock *bb) {
      return li.getLoopFor(bb);
    }

    LLVMSliceLoop *getOuterLoop(LLVMSliceBlock *bb) {
      return getOuterLoopInScope(nullptr, bb);
    }

    // DAG 
    DAPath *getPath(LLVMSliceBlock *bb) {
      return dag.getPath(bb);
    }

    UnrollPath *getUnrolled(LLVMSliceBlock *bb) {
      // path contains several DATrace,
      // each DATrace is a path from entry to the given bb.
      DAPath *path = getPath(bb);
      assert(path->size() > 0);
      return uc.getUnrolled(path, bb, &dag);
    }

  protected:
    // mapping
    map<Instruction *, SliceBlock *> insts;

    // basics
    SliceDomTree dt;
    SliceLoopInfo li;

    // DAG
    DAGraph dag;
    UnrollCache uc;
};

class FuncOracle {
  public:
    FuncOracle(Function &f, 
               const DataLayout &dl, const TargetLibraryInfo &tli) 
    // ac(f),
    // dt(f),
    // li(dt),
    // caa(dl, tli, ac, dt, li)
    {}

    ~FuncOracle() {}

    // reachability
    void getReachBlocks(BasicBlock *cur, set<BasicBlock *> &blks);

protected:
    // AssumptionCache ac;
    // DominatorTree dt;
    // LoopInfo li;
    // CombinedAA caa;
};
class FunctionTaintSummary{
    // -1 represent from fetch
    //in this callInst, map the nth para of this callee to the taint source
    // if taint source is greater or equal to 0, then it's from
    //map<CallInst*,map<int,vector<pair<int,Value*> > > > taintMapping;
    map< CallInst*, map<int,vector<int> > > taintMapping;
public:
    Function* func;
    FunctionTaintSummary(Function*f): func(f){}
    ~FunctionTaintSummary(){
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

    void insertTaintMap(CallInst* ci,int calleeArg,int taintArg){
        if(taintMapping.find(ci) == taintMapping.end()){
            map<int,vector<int> > paratoSource;
            paratoSource[calleeArg] = vector<int>();
            paratoSource[calleeArg].push_back(taintArg);
            taintMapping.insert(make_pair(ci,paratoSource));
        }else{
            if(taintMapping[ci].find(calleeArg) == taintMapping[ci].end()){
                vector<int > taintSources;
                taintSources.push_back(taintArg);
                taintMapping[ci].insert(make_pair(calleeArg,taintSources));
            }else{
                if(find(taintMapping[ci][calleeArg].begin(),taintMapping[ci][calleeArg].end(),taintArg) == taintMapping[ci][calleeArg].end())
                    taintMapping[ci][calleeArg].push_back(taintArg);
            }
        }
    }
    bool taint_could_satisfied(set<int>* deps,CallInst* callSite){
        if(taintMapping.find(callSite) == taintMapping.end())
            return false;
        map<int,vector<int>>& taintMap = taintMapping.at(callSite);
        for(auto eachDepNeedTaint = deps->begin(); eachDepNeedTaint != deps->end(); eachDepNeedTaint++){
            // cannot sat this dep
            if(taintMap.find(*eachDepNeedTaint) == taintMap.end())
                return false;
        }
        return true;
    }
};

struct SysctlInfo{
    bool has_initializer;
    bool has_lower_bound;
    bool has_upper_bound;
    int64_t initializer;
    int64_t lower_bound;
    int64_t upper_bound;
    GlobalValue* val;
    SysctlInfo(GlobalValue*v, bool has_initializer, int64_t init, bool has_low,bool has_up,int64_t low,int64_t up):
        val(v),has_initializer(has_initializer),initializer(init),has_lower_bound(has_low),
        has_upper_bound(has_up),lower_bound(low),upper_bound(up){}
};

class ModuleOracle {
public:
    //call graph related staff
    typedef set<CallInst*> CallInstSet;
    typedef set<Function*> FunctionSet;
    typedef map<CallInst*,FunctionSet> CS2Callees;
    typedef map<Function*,CallInstSet> Func2Callers;
    ModuleOracle(Module &m, TargetLibraryInfo &tli) :
        mo(m),
        dl(m.getDataLayout()),
        tli(tli),
        triple(Triple(Twine(m.getTargetTriple())))
        {}

    ~ModuleOracle() {
        for(auto eachCallerIt = CallersMap.begin(); eachCallerIt != CallersMap.end();eachCallerIt++){
            eachCallerIt->second.clear();
        }
        CallersMap.clear();
        for(auto eachCalleeIt = CalleesMap.begin(); eachCalleeIt != CalleesMap.end();eachCalleeIt++){
            eachCalleeIt->second.clear();
        }
        CalleesMap.clear();
        for(auto eachTaint = moduleTaintSummary.begin(); eachTaint != moduleTaintSummary.end(); eachTaint++){
            delete(*eachTaint);
        }
        moduleTaintSummary.clear();
        moduleTaintSummary.shrink_to_fit();
    }

    // getter
    Triple &getTriple() {
        return triple; 
    }

    const DataLayout &getDataLayout() {
      return dl;
    }

    const TargetLibraryInfo &getTargetLibraryInfo() {
      return tli;
    }

    // data layout
    uint64_t getBits() {
      return BITS;
    }

    uint64_t getPointerWidth() {
      return dl.getPointerSizeInBits();
    }

    uint64_t getPointerSize() {
      return dl.getPointerSize();
    }

    uint64_t getTypeSize(Type *ty) {
      return dl.getTypeAllocSize(ty);
    }

    uint64_t getTypeWidth(Type *ty) {
      return dl.getTypeSizeInBits(ty);
    }

    uint64_t getTypeOffset(Type *type, unsigned idx) {
      assert(isa<StructType>(type));
      return dl.getStructLayout(cast<StructType>(type))->getElementOffset(idx);
    }

    uint64_t getStructFieldSize(Type *type, unsigned idx) {
        assert(isa<StructType>(type));
        const StructLayout *sl = dl.getStructLayout(cast<StructType>(type));
        unsigned tnum = cast<StructType>(type)->getNumElements();
        uint64_t off = sl->getElementOffset(idx);
        uint64_t noff = (idx == tnum - 1) ?
            getTypeSize(type) : // last element
            getTypeOffset(cast<StructType>(type), idx + 1);
        return noff - off;
    }
    bool parseCallGraph(string cg_file);
    void parseTaintSummary(string filename,bool);
    void parseSyscallIoctl(string filename);
    void parseSysctl();
    void insert_user_para(Function*f,int arg_idx,string arg_name);
    bool parseUserInputArg(string&);

    void constructSeedMap();
    tuple<int,int,int> getSeedIndex(CallInst*);
    set<int> getUserSpaceArgs(Function*);
    bool issyscall(Function*);
    pair<formalize_kind, struct SysctlInfo*> issysctl(GlobalValue*,Function*);

    /* Parse the generated annotation of which argument of which function are provided by user
     * input : path to the annotation file
     * return : bool indicating parsing is successful or not
     * */
    bool isReintPointerType(Type *ty) {
      return ty->isPointerTy() || 
        (ty->isIntegerTy() && ty->getIntegerBitWidth() == getPointerWidth());
    }
    

    CallInstSet* getCaller(Function*f){
        auto callerIt = CallersMap.find(f);
        if(callerIt != CallersMap.end()){
            return &callerIt->second;
        }else
            return nullptr;

    }
    FunctionTaintSummary* getFuncSum(Function* f){
        for(auto eachFuncTaint = moduleTaintSummary.begin(); eachFuncTaint != moduleTaintSummary.end(); eachFuncTaint++){
            if((*eachFuncTaint)->func == f){
                return *eachFuncTaint;
            }
        }
        return nullptr;
    }

  protected:

    map<Function*, map<CallInst*,tuple<int,int,int> > > seed_map;
    map<tuple<int,int,int>,set<int> > CalleesTxtMap;
    CS2Callees CalleesMap;
    //some temporary variables
    map<string,map<string,CallInst*> > callsiteMap;
    map<int,Function*> funcMap;
    map<string,Function*> func_str_map;
    // record the caller relations by mapping
    // functionName ->
    // functionName(in which the indirect call resides, the indirect call)
    map<string,set<pair<string,string> > > CallersTxtMap;
    Func2Callers CallersMap;

    void processCalleeUnit(vector<string>& calleeUnit);
    void processCalleeUnit_new(vector<string>& calleeUnit);
    void processCallerUnit(vector<string>& calleeUnit);
    void updateCallersAndCallees(Function * f, CallInst* ci);
    bool renderCallGraph();
    bool renderCallGraph(string funcname);
    Instruction* getInstByIndex(Function*,int,int);
    //taint summary
    vector<FunctionTaintSummary*> moduleTaintSummary;


    Module &mo;
    // info provider
    const DataLayout &dl;
    const TargetLibraryInfo &tli;
    Triple triple;
    
    
    //# User space input
    // record the _user argument 
    // functionName -> argIdx, argName
    map<Function*,set<pair<int,string> > > user_args;
    // sysctl glbs
    vector<SysctlInfo> sysctls;
    //syscall ioctls
    set<Function*> syscallIoctls;


    // consts
    const uint64_t BITS = 8;
};


#endif /* ORACLE_H_ */
