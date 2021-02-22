#ifndef ANALYSIS_DRIVER_H_
#define ANALYSIS_DRIVER_H_

#define EXTERNAL_DEPENDENCIES 0
#define IMCOMPLETE_SYMBOLIZATION 1

#define SATISFIABLE_BUG 1
#define BUG_WITH_FETCH 2
#define SATISFIABLILITY_CHANGABLE_BY_FETCH 4
#include "Project.h"

class SAHandle {
  public:
    SAHandle(Function &f, ModuleOracle &m)
        : func(f), mo(m),
          fo(f, m.getDataLayout(), m.getTargetLibraryInfo()),
          fwriter(nullptr),layer(0)
    {}

    SAHandle(Function &f, ModuleOracle &m, FileWriter *fw,set<int > user)
        : func(f), mo(m),
          fo(f, m.getDataLayout(), m.getTargetLibraryInfo()),
          fwriter(fw),layer(0),
          userInput(user)
    {}
    SAHandle(Function &f,Function* calleeF, ModuleOracle &m, FileWriter *fw,set<int> user,\
    set<CallInst*> callSites, map<set<int>,vector<TraceStatus*> > * calleeTs,\
    FunctionTaintSummary* taint_sum,int layer)
            : func(f), calleeFunc(calleeF), mo(m),
              fo(f, m.getDataLayout(), m.getTargetLibraryInfo()),layer(layer),
              fwriter(fw),callSites(callSites),userInput(user),calleesTraces(calleeTs),taintSummary(taint_sum)
    {
    }

    ~SAHandle() {
        for(auto const &i : fts){
            delete i.second;
        }
        userInput.clear();
        callSites.clear();

        for(auto eachDepCallee = interPTraces.begin(); eachDepCallee != interPTraces.end(); eachDepCallee++){
            for(auto eachInterTrace = eachDepCallee->second.begin();eachInterTrace != eachDepCallee->second.end(); eachInterTrace++){
                delete(*eachInterTrace);
            }
            eachDepCallee->second.clear();
            eachDepCallee->second.shrink_to_fit();
        }
        interPTraces.clear();
        interPTraces_by_seed.clear();
        result.clear();
    }

    void run();
    void runInter();

    void resolveSyscall();
    bool getIsSyscall()      {return isSyscall;}
    Function& getFunc()      {return func;}
    //inter-procedural
    // the traces which depend on the same set of dependent arguments(specified as the first half of this pair)
    // are groupped into one pair
    map<set<int>, vector<TraceStatus*> > interPTraces;
    map<CallInst*,int >interPTraces_by_seed;

    Instruction* seed2sanitizedVar(CallInst*seed,enum UB_types ubtype);
    void mergeResult(map<CallInst*,UBResult>&);
    int getSeedNum(){return seeds.size();}
    static set<CallInst*> aggregateResult(map<CallInst*,UBResult>&,UBResult res);
    void timed_out() {time_out = true;};

    void write_num_interTrace_to_file();
    static void write_result_to_file(ModuleOracle&,FileWriter*,map<CallInst*,UBResult>&);
    void remove_seeds_from_interTraces(set<CallInst*>);

  protected:
    //seed data dependency
    set<Value*> seedDeps(Instruction*seed);
    // seed instruction collection
    void collectSeed();

    void collectFetch();
    Fetch *getFetchFromInst(Instruction *inst);

    // specific analysis
    bool analyze(CallInst *seed);
    bool analyzeInter(CallInst *seed,vector<TraceStatus*>&);
    void analyzeTrace(TraceStatus*, SliceOracle &, iseq & , blist & ,Slice & );
    void analyzeTraceInter(CallInst *inst, SliceOracle &so, blist &blks,Slice& old_slice,vector<TraceStatus*>& traceStatus,vector<TraceStatus*>&);

    bool stillWrong(Instruction * i,SEGraph*seg);
    int postBugAnalysis(TraceStatus*, blist& , Slice&);
    // fetch results
    void addResult(CallInst*, UBResult res);




    bool formalizeUB(SENodeSpecialCall*,int);
    // output results
    bool isFileWriterInitialized() { return fwriter != nullptr; }

    // the taint summary for this func
    FunctionTaintSummary* taintSummary;

  protected:
    // context
    Function &func;
    Function *calleeFunc;
    // indicating if this is a syscall
    bool isSyscall;
    // oracle
    ModuleOracle &mo;
    FuncOracle fo;

    bool time_out = false;
    //how many hops in the call chain
    int layer;

    set<CallInst *> seeds;
    map<Instruction *, Fetch *> fts;

    // results
    map<CallInst *, UBResult > result;
    set<Fetch *> failed;
    //record the number of the traces for each seed.
    vector<int> numTotalTrace;
    //record the number of callee traces for each callInst(seed)
    vector<int> numCalleeTraces;
    // outputs
    FileWriter *fwriter;
    //seeded call inst
    set<CallInst*> callSites;
    //user provided args for this func
    set<int> userInput;
    // traces from callees, that need to be concatenated
    map<set<int>,vector<TraceStatus*> >* calleesTraces;

};

Instruction* seed2root(CallInst*);
#endif /* ANALYSIS_DRIVER_H_ */
