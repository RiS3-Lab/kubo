#ifndef SEG_H_
#define SEG_H_

// typedefs
typedef vector<Instruction *> iseq;

// type tables
enum SEKind {
  SE_LEAF,
  SE_VAR,
  SE_INST,
  SE_UNKNOWN
};

enum SEImportance{
    SE_Vital,
    SE_Normal,
    SE_Needless
};

enum SEType {
  // leaf node
  SE_CInt,
  SE_CFloat,
  SE_CNull,
  SE_CAgg,
  SE_CAggZero,
  SE_CDataSeq,
  SE_CBlkAddr,
  // var node
  SE_Param,
  SE_Global,
  SE_Local,
  SE_Undef,

  // inst node
  SE_Cast,
  SE_CastTrunc,
  SE_CastZExt,
  SE_CastSExt,
  SE_Cast2Ptr,
  SE_Cast2Int,
  SE_CastType,
  SE_CastFPTrunc,
  SE_CastFPExt,
  SE_CastFPToSI,
  SE_CastFPToUI,
  SE_CastSIToFP,
  SE_CastUIToFP,

  SE_CalcAdd,
  SE_CalcSub,
  SE_CalcMul,
  SE_CalcUDiv,
  SE_CalcSDiv,
  SE_CalcURem,
  SE_CalcSRem,
  SE_CalcShl,
  SE_CalcLShr,
  SE_CalcAShr,
  SE_CalcAnd,
  SE_CalcOr,
  SE_CalcXor,
  SE_CalcFAdd,
  SE_CalcFSub,
  SE_CalcFMul,
  SE_CalcFDiv,
  SE_CalcFRem,
  SE_Calc,

  SE_ICmpEq,
  SE_ICmpNe,
  SE_ICmpRel,
  SE_ICmp,

  SE_FCmpEq,
  SE_FCmpNe,
  SE_FCmpRel,
  SE_FCmpTrue,
  SE_FCmpFalse,
  SE_FCmp,

  SE_GEPIdx0,
  SE_GEPIdx1,
  SE_GEPIdx2,
  SE_GEPIdx3,
  SE_GEP,

  SE_Phi,
  SE_Select,
  SE_Branch,

  SE_Load,
  SE_Store,

#define LIBCALL_ENUM
#include "Libcall.def"
#undef LIBCALL_ENUM
  SE_Call,

#define ASMCALL_ENUM
#include "Asmcall.def"
#undef ASMCALL_ENUM
  SE_Asm,


  SE_ExtVal,
  SE_InsertVal,

  // others
  //SE_SpecialCall presents the seed, and call sites(concatenation point)
  SE_SpecialCall,
  //SE_UnhandledCall presents all other call instructions that are not handled
  SE_UnhandledCall,
  SE_Call_Unhandled0,
  SE_Call_Unhandled1,
  SE_Call_Unhandled2,
  SE_Call_Unhandled3,
  SE_Call_Unhandled4,

  SE_UnhandledAsm,
  SE_Asm_Unhandled0,
  SE_Asm_Unhandled1,
  SE_Asm_Unhandled2,
  SE_Asm_Unhandled3,

  SE_Unhandled
};

// forward declear
class SENode;

// classes
class SEOpv {
  public:
    SEOpv(SEType t, unsigned n, int s, Instruction *i)
      : type(t), seq(s), inst(i) {

      vals.reserve(n);
    }

    virtual ~SEOpv() {}

    virtual SEOpv* copy(){}
    SEType getType() const {
      return type;
    }

    Instruction *getInst() {
      return inst;
    }

    // although addUsr/addDep is defined in SENode,
    // this is the function to actually implement the linkage
    void setHost(SENode *h);
    void setHost_host(SENode*h);
    void addVal(SENode *val) {
      vals.push_back(val);
    }

    void replace(SENode* old, SENode* newN){
        std::replace(vals.begin(),vals.end(),old,newN);
    }
    SENode *at(unsigned i) {
      return vals.at(i);
    }

    // check if this SEOpv is ready to get symbolized
    bool ready(SymExec &sym);

    virtual void symbolize(SymExec &sym, SymVar *var) = 0;
    int numDeps(){return vals.size();};
  protected:
    // rtti
    SEType type;

    // basics
    int seq;
    Instruction *inst;

    // links
    SENode *host;
    vector<SENode *> vals;
};

class SEOpv0 : public SEOpv {
  public:
    SEOpv0(SEType t, int s, Instruction *i)
      : SEOpv(t, 0, s, i) {}

    virtual SymVal *derive(SymExec &sym) = 0;
    virtual SEOpv* copy(){}
    void symbolize(SymExec &sym, SymVar *var) override;
};

class SEOpv1 : public SEOpv {
  public:
    SEOpv1(SEType t, int s, Instruction *i,
        SENode *n0)
      : SEOpv(t, 1, s, i) {

      addVal(n0);
    }

    virtual SymVal *derive(SymExec &sym,
        SymVal *i0) = 0;

    virtual SEOpv* copy(){}

    void symbolize(SymExec &sym, SymVar *var) override;
};

class SEOpv2 : public SEOpv {
  public:
    SEOpv2(SEType t, int s, Instruction *i,
        SENode *n0, SENode *n1)
      : SEOpv(t, 2, s, i) {

      addVal(n0);
      addVal(n1);
    }

    virtual SymVal *derive(SymExec &sym,
        SymVal *i0, SymVal *i1) = 0;

    virtual SEOpv* copy(){}

    void symbolize(SymExec &sym, SymVar *var) override;
};

class SEOpv3 : public SEOpv {
  public:
    SEOpv3(SEType t, int s, Instruction *i,
        SENode *n0, SENode *n1, SENode *n2)
      : SEOpv(t, 3, s, i) {

      addVal(n0);
      addVal(n1);
      addVal(n2);
    }

    virtual SymVal *derive(SymExec &sym,
        SymVal *i0, SymVal *i1, SymVal *i2) = 0;

    virtual SEOpv* copy(){}

    void symbolize(SymExec &sym, SymVar *var) override;
};

class SEOpv4 : public SEOpv {
public:
    SEOpv4(SEType t, int s, Instruction *i,
           SENode *n0, SENode *n1, SENode *n2,SENode *n3)
            : SEOpv(t, 4, s, i) {

        addVal(n0);
        addVal(n1);
        addVal(n2);
        addVal(n3);
    }

    virtual SymVal *derive(SymExec &sym,
                           SymVal *i0, SymVal *i1, SymVal *i2, SymVal *i3) = 0;

    virtual SEOpv* copy(){}

    void symbolize(SymExec &sym, SymVar *var) override;
};

// subclass helpers
#define OPV_0(se_type)                                                \
  class SEOpv##se_type : public SEOpv0 {                              \
    public:                                                           \
      SEOpv##se_type(int s, Instruction *i)                           \
        : SEOpv0(SE_##se_type, s, i) {}                               \
                                                                      \
      ~SEOpv##se_type() {}                                            \
                                                                      \
      static bool classof(const SEOpv *op) {                          \
        return op->getType() == SE_##se_type;                         \
      }                                                               \
      SEOpv* copy(){                                                  \
         SEOpv* ret = new SEOpv##se_type(seq,inst);                   \
         ret->setHost_host(host);                                          \
         return ret;                                                  \
      }                                                               \
      SymVal *derive(SymExec &sym) override;                          \
  };

#define OPV_1(se_type, op0)                                           \
  class SEOpv##se_type : public SEOpv1 {                              \
    public:                                                           \
      SEOpv##se_type(int s, Instruction *i,                           \
          SENode *op0)                                                \
        : SEOpv1(SE_##se_type, s, i, op0) {}                          \
                                                                      \
      ~SEOpv##se_type() {vals.clear();}                               \
                                                                      \
      static bool classof(const SEOpv *op) {                          \
        return op->getType() == SE_##se_type;                         \
      }                                                               \
      SEOpv* copy(){                                                  \
         SEOpv* ret = new SEOpv##se_type(seq,inst,vals[0]);           \
         ret->setHost_host(host);                                          \
         return ret;                                                  \
      }                                                               \
                                                                      \
      SymVal *derive(SymExec &sym,                                    \
          SymVal *i0)                                                 \
          override;                                                   \
  };

#define OPV_2(se_type, op0, op1)                                      \
  class SEOpv##se_type : public SEOpv2 {                              \
    public:                                                           \
      SEOpv##se_type(int s, Instruction *i,                           \
          SENode *op0, SENode *op1)                                   \
        : SEOpv2(SE_##se_type, s, i, op0, op1) {}                     \
                                                                      \
      ~SEOpv##se_type() {vals.clear();}                               \
                                                                      \
      static bool classof(const SEOpv *op) {                          \
        return op->getType() == SE_##se_type;                         \
      }                                                               \
      SEOpv* copy(){                                                  \
         SEOpv* ret = new SEOpv##se_type(seq,inst,vals[0],vals[1]);   \
         ret->setHost_host(host);                                          \
         return ret;                                                  \
      }                                                               \
                                                                      \
      SymVal *derive(SymExec &sym,                                    \
          SymVal *i0, SymVal *i1)                                     \
          override;                                                   \
  };

#define OPV_3(se_type, op0, op1, op2)                                        \
  class SEOpv##se_type : public SEOpv3{                                      \
    public:                                                                  \
      SEOpv##se_type(int s, Instruction *i,                                  \
          SENode *op0, SENode *op1, SENode *op2)                             \
        : SEOpv3(SE_##se_type, s, i, op0, op1, op2) {}                       \
                                                                             \
      ~SEOpv##se_type() {vals.clear();}                                      \
                                                                             \
      static bool classof(const SEOpv *op) {                                 \
        return op->getType() == SE_##se_type;                                \
      }                                                                      \
      SEOpv* copy(){                                                         \
         SEOpv* ret = new SEOpv##se_type(seq,inst,vals[0],vals[1],vals[2]);   \
         ret->setHost_host(host);                                                  \
         return ret;                                                          \
      }                                                                       \
                                                                              \
      SymVal *derive(SymExec &sym,                                            \
          SymVal *i0, SymVal *i1, SymVal *i2)                                 \
          override;                                                           \
  };

#define OPV_4(se_type, op0, op1, op2, op3)                                                       \
  class SEOpv##se_type : public SEOpv4{                                                          \
    public:                                                                                      \
      SEOpv##se_type(int s, Instruction *i,                                                      \
          SENode *op0, SENode *op1, SENode *op2,SENode* op3)                                     \
        : SEOpv4(SE_##se_type, s, i, op0, op1, op2, op3) {}                                      \
                                                                                                 \
      ~SEOpv##se_type() {vals.clear();}                                                          \
                                                                                                 \
      static bool classof(const SEOpv *op) {                                                     \
        return op->getType() == SE_##se_type;                                                    \
      }                                                                                          \
      SEOpv* copy(){                                                                             \
         SEOpv* ret = new SEOpv##se_type(seq,inst,vals[0],vals[1],vals[2],vals[3]);              \
         ret->setHost_host(host);                                                                     \
         return ret;                                                                             \
      }                                                                                          \
                                                                                                 \
      SymVal *derive(SymExec &sym,                                                               \
          SymVal *i0, SymVal *i1, SymVal *i2, SymVal * i3)                                       \
          override;                                                                              \
  };

class SENode {
  public:
    SENode(SEKind k, SEType t, int s, Value *v, SEGraph *g)
      : kind(k), type(t), seq(s), val(v), graph(g) {}

    virtual ~SENode() {
        deps.clear();
        usrs.clear();
    }

    virtual SENode* copy(SEGraph* newg){}

    void setSEG(SEGraph* newSEG){
        graph = newSEG;
    }
    void setId(unsigned i) {
      id = i;
    }

    unsigned getId() {
      return id;
    }

    int getSeq() {
      return seq;
    }
    void setSeq(int newSeq) {
        seq = newSeq ;
    }

    Value *getVal() {
      return val;
    }

    SEType getType() const {
      return type;
    }

    SEKind getKind() const {
      return kind;
    }

    SEGraph *getGraph() {
      return graph;
    }

    void addUsr(SENode *n) {
      usrs.insert(n);
    }

    void delUsr(SENode *n){
        assert(usrs.find(n)!=usrs.end());
        usrs.erase(n);
    }

    void addDep(SENode *n) {
      deps.insert(n);
    }

    void delDep(SENode *n) {
      assert(deps.find(n) != deps.end());
      deps.erase(n);
    }

    unsigned numUsrs() {
      return usrs.size();
    }

    void display(){
       errs()<<"  "<<getId()<<"|"<<getSeq()<<'|'<<*getVal()<<"\n";
    }
    unsigned numDeps() {
      return deps.size();
    }

    typedef typename set<SENode *>::iterator linkIter;

    linkIter usrBegin() {
      return usrs.begin();
    }

    linkIter usrEnd() {
      return usrs.end();
    }

    linkIter depBegin() {
      return deps.begin();
    }

    linkIter depEnd() {
      return deps.end();
    }

    //SEImportance getImportance(){
    //    return imp;
   // }
    //void setImportance(SEImportance im){
    //    imp = im;
    //}



    virtual SymVar *getSymbol(SymExec &sym) = 0;
    set<SENode *> deps;
    set<SENode *> usrs;
  protected:
    // rtti
    SEKind kind;
    SEType type;
    //SEImportance imp = SE_Normal;
    // basics
    unsigned id;
    int seq;
    Value *val;
    // links
    SEGraph *graph;
};

class SENodeLeaf : public SENode {
  public:
    SENodeLeaf(SEType t, int s, Value *v, SEGraph *g)
      : SENode(SE_LEAF, t, s, v, g) {}

    virtual ~SENodeLeaf() {}

    virtual SENode* copy(SEGraph*newg) {};
    virtual SymVal *derive(SymExec &sym) = 0;

    void symbolize(SymExec &sym, SymVar *var) {
      var->add(derive(sym));
      var->simplify(sym.getContext());
    }

    SymVar *getSymbol(SymExec &sym) override;

    static bool classof(const SENode *node) {
      return node->getKind() == SE_LEAF;
    }
};

class SENodeVar : public SENode {
  public:
    SENodeVar(SEType t, int s, Value *v, SEGraph *g)
      : SENode(SE_VAR, t, s, v, g) {}

    virtual ~SENodeVar() {}

    virtual SENode*  copy(SEGraph* newg){}
    virtual SymVal *derive(SymExec &sym) = 0;

    void symbolize(SymExec &sym, SymVar *var) {
      var->add(derive(sym));
      var->simplify(sym.getContext());
    }

    SymVar *getSymbol(SymExec &sym) override;

    static bool classof(const SENode *node) {
      return node->getKind() == SE_VAR;
    }
};

class SENodeInst : public SENode {
  public:
    SENodeInst(SEType t, int s, Value *v, SEGraph *g)
      : SENode(SE_INST, t, s, v, g) {}

    ~SENodeInst() {
      for(SEOpv *i : opval){
        delete i;
      }
    }
    virtual SENode* copy(SEGraph*newg){}

    void addOpv(SEOpv *op) {
      op->setHost(this);
      opval.push_back(op);
    }
    void addOpval(SEOpv *op) {
      opval.push_back(op);
    }

    unsigned numOpv() {
      return opval.size();
    }

    SEOpv *getOpv(unsigned i) {
      return opval.at(i);
    }

    SEOpv *getSingleOpv() {
      assert(opval.size() == 1);
      return opval.at(0);
    }
    void replace(SENode* oldNode, SENode* newNode){
        for(SEOpv *i : opval){
            i->replace(oldNode, newNode);
        }
    }

    void symbolize(SymExec &sym, SymVar *var) {
      for(SEOpv *i : opval){
        i->symbolize(sym, var);
      }
      // BUG(yaohway/when z3 timeout sets to a small number,
      // the solver might timeout when simplifying the z3_ast
      // and throw a exception, which if not handled will terminate
      // the process.)

      // TODO(yaohway/handle timeout exception, and reenable the simplication)
      // var->simplify(sym.getContext());
    }

    SymVar *getSymbol(SymExec &sym) override;

    static bool classof(const SENode *node) {
      return node->getKind() == SE_INST;
    }

  protected:
    vector<SEOpv *> opval;
};

class SENodeUnknown : public SENode {
  public:
    SENodeUnknown(int s, Value *v, SEGraph *g)
      : SENode(SE_UNKNOWN, SE_Unhandled, s, v, g) {}

    ~SENodeUnknown() {}

    SENode* copy(SEGraph* newg){
        SENode* newNode = new SENodeUnknown(seq,val,newg);
        return newNode;
    }
    SymVar *getSymbol(SymExec &sym) override;

    static bool classof(const SENode *node) {
      return node->getKind() == SE_UNKNOWN;
    }
};

// subclass helpers
#define LEAF_NODE(se_type, val_type)                                      \
  class SENode##se_type : public SENodeLeaf {                             \
    public:                                                               \
      SENode##se_type(int s, val_type *v, SEGraph *g)                     \
        : SENodeLeaf(SE_##se_type, s, v, g) {}                            \
                                                                          \
      ~SENode##se_type() {}                                               \
      SENode* copy(SEGraph* newg){                                        \
        SENode* ret = new SENode##se_type(seq,cast<val_type>(val),newg);  \
        ret->setId(id);                                                   \
        for(auto usrIt = usrs.begin();usrIt != usrs.end();usrIt++)        \
            ret->addUsr(*usrIt);                                          \
        for(auto depIt = deps.begin();depIt != deps.end();depIt++)        \
            ret->addDep(*depIt);                                          \
        return  ret;                                                      \
       }                                                                  \
      static bool classof(const SENode *node) {                           \
        return node->getType() == SE_##se_type;                           \
      }                                                                   \
                                                                          \
      SymVal *derive(SymExec &sym) override;                              \
                                                                          \
    public:                                                               \
      val_type *getCastedVal() {                                          \
        return cast<val_type>(val);                                       \
      }                                                                   \
  };

#define VAR_NODE(se_type, val_type)                                      \
  class SENode##se_type : public SENodeVar {                             \
    public:                                                              \
      SENode##se_type(int s, val_type *v, SEGraph *g)                    \
        : SENodeVar(SE_##se_type, s, v, g) {}                            \
                                                                         \
      ~SENode##se_type() {}                                              \
      SENode* copy(SEGraph* newg){                                       \
        SENode* ret = new SENode##se_type(seq,cast<val_type>(val),newg); \
        ret->setId(id);                                                   \
        for(auto usrIt = usrs.begin();usrIt != usrs.end();usrIt++)        \
            ret->addUsr(*usrIt);                                          \
        for(auto depIt = deps.begin();depIt != deps.end();depIt++)        \
            ret->addDep(*depIt);                                          \
        return  ret;                                                     \
       }                                                                 \
      static bool classof(const SENode *node) {                          \
        return node->getType() == SE_##se_type;                          \
      }                                                                  \
                                                                         \
      SymVal *derive(SymExec &sym) override;                             \
                                                                         \
    public:                                                              \
      val_type *getCastedVal() {                                         \
        return cast<val_type>(val);                                      \
      }                                                                  \
  };

#define INST_NODE(se_type, val_type)                                               \
  class SENode##se_type : public SENodeInst {                                      \
    public:                                                                        \
      SENode##se_type(int s, val_type *v, SEGraph *g)                              \
        : SENodeInst(SE_##se_type, s, v, g) {}                                     \
                                                                                   \
      ~SENode##se_type() {}                                                        \
                                                                                   \
      SENode* copy(SEGraph*newg){                                                  \
        SENode##se_type* ret = new SENode##se_type(seq,cast<val_type>(val),newg);  \
        ret->setId(id);                                                            \
        for(auto usrIt = usrs.begin();usrIt != usrs.end();usrIt++)                 \
            ret->addUsr(*usrIt);                                                   \
        for(auto depIt = deps.begin();depIt != deps.end();depIt++)                 \
            ret->addDep(*depIt);                                                   \
        int numOp =  opval.size();                                                 \
        for(int i = 0 ; i < numOp;i++)                                             \
            ret->addOpval(opval[i]->copy());                                       \
        return cast<SENode>(ret);                                                  \
      }                                                                            \
      static bool classof(const SENode *node) {                                    \
        return node->getType() == SE_##se_type;                                    \
      }                                                                            \
                                                                                   \
    public:                                                                        \
      val_type *getCastedVal() {                                                   \
        return cast<val_type>(val);                                                \
      }                                                                            \
  };

// leaf nodes
LEAF_NODE(CInt, ConstantInt);

LEAF_NODE(CFloat, ConstantFP);

LEAF_NODE(CNull, ConstantPointerNull);

LEAF_NODE(CAgg, ConstantAggregate);

LEAF_NODE(CAggZero, ConstantAggregateZero);

LEAF_NODE(CDataSeq,ConstantDataSequential);

LEAF_NODE(CBlkAddr,BlockAddress);
// var nodes
VAR_NODE(Param, Argument);

VAR_NODE(Global, GlobalValue);

VAR_NODE(Local, AllocaInst);

VAR_NODE(Undef, UndefValue);

// inst nodes and opvals
OPV_1(CastTrunc, Orig);
OPV_1(CastZExt, Orig);
OPV_1(CastSExt, Orig);
OPV_1(Cast2Int, Orig);
OPV_1(Cast2Ptr, Orig);
OPV_1(CastType, Orig);
OPV_1(CastFPTrunc, Orig);
OPV_1(CastFPExt, Orig);
OPV_1(CastFPToSI, Orig);
OPV_1(CastFPToUI, Orig);
OPV_1(CastSIToFP, Orig);
OPV_1(CastUIToFP, Orig);
INST_NODE(Cast, CastInst);


OPV_2(CalcAdd, Lhs, Rhs);
OPV_2(CalcSub, Lhs, Rhs);
OPV_2(CalcMul, Lhs, Rhs);
OPV_2(CalcUDiv, Lhs, Rhs);
OPV_2(CalcSDiv, Lhs, Rhs);
OPV_2(CalcURem, Lhs, Rhs);
OPV_2(CalcSRem, Lhs, Rhs);
OPV_2(CalcShl, Lhs, Rhs);
OPV_2(CalcLShr, Lhs, Rhs);
OPV_2(CalcAShr, Lhs, Rhs);
OPV_2(CalcAnd, Lhs, Rhs);
OPV_2(CalcOr, Lhs, Rhs);
OPV_2(CalcXor, Lhs, Rhs);
OPV_2(CalcFAdd, Lhs, Rhs);
OPV_2(CalcFSub, Lhs, Rhs);
OPV_2(CalcFMul, Lhs, Rhs);
OPV_2(CalcFDiv, Lhs, Rhs);
OPV_2(CalcFRem, Lhs, Rhs);
INST_NODE(Calc, BinaryOperator)

OPV_2(ICmpEq, Lhs, Rhs);
OPV_2(ICmpNe, Lhs, Rhs);
OPV_2(ICmpRel, Lhs, Rhs);
INST_NODE(ICmp, ICmpInst);

OPV_0(FCmpTrue);
OPV_0(FCmpFalse);
OPV_2(FCmpEq, Lhs, Rhs);
OPV_2(FCmpNe, Lhs, Rhs);
OPV_2(FCmpRel, Lhs, Rhs);
INST_NODE(FCmp, FCmpInst);

OPV_1(GEPIdx0, Ptr);
OPV_2(GEPIdx1, Ptr, Idx0);
OPV_3(GEPIdx2, Ptr, Idx0, Idx1);
OPV_4(GEPIdx3, Ptr, Idx0, Idx1, Idx2);
INST_NODE(GEP, GetElementPtrInst);

OPV_1(Phi, Tran);
INST_NODE(Phi, PHINode);

OPV_3(Select, CVal, TVal, FVal);
INST_NODE(Select, SelectInst);

OPV_1(Branch, Cval);
INST_NODE(Branch, BranchInst);

OPV_1(Load, Ptr);
INST_NODE(Load, LoadInst);

OPV_2(Store, Ptr, Vop);
INST_NODE(Store, StoreInst);



INST_NODE(UnhandledCall, CallInst);
INST_NODE(SpecialCall, CallInst);

#define LIBCALL_DOPV
#include "Libcall.def"
#undef LIBCALL_DOPV
INST_NODE(Call, CallInst);


INST_NODE(UnhandledAsm,CallInst);
#define ASMCALL_DOPV
#include "Asmcall.def"
#undef ASMCALL_DOPV
INST_NODE(Asm, CallInst);


OPV_1(ExtVal, Ptr);
INST_NODE(ExtVal, ExtractValueInst);

OPV_2(InsertVal, Ptr, Val);
INST_NODE(InsertVal, InsertValueInst);

// the SEG graph
class SEGraph {
  public:
    SEGraph(SliceOracle *s, iseq t, Function& f);
    SEGraph(SliceOracle *s, iseq t,map<SENode *, int>& oldConds,Function& f);

    ~SEGraph() {
      for(auto each_node = nodes.begin(); each_node != nodes.end(); each_node++){

          //errs()<<"deleting node:"<<each_node->second<<'\n';
          delete each_node->second;
          each_node->second = nullptr;
      }
      nodes.clear();
      nodes_back.clear();

      trace_back.clear();
      trace_back.shrink_to_fit();
      trace.clear();
      trace.shrink_to_fit();


      conds_back.clear();
      conds.clear();

      updated_values.clear();

      // no need to delete Slice Oracle, since it's on the stack
    }

    void addNode(SENode *node) {
        auto key = make_pair(node->getSeq(), node->getVal());
        assert(nodes.find(key) == nodes.end());
        node->setId(count++);
        nodes.insert(make_pair(key, node));
    }
    void copy(map<int,SENode*>&newTrace,SEGraph* copy_to);
    void deleteNode(SENode* todel){
        SENode::linkIter di = todel->depBegin(), de = todel->depEnd();
        for(; di != de; ++di){
            (*di)->delUsr(todel);
        }
        SENode::linkIter ui = todel->usrBegin(), ue = todel->usrEnd();
        for(; ui != ue; ++ui){
            (*ui)->delDep(todel);
        }
        nodes.erase(make_pair(todel->getSeq(), todel->getVal()));
        //delete it from conds
        if(conds.find(todel) != conds.end()){
            conds.erase(todel);
        }
        delete todel;
    }

    // for get node, seq must match the location of val
    SENode *getNode(int seq, Value *val);

    // for get or build node, seq might not match the location of val,
    // but must be equal of less than the location of val
    SENode *getNodeOrBuild(int cur, Value *val);

    // for get node or null, seq must the location of val
    SENode *getNodeOrNull(int seq, Value *val);

    // for get node or fail, cur might not match the location of val,
    SENode *getNodeOrFail(int cur, Value *val);

    // get the node with the highest in location
    SENode *getNodeProbe(Value *val,bool ask_update = true);

    // get the node according to id
    SENode *getNodeById(int id);
    // node iterator
    typedef typename map<pair<int, Value *>, SENode *>::iterator iterator;

    iterator begin() {
      return nodes.begin();
    }

    iterator end() {
      return nodes.end();
    }

    void filterTrace(iseq &filt);

    SENodeSpecialCall* getSeed(){
        SENode* node = getNodeProbe(trace.back());
        return cast<SENodeSpecialCall>(node);
    }
    // get condition
    int getCond(SENode *node) {
      auto i = conds.find(node);
      if(i == conds.end()){
        return -1;
      } else {
        return i->second;
      }
    }
    void displayConds(){
        for(auto condIt = conds.begin(); condIt != conds.end(); condIt++){
            SENode* node = condIt->first;
            Value * cond_val = node->getVal();
            int cond_int = condIt->second;
            int cond_id = node->getId();
            errs()<<"br:"<<*(cond_val)<<"|id:"<<cond_id<<"|value:"<<cond_int<<'\n';
        }
    }

    bool unhandledNode();
    // symbolize the seg
    // input : the oracle of the symbolization system
    // output: if there is any conditional variable that is affected by unhandled instruction
    //         if there is, it means this constraint is not complete.
    bool symbolize(SymExec &sym);
    bool additional_constraints(SymExec &sym);

    // replay the instruction with the solved model and dump the trace
    void replay(SymExec &sym);

    // mark the unhandled paras and fetch's desitantion
    //void markImportance();
    // check if there is any authentic constraint on globals or
    bool constraintedGlobalsOrParas(set<pair<int,string> >);

    //check if there is any constaint from the UB branch to parameter
    bool externalDependency(SENode* brNode,set<int>&);
    //record the cause for SENode condtional variables
    vector<Instruction*> failCauses;

    //to see if the fetch effect the
    bool fetchTriggerabilityScan(SENode*);
    bool fetchTriggerabilityScan(vector<SENode*>&);
    void expandDep2(set<SENode*>&deps);
    void expandDep3(set<SENode*>&deps);
    //add more constraints to the seg
    void push_context(SliceOracle*);
    void pop_context();
    void additional_seg(iseq &t, SliceOracle*);

    int mergeSEG(SEGraph*,CallInst*,set<int>&);
    bool in_original_node_set(SENode*);

    string str_nodetype(SENode*);
    void displayTrace();
    void displayForDeps();
    void displayForUsrs();
    void displayForDeps(set<SENode*>);
    void displayDepForNode(SENode*node);
    void displaySubstitutes();

    map<SENode *, int>& getConds(){return conds;};
    bool abort = false;

    map<Value*,Value*> updated_values;

    Function & func;
protected:
    // locator
    bool locateValue(int cur, Value *val, int &seq);

    // for build node method, cur might match the location of val
    SENode *buildNode(int seq, Value *val);

    SENode* buildSpecial(int seq,CallInst*val);
    // for trace following and triming
    void followTrace();
    void trimGraph();
    void verify();
    int get_highest_seq();

protected:

    // info provider
    SliceOracle *so;
    SliceOracle *so_back;


    // trace
    iseq trace;
    iseq trace_back;
    // basics
    unsigned count;
    unsigned count_back;
    int highest_seq = -1;

    // graph
    map<pair<int, Value *>, SENode *> nodes;
    map<pair<int, Value *>, SENode *> nodes_back;

    // conditions, int takes 0 represent true, and 1 represent false
    map<SENode *, int> conds;
    map<SENode *, int> conds_back;

    friend class UB_root;
};


// helpers
namespace SEGUtil {

static bool decompose(GetElementPtrInst *gep, vector<Value *> &vars) {
  Value *ptr = gep->getPointerOperand();

  Value *idx;
  Type *cty, *nty;

  cty = ptr->getType();

  User::op_iterator i = gep->idx_begin(), ie = gep->idx_end();
  for(; i != ie; ++i){
    idx = i->get();
    if(!isa<ConstantInt>(idx)){
      vars.push_back(idx);
    }

    // nothing to GEP on if it is integer type
    assert(!isa<IntegerType>(cty));

    if(isa<PointerType>(cty)){
      nty = cty->getPointerElementType();
    }

    else if(isa<StructType>(cty)){
      if(!isa<ConstantInt>(idx)){
          errs()<<"idx:"<<*idx<<'\n';
          errs()<<"cty:"<<*cty<<'\n';
          errs()<<"nty:"<<*nty<<'\n';
          errs()<<"gep:"<<*gep<<'\n';
          errs()<<"index into the struct is not constant\n";
          return false;
      }
      unsigned fid = cast<ConstantInt>(idx)->getZExtValue();
      nty = cty->getStructElementType(fid);
    }

    else if(isa<ArrayType>(cty)){
      nty = cty->getArrayElementType();
    }
    else if(isa<VectorType>(cty)){
        nty = cty->getVectorElementType();
    }
    else {
      DUMP.typedValue(gep);
      llvm_unreachable("Unhandled GEP type");
    }

    cty = nty;
  }

  // ensure we derefered correctly
  if(cty != gep->getResultElementType()){
      errs()<<"Err:"<<*gep<<" GEP was not decomposed correctly\n";
  }
  return true;
}

static Value *backtrace(int seq, PHINode *phi, iseq &trace, SliceOracle *so) {
  if(so != nullptr){
      SliceBlock *host = so->getSliceHost(phi);
      assert(host != nullptr);

      SliceBlock *prev = nullptr;
      while(--seq >= 0){
          prev = so->getSliceHost(trace.at(seq));
          if(prev != host){
              break;
          }
      }
      assert(prev != nullptr && prev != host);
      assert(host->hasPred(prev) && host->inPTab(prev));

      Value *res = nullptr, *val;
      // iterate over incoming blocks
      PHINode::block_iterator bi = phi->block_begin(), be = phi->block_end();
      for(; bi != be; ++bi){
          if(host->inPTab(prev, *bi)){
              val = phi->getIncomingValueForBlock(*bi);

              if(res == nullptr) {
                  res = val;
              } else {
                  assert(res == val);
              }
          }
      }
      assert(res != nullptr);
      return res;
  }
  else{
      BasicBlock *host = phi->getParent();
      assert(host != nullptr);

      BasicBlock *prev = nullptr;
      while(--seq >= 0){
          prev = trace.at(seq)->getParent();
          if(prev != host){
              break;
          }
      }
      if(!(prev != nullptr && prev != host)){
          throw("prev not found in the host!");
      }

      Value *res = nullptr, *val;
      // iterate over incoming blocks
      PHINode::block_iterator bi = phi->block_begin(), be = phi->block_end();
      for(; bi != be; ++bi){
          if(*bi == prev){
              val = phi->getIncomingValueForBlock(*bi);
              if(res == nullptr) {
                  res = val;
                  break;
              } else {
                  assert(res == val);
              }
          }
      }
      if(res == nullptr){
          throw("Phi cannot be backtraced\n");
      }
      return res;
  }
}


} // end of SEGUtil namespace

#endif /* SEG_H_ */
