//
// Created by lcm on 1/20/2021.
//

#ifndef PASS_PROPAGATION_H
#define PASS_PROPAGATION_H

#include "Project.h"
#endif //PASS_PROPAGATION_H
// forwared declerations
class SENode;
class SEGraph;
class SEOpv;

class SENodeCalc;
class SENodeInst;
class SENodeCast;
class SENodeLeaf;
class SENodeLoad;
class SENodeGEP;
class SENodeStore;
class SENodeInsertVal;
class SENodeExtVal;
class SENodeSpecialCall;
class SENodeSelect;
class SENodePhi;
class SENodeBranch;
enum F_Op{
    F_Add,
    F_Sub,
    F_Mul,
    F_UDiv,
    F_SDiv,
    F_URem,
    F_SRem,
    F_Shl,
    F_LShr,
    F_AShr,
    F_And,
    F_Or,
    F_Xor,
    F_Fadd,
    F_Fsub,
    F_Fmul,
    F_Fdiv,
    F_Frem,
};



enum val_kind{
    F_Boolean,
    F_SInt,
    F_UInt,
    F_Float,
    F_Double,
    F_Ptr,
    F_BigInt,
    F_dummy
};

void collectDepChains(SENode* curNode,vector<SENode*>& depChain,vector<vector<SENode*>>* depChains);
void collectUseChains(SENode* curNode,vector<SENode*>& useChain,vector<vector<SENode*>>* useChains);

class F_AST{
public:
    enum F_ASTKind{
        AST_BINOP,
        AST_CAST,
        AST_VAR
    };
    // id can be used to associate current AST with the SENode
    int id;
private:
    const F_ASTKind LLVMKind;
public:
    F_ASTKind getKind() const{return LLVMKind;}
    F_AST(F_ASTKind K,int id): LLVMKind(K),id(id) {};
    virtual F_AST* copy(){}
};

class F_BinOp : public F_AST{
public:
    F_BinOp(int id,F_Op op_type,F_AST* left,F_AST* right):
        F_AST(AST_BINOP,id),type(op_type),left(left),right(right){}

    static bool classof(const F_AST*S){
        return S->getKind() == AST_BINOP;
    }
    F_AST* copy(){
        F_BinOp* ret = new F_BinOp(id,type,left,right);
        return ret;
    }
    F_Op type;
    F_AST* left;
    F_AST* right;
};

class F_CastOp: public F_AST{
public:
    F_CastOp(int id, F_AST* src,int bitWidth,val_kind v_kind):
        F_AST(AST_CAST,id),src(src),target_bitwidth(bitWidth),target_v_kind(v_kind){}

    static bool classof(const F_AST*S){
        return S->getKind() == AST_CAST;
    }

    F_AST* copy(){
        F_CastOp * ret = new F_CastOp(id,src,target_bitwidth,target_v_kind);
        return ret;
    }

    F_AST* src;
    int target_bitwidth;
    val_kind target_v_kind;
};
struct U_IntFloat {
    val_kind kind;
    union {
        bool boolean_var;
        uint64_t integer_var;
        float float_var;
        double double_var;
    };
    string big_int;
    U_IntFloat(val_kind kind):kind(kind){}
    U_IntFloat(const U_IntFloat& other){
        kind = other.kind;
        switch(kind){
            case F_SInt:
            case F_UInt:
                integer_var = other.integer_var;
                break;
            case F_Float:
                float_var = other.float_var;
                break;
            case F_Double:
                double_var = other.double_var;
                break;
        }
    }
    ~U_IntFloat(){
    }
};

class F_Val: public F_AST{
public:
    F_Val(int id,formalize_kind kind,int bitWidth,val_kind v_kind, U_IntFloat val):
        F_AST(AST_VAR,id),kind(kind),bitWidth(bitWidth),v_kind(v_kind),val(val){}

    static bool classof(const F_AST*S){
        return S->getKind() == AST_VAR;
    }

    F_AST* copy(){
        F_Val* ret = new F_Val(id,kind,bitWidth,v_kind,val);
        return ret;
    }
    formalize_kind kind;
    val_kind v_kind;
    int bitWidth;
    // if not const, this is ignored
    U_IntFloat val;
};

class UB_root{
public:
    //context-sensitive map of a SENode to <idx, TAG> where tag is the index of the element
    // if this SENode only has one element, then this is reflected by idx == -1
    map<SENode*,map<int,formalize_kind> > cache;
    set<GlobalValue*> non_user_glbs;
    set<Argument*> non_user_args;
    bool expandAndFormalize();
    bool root_controlled_by_user();

    //producing a .dot graph
    void renderAST(stringstream&);

    string getASTName(F_AST*);
    string stringBinOp(F_Op);
    string stringVal(val_kind kind);
    string stringKind(formalize_kind);
    static string stringUB_types(UB_types);


    UB_root(SENode* root,set<int> userArgs, ModuleOracle& mo);
    ~UB_root();

    void mergeStatusAndFormula(UB_root* another);
    void displayCache();
    //for debug purpose, display the nodes
    void displayNodes();
private:
    bool collectScope();
    void initialize_leaves();
    void updateStrip(SENode*,formalize_kind);
    //mem dep analysis
    bool memDepAna(SENode* ptrNode);
    //process call inst
    void processCall(SENode*);
    //process calculation
    void processCalc(SENodeCalc* calcNode);
    //process a cast inst
    void processCast(SENodeCast* castNode);
    //model loaded memory
    void processLoad(SENodeLoad*);
    //process a GEP
    void processGEP(SENodeGEP*);
    //process cmp
    void processCmp(SENode*);
    //process store
    void processStore(SENodeStore*);
    void processPhi(SENodePhi*);
    //process insertVal;
    void processInsertVal(SENodeInsertVal*);
    //process ExtVal
    void processExtVal(SENodeExtVal*);
    //process seed
    void processSeedNode(SENodeSpecialCall*);
    //process BrchNode
    void processBrchNode(SENodeBranch*);
    void processSelectNode(SENodeSelect*);
    //merge 2 different formaized_kind
    formalize_kind merge_kind(formalize_kind,formalize_kind);


    //build a F_Var for const
    void buildConst(SENodeLeaf*);
    //build a memory for a loaded memory
    void buildMem(SENode*,formalize_kind);
    void insert_cache(SENode*,int,formalize_kind,bool over_write = false);
    void insert_cache(SENode*,map<int,formalize_kind>);
private:
    SENode* root;
    map<int,SENode*> nodes;
    //set<SENode*> leaves;
    set<int> usrArgs;
    F_AST* formula;
    // if a node can produce a AST node, then record it here
    map<SENode*,F_AST*> cached_formula;
    // if a node's status is changed by an instruction(SENode)
    ModuleOracle& mo;
    map<SENode*,SENode*> aliased;

    const map<formalize_kind,int> mapped_kind = {
        {F_Const,0},{F_Unknown,0},{F_Alloc,0},{F_Malloc,0},
        {F_User,1},\
        {F_Para,2},{F_Glb,2},{F_Call,2},{F_Calculated,2},{F_Cmped,2}};
};