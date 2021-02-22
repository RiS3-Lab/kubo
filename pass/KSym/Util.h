#ifndef UTIL_H_
#define UTIL_H_

#include "Project.h"
#include<iostream>
#include<fstream>
class Helper {
  public:
    // LLVM value
    static string getValueName(Value *v);
    static string getValueType(Value *v);
    static string getValueRepr(Value *v);
    static string getCtypeRepr(Type *t);

    // Z3 expr
    static string getExprType(Z3_context ctxt, Z3_ast ast);
    static string getExprRepr(Z3_context ctxt, Z3_ast ast);

    // string conversion
    static void convertDotInName(string &name);
};

class Dumper {
  public:
    Dumper() {}
    ~Dumper() {}

    // LLVM value
    void valueName(Value *val);
    void typedValue(Value *val);
    void ctypeValue(Value *val);

    // Z3 expr
    void typedExpr(Z3_context ctxt, Z3_ast ast);
};

class FileWriter {
public:
    FileWriter(string filename);

    ~FileWriter();

    void writeLine(int64_t i);

    void writeLine(string s);


private:
    ofstream *out;
    error_code EC;
    string filename;
};
class FileReader{
public:
    FileReader(string filename);
    ~FileReader();
    bool readLine(string * str);
    bool ok();
private:
    ifstream  *infile;
    error_code EC;
    string filename;
};

bool isSanitizerBlock(BasicBlock &BB);

CallInst *getSanHandlerAsSeed(BranchInst *bi);

static inline bool starts_with(const std::string& str, const std::string& prefix)
{
    if(prefix.length() > str.length()) { return false; }
    return str.substr(0, prefix.length()) == prefix;
}

string get_constant_subclass(Constant* con);
#endif /* UTIL_H_ */
