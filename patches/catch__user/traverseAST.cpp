//#include <iostream>
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

using namespace llvm;
using namespace clang;
using namespace clang::tooling;
using namespace std;

#include <iostream>
#include <fstream>
#include <string>
ofstream  *out;
static llvm::cl::extrahelp
    CommonHelp(clang::tooling::CommonOptionsParser::HelpMessage);
llvm::cl::OptionCategory FindDeclCategory("find-decl options");

class FindNamedCallVisitor : public RecursiveASTVisitor<FindNamedCallVisitor> {
 public:
  explicit FindNamedCallVisitor(ASTContext *Context, std::string fName)
      : Context(Context), fName(fName) {}

  bool VisitFunctionDecl(FunctionDecl *func)
  {
    // *out << "func:"<< func->getNameInfo().getAsString()<<"\n";
    for(int i=0; i<func->getNumParams(); i++)
    {
      string tyStr = func->parameters()[i]->getOriginalType().getAsString();
      //*out << to_string(i)<<'|'<<func->parameters()[i]->getOriginalType().getAsString()<<'\n';
      //*out << to_string(i)<<'|'<<func->parameters()[i]->getQualifiedNameAsString()<<'\n';
      if(tyStr.find("__user") != string::npos){
        *out << "func:"<< func->getNameInfo().getAsString()<<"\n";
        *out << to_string(i)<<'|'<<func->parameters()[i]->getQualifiedNameAsString()<<'\n';
      }
    }
    return true;
  }

 private:
  ASTContext *Context;
  std::string fName;
};

class FindNamedCallConsumer : public clang::ASTConsumer {
 public:
  explicit FindNamedCallConsumer(ASTContext *Context, std::string fName)
      : Visitor(Context, fName) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

 private:
  FindNamedCallVisitor Visitor;
};

class FindNamedCallAction : public clang::ASTFrontendAction {
 public:
  FindNamedCallAction() {}

  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    const std::string fName = "doSomething";
    return std::unique_ptr<clang::ASTConsumer>(
        new FindNamedCallConsumer(&Compiler.getASTContext(), fName));
  }
};


int main(int argc, const char **argv) {
  const std::string fName = "doSomething";
  //int realArgc = argc - 1;
  string outputfilename(argv[argc-1]);
  //out = new ofstream("/home/changming/newLinux/symslice/work/static.txt",ios::out | ios::app);
  out = new ofstream(outputfilename,ios::out);
  *out<<"module:"<<outputfilename<<"\n";
  int real_argc = argc - 1;
  CommonOptionsParser OptionsParser(real_argc, argv, FindDeclCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  // run the Clang Tool, creating a new FrontendAction (explained below)
  int result = Tool.run(newFrontendActionFactory<FindNamedCallAction>().get());
  out->close();
  return result;
}
