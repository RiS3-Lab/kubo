//
// Created by machiry on 1/30/17.
//

#include <iostream>
#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Analysis/CFGPrinter.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/DebugInfoMetadata.h"

#include "llvm/IR/Module.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <fstream>
#include <string>
using namespace std;
using namespace llvm;


int main(int argc, char *argv[]) {

    char *src_llvm_file = argv[1];
    char *output_txt_file = argv[2];
    char *target_func_name = argv[3];
    char * func_id_string = argv[4];
    char * blk_id_string = argv[5];
    char * inst_id_string = argv[6];
    string tar_func_str(target_func_name);

    int t_func_id = atoi(func_id_string);
    int t_blk_id = atoi(blk_id_string);
    int t_inst_id = atoi(inst_id_string);

    FILE *outputFile = fopen(output_txt_file, "w");
    assert(outputFile != nullptr);

    LLVMContext context;
    ErrorOr<std::unique_ptr<MemoryBuffer>> fileOrErr = MemoryBuffer::getFileOrSTDIN(src_llvm_file);

    Expected<std::unique_ptr<llvm::Module> > moduleOrErr = parseBitcodeFile(fileOrErr.get()->getMemBufferRef(), context);

    Module * m = moduleOrErr.get().get();

    int func_id = 0;
    int blk_id = 0;
    int inst_id = 0;

    // find syscall functions
    for(Module::iterator func_it = m->begin(); func_it != m->end() ; func_it++,func_id++){
        string funcName = (*func_it).getName().str();
        if(tar_func_str == funcName){
            //assert(func_id == t_func_id);

            blk_id = 0;
            for(auto blk_it = func_it->begin();blk_it != func_it->end(); blk_it++, blk_id++){
                if(blk_id != t_blk_id){
                    continue;
                }

                inst_id = 0;
                for(auto inst_it = blk_it->begin(); inst_it != blk_it->end(); inst_it++, inst_id++){
                    if(inst_id != t_inst_id){
                        continue;
                    }
                    string seed_str;
                    raw_string_ostream os (seed_str);

                    CallInst * call_inst = cast<CallInst>(&*(inst_it));
                    const DebugLoc & dbg = call_inst->getDebugLoc();

                    DILocation * dbg_loc = dbg.get();
                    call_inst->print(os);
                    if(dbg_loc != nullptr){
                        string file_name = dbg_loc->getFilename();
                        string sub_program_name = dbg_loc->getScope()->getSubprogram()->getName().str();
                        unsigned int line_num = dbg_loc->getLine();
                        fprintf(outputFile, "%s\n%s|%s|%d\n",seed_str.c_str(),file_name.c_str(),sub_program_name.c_str(),line_num);
                    }else{
                        fprintf(outputFile, "%s\n",seed_str.c_str());
                    }
                }
            }

            string func_body;
            raw_string_ostream os (func_body);
            (*func_it).print(os);
            fprintf(outputFile, "%s\n",func_body.c_str());
            break;
        }


    }
    
    fclose(outputFile);
}
