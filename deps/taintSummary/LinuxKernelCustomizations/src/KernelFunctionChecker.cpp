//
// Created by machiry on 12/4/16.
//
#include "KernelFunctionChecker.h"
using namespace llvm;

namespace DRCHECKER {

    // These are allocators
    const std::set<std::string> KernelFunctionChecker::known_allocators{"__kmalloc", "kmem_cache_alloc",
                                                                        "mempool_alloc",
                                                                        "__get_free_pages", "get_free_pages",
                                                                        "__get_free_page", "get_free_page",
                                                                        "__vmalloc", "vmalloc",
                                                                        "alloc_percpu", "__alloc_percpu",
                                                                        "alloc_bootmem"};

    // these are initializers
    const std::set<std::string> KernelFunctionChecker::zero_initializers{"__kmalloc"};
    const std::set<std::string> KernelFunctionChecker::memset_function_names{"memset"};
    // copy to user function.
    const std::set<std::string> KernelFunctionChecker::copy_out_function_names{"__copy_to_user", "copy_to_user"};
    // init functions
    const std::set<std::string> KernelFunctionChecker::init_section_names{".init.text"};
    // memcpy functions: for points to and taint propagation.
    const std::set<std::string> KernelFunctionChecker::memcpy_function_names{"llvm.memcpy", "strcpy", "strncpy",
                                                                             "strcat", "strncat", "strlcpy",
                                                                             "strlcat"};

    const std::set<std::string> KernelFunctionChecker::atoiLikeFunctions{"kstrto", "simple_strto"};

    bool KernelFunctionChecker::is_debug_function(const Function *targetFunction) {
        if(targetFunction->hasName()) {
            std::string currFuncName = targetFunction->getName().str();
            if(currFuncName.find("llvm.dbg") != std::string::npos) {
                return true;
            }

        }
        return false;
    }

    bool KernelFunctionChecker::is_init_function(const Function *targetFunction) {
        if(!targetFunction->isDeclaration()) {
            for(const std::string &curr_sec:KernelFunctionChecker::init_section_names) {
                if(!(targetFunction->getSection().empty()) && strlen(targetFunction->getSection().str().c_str()) &&
                   (curr_sec.find(targetFunction->getSection()) != std::string::npos)) {
                    return true;
                }
            }
        }
        return false;
    }

    bool KernelFunctionChecker::is_copy_out_function(const Function *targetFunction) {
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string func_name = targetFunction->getName().str();
            for (const std::string &curr_func:KernelFunctionChecker::copy_out_function_names) {
                if (func_name.find(curr_func.c_str()) != std::string::npos) {
                    return true;
                }
            }
        }
        return false;
    }

    bool KernelFunctionChecker::is_kmalloc_function(const Function *targetFunction) {
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string func_name = targetFunction->getName().str();
            for (const std::string &curr_func:KernelFunctionChecker::zero_initializers) {
                if (func_name.find(curr_func.c_str()) != std::string::npos) {
                    return true;
                }
            }
        }
        return false;
    }

    bool KernelFunctionChecker::is_memset_function(const Function *targetFunction) {
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string func_name = targetFunction->getName().str();
            for (const std::string &curr_func:KernelFunctionChecker::memset_function_names) {
                if (func_name.find(curr_func.c_str()) != std::string::npos) {
                    return true;
                }
            }
        }
        return false;
    }
    
    bool KernelFunctionChecker::is_function_allocator(const Function *targetFunction) {
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string func_name = targetFunction->getName().str();
            for (const std::string &curr_func:KernelFunctionChecker::known_allocators) {
                if (func_name.find(curr_func.c_str()) != std::string::npos) {
                    return true;
                }
            }
        }
        return false;
    }

    bool KernelFunctionChecker::is_custom_function(const Function *targetFunction) {
        // is this a kernel function and returns a pointer?
        return targetFunction->isDeclaration() && targetFunction->getReturnType()->isPointerTy();
    }

    bool KernelFunctionChecker::is_memcpy_function(const Function *targetFunction) {
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string func_name = targetFunction->getName().str();
            for (const std::string &curr_func:KernelFunctionChecker::memcpy_function_names) {
                if (func_name.find(curr_func.c_str()) != std::string::npos) {
                    return true;
                }
            }
        }
        return false;
    }

    std::vector<long> KernelFunctionChecker::get_memcpy_arguments(const Function *targetFunction) {
        std::vector<long> memcpy_args;
        if(this->is_memcpy_function(targetFunction)) {
            // src argument is the second parameter
            memcpy_args.push_back(1);
            // dst argument is the first parameter
            memcpy_args.push_back(0);
            return memcpy_args;
        }
        // should never reach here..make sure that you call is_memcpy_function function
        // before this.
        assert(false);
        return memcpy_args;
    }

    bool KernelFunctionChecker::is_taint_initiator(const Function *targetFunction) {
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string func_name = targetFunction->getName().str();
            bool isTaintInitiator = (func_name.find("copy_from_user") != std::string::npos) || (func_name == "simple_write_to_buffer") || (func_name.find("__copy_user_" ) != std::string::npos);
            return isTaintInitiator;
        }
        return false;
    }

    std::set<long> KernelFunctionChecker::get_tainted_arguments(const Function *targetFunction) {
        std::set<long> tainted_args;
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string func_name = targetFunction->getName().str();
            if(func_name.find("copy_from_user") != std::string::npos ||
               func_name == "simple_write_to_buffer" || func_name.find("__copy_user_" ) != std::string::npos) {
                // first argument will get tainted.
                tainted_args.insert(tainted_args.end(), 0);
                return tainted_args;
            }

        }
        // should never reach here..make sure that you call is_taint_initiator function
        // before this.
        assert(false);
        return tainted_args;
    }

    bool KernelFunctionChecker::is_atoi_function(const Function *targetFunction) {
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string funcName = targetFunction->getName().str();
            for (const std::string &curr_func:KernelFunctionChecker::atoiLikeFunctions) {
                if(funcName.compare(0, curr_func.length(), curr_func) == 0) {
                    return true;
                }
            }
        }
        return false;
    }

    bool KernelFunctionChecker::is_sscanf_function(const Function *targetFunction) {
        std::string sscanf_func("sscanf");
        if(targetFunction->isDeclaration() && targetFunction->hasName()) {
            std::string funcName = targetFunction->getName().str();
            return funcName.compare(0, sscanf_func.length(), sscanf_func) == 0;
        }
        return false;

    }
    
}

