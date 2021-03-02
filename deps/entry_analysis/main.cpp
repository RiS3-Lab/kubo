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
#include "llvm/IR/Module.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <fstream>

using namespace std;
using namespace llvm;

#define NETDEV_IOCTL "NETDEVIOCTL"
#define READ_HDR "FileRead"
#define WRITE_HDR "FileWrite"
#define IOCTL_HDR "IOCTL"
#define DEVATTR_SHOW "DEVSHOW"
#define DEVATTR_STORE "DEVSTORE"
#define V4L2_IOCTL_FUNC "V4IOCTL"
#define END_HDR "ENDEND"

typedef struct {
    std::string st_name;
    int mem_id;
    std::string method_lab;
} INT_STS;

static const vector<string> _builtin_syscall_prefix ={
        "compat_SyS_",
        "compat_sys_",
        "__x64_sys",
        "__x32_compat_sys_",
        "__ia32_sys_",
        "__ia32_compat_sys_"
};
INT_STS kernel_sts[] {
        //{"struct.drm_driver",-1,IOCTL_HDR},
        //{"struct.drm_ioctl_desc",-1,IOCTL_HDR},
        {"struct.watchdog_ops", 9, IOCTL_HDR},
        // TODO:needs to be refined
        //{"struct.bin_attribute", 3, READ_HDR},
        //{"struct.bin_attribute", 4, WRITE_HDR},
        {"struct.atmdev_ops", 3, IOCTL_HDR},
        {"struct.atmphy_ops", 1, IOCTL_HDR},
        {"struct.atm_ioctl", 1, IOCTL_HDR},
        {"struct.kernfs_ops", 4, READ_HDR},
        {"struct.kernfs_ops", 6, WRITE_HDR},
        {"struct.ppp_channel_ops", 1, IOCTL_HDR},
        {"struct.hdlcdrv_ops", 4, IOCTL_HDR},
        {"struct.vfio_device_ops", 3, READ_HDR},
        {"struct.vfio_device_ops", 4, WRITE_HDR},
        {"struct.vfio_device_ops", 5, IOCTL_HDR},
        {"struct.vfio_iommu_driver_ops", 4, READ_HDR},
        {"struct.vfio_iommu_driver_ops", 5, WRITE_HDR},
        {"struct.vfio_iommu_driver_ops", 6, IOCTL_HDR},
        {"struct.net_device_ops", 10, IOCTL_HDR},
        {"struct.kvm_device_ops", 6, IOCTL_HDR},
        {"struct.ide_disk_ops", 8, IOCTL_HDR},
        {"struct.ide_ioctl_devset", 0, IOCTL_HDR},
        {"struct.ide_ioctl_devset", 1, IOCTL_HDR},
        {"struct.pci_ops", 0, READ_HDR},
        {"struct.pci_ops", 1, WRITE_HDR},
        {"struct.cdrom_device_info", 10, IOCTL_HDR},
        {"struct.cdrom_device_ops", 12, IOCTL_HDR},
        {"struct.iio_chan_spec_ext_info", 2, READ_HDR},
        {"struct.iio_chan_spec_ext_info", 3, WRITE_HDR},
        {"struct.proto_ops", 9, IOCTL_HDR},
        {"struct.usb_phy_io_ops", 0, READ_HDR},
        {"struct.usb_phy_io_ops", 1, WRITE_HDR},
        {"struct.usb_gadget_ops", 6, IOCTL_HDR},
        {"struct.uart_ops", 23, IOCTL_HDR},
        {"struct.tty_ldisc_ops", 8, READ_HDR},
        {"struct.tty_ldisc_ops", 9, WRITE_HDR},
        {"struct.tty_ldisc_ops", 10, IOCTL_HDR},
        {"struct.fb_ops", 17, IOCTL_HDR},
        {"struct.v4l2_subdev_core_ops", 13, IOCTL_HDR},
        {"struct.m2m1shot_devops", 8, IOCTL_HDR},
        {"struct.nfc_phy_ops", 0, WRITE_HDR},
        {"struct.snd_ac97_bus_ops", 2, WRITE_HDR},
        {"struct.snd_ac97_bus_ops", 3, READ_HDR},
        {"struct.snd_hwdep_ops", 1, READ_HDR},
        {"struct.snd_hwdep_ops", 2, WRITE_HDR},
        {"struct.snd_hwdep_ops", 6, IOCTL_HDR},
        {"struct.snd_hwdep_ops", 7, IOCTL_HDR},
        {"struct.snd_soc_component", 14, READ_HDR},
        {"struct.snd_soc_component", 15, WRITE_HDR},
        {"struct.snd_soc_codec_driver", 14, READ_HDR},
        {"struct.snd_soc_codec_driver", 15, WRITE_HDR},
        {"struct.snd_pcm_ops", 2, IOCTL_HDR},
        {"struct.snd_ak4xxx_ops", 2, WRITE_HDR},
        {"struct.snd_info_entry_text", 0, READ_HDR},
        {"struct.snd_info_entry_text", 1, WRITE_HDR},
        {"struct.snd_info_entry_ops", 2, READ_HDR},
        {"struct.snd_info_entry_ops", 3, WRITE_HDR},
        {"struct.snd_info_entry_ops", 6, IOCTL_HDR},
        {"struct.tty_buffer", 4, READ_HDR},
        {"struct.tty_operations", 7, WRITE_HDR},
        {"struct.tty_operations", 12, IOCTL_HDR},
        {"struct.posix_clock_operations", 10, IOCTL_HDR},
        {"struct.posix_clock_operations", 15, READ_HDR},
        {"struct.block_device_operations", 3, IOCTL_HDR},
        {"struct.security_operations", 64, IOCTL_HDR},
        {"struct.file_operations", 2, READ_HDR},
        {"struct.file_operations", 3, WRITE_HDR},
        {"struct.file_operations", 10, IOCTL_HDR},
        {"struct.efi_pci_io_protocol_access_32_t", 0, READ_HDR},
        {"struct.efi_pci_io_protocol_access_32_t", 1, WRITE_HDR},
        {"struct.efi_pci_io_protocol_access_64_t", 0, READ_HDR},
        {"struct.efi_pci_io_protocol_access_64_t", 1, WRITE_HDR},
        {"struct.efi_pci_io_protocol_access_t", 0, READ_HDR},
        {"struct.efi_pci_io_protocol_access_t", 1, WRITE_HDR},
        {"struct.efi_file_handle_32_t", 4, READ_HDR},
        {"struct.efi_file_handle_32_t", 5, WRITE_HDR},
        {"struct.efi_file_handle_64_t", 4, READ_HDR},
        {"struct.efi_file_handle_64_t", 5, WRITE_HDR},
        {"struct._efi_file_handle", 4, READ_HDR},
        {"struct._efi_file_handle", 5, WRITE_HDR},
        {"struct.video_device", 21, IOCTL_HDR},
        {"struct.video_device", 22, IOCTL_HDR},
        {"struct.v4l2_file_operations", 1, READ_HDR},
        {"struct.v4l2_file_operations", 2, WRITE_HDR},
        {"struct.v4l2_file_operations", 4, IOCTL_HDR},
        {"struct.v4l2_file_operations", 5, IOCTL_HDR},
        {"struct.media_file_operations", 1, READ_HDR},
        {"struct.media_file_operations", 2, WRITE_HDR},
        {"struct.media_file_operations", 4, IOCTL_HDR},
        {"END", 0, END_HDR}
};

void print_err(char *prog_name) {
    std::cerr << "[!] This program identifies all the entry points from the provided bitcode file.\n";
    std::cerr << "[!] saves these entry points into provided output file, which could be used to run analysis on.\n";
    std::cerr << "[?] " << prog_name << " <llvm_linked_bit_code_file> <output_txt_file>\n";
    exit(-1);
}

void fprintf_mock(FILE* outputFile,string fmt, string ioctl_type, string func_name){
    fprintf(outputFile, fmt.c_str(),ioctl_type.c_str(),func_name.c_str());
}
void process_netdev_st(GlobalVariable *currGlobal, FILE *outputFile) {

    if(currGlobal->hasInitializer()) {
        // get the initializer.
        Constant *targetConstant = currGlobal->getInitializer();
        ConstantStruct *actualStType = dyn_cast<ConstantStruct>(targetConstant);

        // net device ioctl: 10
        Value *currFieldVal = actualStType->getOperand(10);
        Function *targetFunction = dyn_cast<Function>(currFieldVal);

        if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
            fprintf_mock(outputFile, "%s:%s\n", NETDEV_IOCTL, targetFunction->getName().str());
        }
    }
}

void process_device_attribute_st(GlobalVariable *currGlobal, FILE *outputFile) {

    if(currGlobal->hasInitializer()) {

        // get the initializer.
        Constant *targetConstant = currGlobal->getInitializer();
        ConstantStruct *actualStType = dyn_cast<ConstantStruct>(targetConstant);

        // dev show: 1
        Value *currFieldVal = actualStType->getOperand(1);
        Function *targetFunction = dyn_cast<Function>(currFieldVal);

        if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
            fprintf_mock(outputFile, "%s:%s\n", DEVATTR_SHOW, targetFunction->getName().str());
        }

        // dev store : 2
        currFieldVal = actualStType->getOperand(2);
        targetFunction = dyn_cast<Function>(currFieldVal);
        if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
            fprintf_mock(outputFile, "%s:%s\n", DEVATTR_STORE, targetFunction->getName().str());
        }

    }
}

void process_file_operations_st(GlobalVariable *currGlobal, FILE *outputFile) {
    if(currGlobal->hasInitializer()) {
        // get the initializer.
        Constant *targetConstant = currGlobal->getInitializer();
        ConstantStruct *file_op_struct = dyn_cast<ConstantStruct>(targetConstant);
        if(file_op_struct == nullptr)
            return;
        int numOperands = file_op_struct->getNumOperands();
        Value *currFieldVal = nullptr;
        Function *targetFunction = nullptr;

        if (numOperands >= 3){
            // read : 2
            Value *currFieldVal = file_op_struct->getOperand(2);
            Function *targetFunction = dyn_cast<Function>(currFieldVal);
            if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
                fprintf_mock(outputFile, "%s:%s\n", READ_HDR, targetFunction->getName().str());
            }
        }
        if(numOperands >= 4){
            // write: 3
            currFieldVal = file_op_struct->getOperand(3);
            targetFunction = dyn_cast<Function>(currFieldVal);
            if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
                fprintf_mock(outputFile, "%s:%s\n", WRITE_HDR, targetFunction->getName().str());
            }
        }
        if(numOperands >= 11){
            // ioctl : 10
            currFieldVal = file_op_struct->getOperand(10);
            targetFunction = dyn_cast<Function>(currFieldVal);
            if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
                fprintf_mock(outputFile, "%s:%s\n", IOCTL_HDR, targetFunction->getName().str());
            }
        }
    }
}

void process_snd_pcm_ops_st(GlobalVariable *currGlobal, FILE *outputFile) {
    if(currGlobal->hasInitializer()) {
        // get the initializer.
        Constant *targetConstant = currGlobal->getInitializer();
        ConstantStruct *nd_pcm_op_struct = dyn_cast<ConstantStruct>(targetConstant);

        // ioctl: 2
        Value *currFieldVal = nd_pcm_op_struct->getOperand(2);
        Function *targetFunction = dyn_cast<Function>(currFieldVal);

        if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
            fprintf_mock(outputFile, "%s:%s\n", IOCTL_HDR, targetFunction->getName().str());
        }

    }
}

void process_v4l2_ioctl_st(GlobalVariable *currGlobal, FILE *outputFile) {
    if(currGlobal->hasInitializer()) {

        // get the initializer.
        Constant *targetConstant = currGlobal->getInitializer();
        ConstantStruct *v4l2_ioctl_struct = dyn_cast<ConstantStruct>(targetConstant);

        // all fields are function pointers
        for(unsigned int i=0; i<v4l2_ioctl_struct->getNumOperands(); i++) {
            Value *currFieldVal = v4l2_ioctl_struct->getOperand(i);
            Function *targetFunction = dyn_cast<Function>(currFieldVal);
            if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
                fprintf_mock(outputFile, "%s:%s\n", V4L2_IOCTL_FUNC, targetFunction->getName().str());
            }
        }

    }
}

void process_v4l2_file_ops_st(GlobalVariable *currGlobal, FILE *outputFile) {
    if(currGlobal->hasInitializer()) {
        // get the initializer.
        Constant *targetConstant = currGlobal->getInitializer();
        ConstantStruct *v4l2_file_ops_struct = dyn_cast<ConstantStruct>(targetConstant);

        // read: 1
        Value *currFieldVal = v4l2_file_ops_struct->getOperand(1);
        Function *targetFunction = dyn_cast<Function>(currFieldVal);

        if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
            fprintf_mock(outputFile, "%s:%s\n", READ_HDR, targetFunction->getName().str());
        }

        // write: 2
        currFieldVal = v4l2_file_ops_struct->getOperand(2);
        targetFunction = dyn_cast<Function>(currFieldVal);
        if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
            fprintf_mock(outputFile, "%s:%s\n", WRITE_HDR, targetFunction->getName().str());
        }

    }
}

char** str_split(char* a_str, const char a_delim)
{
    char** result    = 0;
    size_t count     = 0;
    char* tmp        = a_str;
    char* last_comma = 0;
    char delim[2];
    delim[0] = a_delim;
    delim[1] = 0;

    /* Count how many elements will be extracted. */
    while (*tmp)
    {
        if (a_delim == *tmp)
        {
            count++;
            last_comma = tmp;
        }
        tmp++;
    }

    /* Add space for trailing token. */
    count += last_comma < (a_str + strlen(a_str) - 1);

    /* Add space for terminating null string so caller
       knows where the list of returned strings ends. */
    count++;

    result = (char**)malloc(sizeof(char*) * count);

    if (result)
    {
        size_t idx  = 0;
        char* token = strtok(a_str, delim);

        while (token)
        {
            assert(idx < count);
            *(result + idx++) = strdup(token);
            token = strtok(0, delim);
        }
        assert(idx == count - 1);
        *(result + idx) = 0;
    }

    return result;
}

bool process_struct_in_custom_entry_files(GlobalVariable *currGlobal, FILE *outputFile,
                                          std::vector<string> &allentries) {
    bool retVal = false;
    if(currGlobal->hasInitializer()) {
        // get the initializer.
        Constant *targetConstant = currGlobal->getInitializer();
        ConstantStruct *actualStType = dyn_cast<ConstantStruct>(targetConstant);
        Type *targetType = currGlobal->getType();
        assert(targetType->isPointerTy());
        Type *containedType = targetType->getContainedType(0);
        std::string curr_st_name = containedType->getStructName();
        char hello_str[1024];
        for(auto curre:allentries) {
            if(curre.find(curr_st_name) != std::string::npos) {
                strcpy(hello_str, curre.c_str());
                // structure found
                char** tokens = str_split(hello_str, ',');
                assert(!strcmp(curr_st_name.c_str(), tokens[0]));
                long ele_ind = strtol(tokens[1], NULL, 10);
                Value *currFieldVal = actualStType->getOperand(ele_ind);
                Function *targetFunction = dyn_cast<Function>(currFieldVal);

                if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
                    fprintf_mock(outputFile, "%s:%s\n", tokens[2], targetFunction->getName().str());
                }
                if (tokens)
                {
                    int i;
                    for (i = 0; *(tokens + i); i++)
                    {
                        free(*(tokens + i));
                    }
                    free(tokens);
                }
                retVal = true;
            }
        }

    }
    return retVal;
}
void check_if_syscall(GlobalVariable *currGlobal, FILE *outputFile, std::vector<string> &allentries) {

}
void process_global(GlobalVariable *currGlobal, FILE *outputFile, std::vector<string> &allentries) {
    std::string file_op_st("struct.file_operations");
    std::string dev_attr_st("struct.device_attribute");
    std::string dri_attr_st("struct.driver_attribute");
    std::string bus_attr_st("struct.bus_attribute");
    std::string net_dev_st("struct.net_device_ops");
    std::string snd_pcm_st("struct.snd_pcm_ops");
    std::string v4l2_ioctl_st("struct.v4l2_ioctl_ops");
    std::string v4l2_file_ops_st("struct.v4l2_file_operations");


    Type *targetType = currGlobal->getType();
    assert(targetType->isPointerTy());
    Type *containedType = targetType->getContainedType(0);
    if (containedType->isStructTy()) {
        StructType *targetSt = dyn_cast<StructType>(containedType);
        if(targetSt->isLiteral()) {
            return;
        }
        if(process_struct_in_custom_entry_files(currGlobal, outputFile, allentries)) {
            return;
        }
        //if (containedType->getStructName() == file_op_st) {
        if(containedType->getStructName().str().find(file_op_st) == 0){
            process_file_operations_st(currGlobal, outputFile);
        } else if(containedType->getStructName().str().find(dev_attr_st) == 0 || containedType->getStructName().str().find(dri_attr_st) == 0) {
            process_device_attribute_st(currGlobal, outputFile);
        } else if(containedType->getStructName().str().find(net_dev_st) == 0) {
            process_netdev_st(currGlobal, outputFile);
        } else if(containedType->getStructName().str().find(snd_pcm_st) == 0) {
            process_snd_pcm_ops_st(currGlobal, outputFile);
        } else if(containedType->getStructName().str().find(v4l2_file_ops_st) == 0) {
            process_v4l2_file_ops_st(currGlobal, outputFile);
        } else if(containedType->getStructName().str().find(v4l2_ioctl_st) == 0) {
            process_v4l2_ioctl_st(currGlobal, outputFile);
        } else {
            unsigned long i=0;
            while(kernel_sts[i].method_lab != END_HDR) {
                if(containedType->getStructName().str().find(kernel_sts[i].st_name) == 0) {
                    if(currGlobal->hasInitializer()) {
                        Constant *targetConstant = currGlobal->getInitializer();
                        ConstantStruct *actualStType = dyn_cast<ConstantStruct>(targetConstant);
                        if(actualStType==nullptr){
                            i++;
                            continue;
                        }
                        int numOperands = actualStType->getNumOperands();
                        if(numOperands >= (kernel_sts[i].mem_id + 1)){
                            Value *currFieldVal = actualStType->getOperand(kernel_sts[i].mem_id);
                            Function *targetFunction = dyn_cast<Function>(currFieldVal);

                            if(targetFunction != nullptr && !targetFunction->isDeclaration()) {
                                fprintf_mock(outputFile, "%s:%s\n", kernel_sts[i].method_lab.c_str(), targetFunction->getName().str());
                            }
                        }

                    }
                }
                i++;
            }
        }
    }
}

int main(int argc, char *argv[]) {
    //check args
    if(argc < 3) {
        print_err(argv[0]);
    }

    char *src_llvm_file = argv[1];
    char *output_txt_file = argv[2];
    char *entry_point_file = NULL;
    std::vector<string> entryPointStrings;
    entryPointStrings.clear();
    if(argc > 3) {
        entry_point_file = argv[3];
        std::ifstream infile(entry_point_file);
        std::string line;
        while (std::getline(infile, line)) {
            entryPointStrings.push_back(line);
        }
    }



    FILE *outputFile = fopen(output_txt_file, "w");
    assert(outputFile != nullptr);

    LLVMContext context;
    ErrorOr<std::unique_ptr<MemoryBuffer>> fileOrErr = MemoryBuffer::getFileOrSTDIN(src_llvm_file);

    Expected<std::unique_ptr<llvm::Module> > moduleOrErr = parseBitcodeFile(fileOrErr.get()->getMemBufferRef(), context);

    Module * m = moduleOrErr.get().get();

    // find ioctl function
    Module::GlobalListType &currGlobalList = m->getGlobalList();
    for(Module::global_iterator gstart = currGlobalList.begin(), gend = currGlobalList.end(); gstart != gend; gstart++) {
        GlobalVariable *currGlobal = &(*gstart);
        process_global(currGlobal, outputFile, entryPointStrings);
    }
    // find syscall functions
    for(Module::iterator F_it = m->begin(); F_it != m->end() ; F_it++){
        StringRef funcName = (*F_it).getName();
        bool foundSyscall = false;
        for(auto each_syscall_prefix = _builtin_syscall_prefix.begin(); each_syscall_prefix != _builtin_syscall_prefix.end(); each_syscall_prefix++){
            if (funcName.startswith(*(each_syscall_prefix))){
                foundSyscall = true;
                break;
            }
        }
        if(foundSyscall){
            fprintf_mock(outputFile, "%s:%s\n", "is syscall", funcName.str());
        }
    }
    
    fclose(outputFile);
}
