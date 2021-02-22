//===----- CHA.cpp  Base class of pointer analyses ---------------------------//
//
//                     SVF: Static Value-Flow Analysis
//
// Copyright (C) <2013-2017>  <Yulei Sui>
//

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//===----------------------------------------------------------------------===//

/*
 * CHA.cpp
 *
 *  Created on: Apr 13, 2016
 *      Author: Xiaokang Fan
 */

#include <set>
#include <vector>
#include <map>
#include <fstream>
#include <iostream>
#include <iomanip>   // setw() for formatting cout
#include "Util/CPPUtil.h"
#include <assert.h>
#include <stack>
#include "MemoryModel/CHA.h"
#include <llvm/Support/CommandLine.h>
#include <llvm/IR/DebugInfo.h> // for debuginfo like DILocation
#include <llvm/Support/DOTGraphTraits.h>	// for dot graph traits
#include <llvm/IR/InstIterator.h> // for inst_iterator
#include "Util/GraphUtil.h"
#include "Util/AnalysisUtil.h"
#include "Util/SVFModule.h"

using namespace llvm;
using namespace cppUtil;
using namespace std;

static cl::opt<bool>
dumpGraph("dump-cha-graph", cl::init(false),
          cl::desc("dump the class hierarchy graph"));

const string pureVirtualFunName = "__cxa_pure_virtual";

const string ztiLabel = "_ZTI";

static bool hasEdge(const CHNode *src, const CHNode *dst,
                    CHEdge::CHEDGETYPE et) {
    for (set<CHEdge*>::const_iterator it = src->getOutEdges().begin(),
            eit = src->getOutEdges().end(); it != eit; ++it) {
        CHNode *node = (*it)->getDstNode();
        CHEdge::CHEDGETYPE edgeType = (*it)->getEdgeType();
        if (node == dst && edgeType == et)
            return true;
    }
    return false;
}

void CHNode::getVirtualFunctions(u32_t idx,
                                 set<const Function*> &virtualFunctions) const {
    vector<vector<const Function*>>::const_iterator it, eit;
    for (it = virtualFunctionVectors.begin(),
            eit = virtualFunctionVectors.end(); it != eit; ++it) {
        if ((*it).size() > idx) {
            virtualFunctions.insert((*it)[idx]);
        }
    }
}

CHGraph::~CHGraph() {
    for (CHGraph::iterator it = this->begin(), eit = this->end();
            it != eit; ++it) {
        CHNode *node = it->second;
        delete node;
    }
}

void CHGraph::buildCHG(SVFModule svfModule) {
    double timeStart, timeEnd;
    timeStart = CLOCK_IN_MS();

    svfMod = svfModule;
    u32_t moduleNum = svfModule.getModuleNum();
    for (u32_t i = 0; i < moduleNum; ++i) {
        Module *m = svfModule.getModule(i);
        if (m == NULL)
            continue;
        DBOUT(DGENERAL, outs() << analysisUtil::pasMsg("construct CHGraph From lib " + m->getName().str() + " [" + to_string(i+1) + " of " + to_string(moduleNum) + "]...\n"));
        constructCHGraphFromIR(*m);
    }

    DBOUT(DGENERAL, outs() << analysisUtil::pasMsg("build Internal Maps ...\n"));
    buildInternalMaps();

    timeEnd = CLOCK_IN_MS();
    buildingCHGTime = (timeEnd - timeStart)/TIMEINTERVAL;
}

void CHGraph::constructCHGraphFromIR(const Module &M) {
    readInheritanceMetadataFromModule(M);

    for (Module::const_iterator F = M.begin(), E = M.end(); F != E; ++F) {
        const Function &function = *F;
        buildCHGOnFunction(&function);
    }

    analyzeVTables(M);
}

void CHGraph::buildInternalMaps() {
    buildTemplateNameToInstancesMap();
    buildClassNameToAncestorsDescendantsMap();
    buildClassNameToNamesMap();
    buildArgsizeToVFunMap();
    buildVirtualFunctionToIDMap();

    collectVirtualCallSites();
    buildCSToCHAVtblsAndVfnsMap();
}

void CHGraph::buildCHGOnFunction(const Function *F) {
    if (F->isDeclaration()) {
        return;
    } else if (isConstructor(F)) {
        struct DemangledName dname = demangle(F->getName().str());
        for (Function::const_iterator B = F->begin(), E = F->end(); B != E; ++B)  {
            const BasicBlock &bb = *B;
            buildCHGOnBasicBlock(&bb, dname.className, CONSTRUCTOR);
        }
    }
    else if (isDestructor(F)) {
        struct DemangledName dname = demangle(F->getName().str());
        for (Function::const_iterator B = F->begin(), E = F->end(); B != E; ++B)  {
            const BasicBlock &bb = *B;
            buildCHGOnBasicBlock(&bb, dname.className, DESTRUCTOR);
        }
    } else {
        return;
    }
}

void CHGraph::buildCHGOnBasicBlock(const BasicBlock *B,
                                   const string className,
                                   RELATIONTYPE relationType) {
    for (BasicBlock::const_iterator I = B->begin(), E = B->end(); I != E; ++I) {
        if (isa<CallInst>(I) || isa<InvokeInst>(I)) {
            const Instruction &instruction = *I;
            CallSite cs = analysisUtil::getLLVMCallSite(&instruction);
            const Function *callee = analysisUtil::getCallee(cs);
            if (callee == NULL)
                continue;
            if ((relationType == CONSTRUCTOR && isConstructor(callee)) ||
                    (relationType == DESTRUCTOR && isDestructor(callee))) {
                if (cs.arg_size() < 1 || (cs.arg_size() < 2 && cs.paramHasAttr(0, Attribute::StructRet)))
                    continue;
                const Value *thisPtr = getVCallThisPtr(cs);
                if (thisPtr != NULL) {
                    struct DemangledName dname = demangle(callee->getName().str());
                    if (!isa<CallInst>(thisPtr) && !isa<InvokeInst>(thisPtr) &&
                            dname.className.size() > 0) {
                        addEdge(className, dname.className, CHEdge::INHERITANCE);
                    }
                }
            }
        }
    }

    if (relationType == DESTRUCTOR) {
        for (BasicBlock::const_iterator I = B->begin(), E = B->end(); I != E; ++I) {
            const Instruction *inst = &*I;
            if (const StoreInst *storeInst = dyn_cast<StoreInst>(inst)) {
                const Value *val = storeInst->getValueOperand();
                if (const ConstantExpr *ce = dyn_cast<ConstantExpr>(val)) {
                    u32_t opcode = ce->getOpcode();
                    if (opcode == Instruction::BitCast) {
                        const Value *bitcastval = ce->getOperand(0);
                        if (const ConstantExpr *bcce = dyn_cast<ConstantExpr>(bitcastval)) {
                            u32_t bcopcode = bcce->getOpcode();
                            if (bcopcode == Instruction::GetElementPtr) {
                                const Value *gepval = bcce->getOperand(0);
                                if (isValVtbl(gepval)) {
                                    string vtblClassName = getClassNameFromVtblObj(gepval);
                                    if (vtblClassName.size() > 0 &&
                                            className.compare(vtblClassName) != 0) {
                                        addEdge(className, vtblClassName, CHEdge::INHERITANCE);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

void CHGraph::readInheritanceMetadataFromModule(const Module &M) {
    for (Module::const_named_metadata_iterator mdit = M.named_metadata_begin(),
            mdeit = M.named_metadata_end(); mdit != mdeit; ++mdit) {
        const NamedMDNode *md = &*mdit;
        string mdname = md->getName().str();
        if (mdname.compare(0, 15, "__cxx_bases_of_") != 0)
            continue;
        string className = mdname.substr(15);
        for (NamedMDNode::const_op_iterator opit = md->op_begin(),
                opeit = md->op_end(); opit != opeit; ++opit) {
            const MDNode *N = *opit;
            MDString *mdstr = cast<MDString>(N->getOperand(0));
            string baseName = mdstr->getString().str();
            addEdge(className, baseName, CHEdge::INHERITANCE);
        }
    }
}

void CHGraph::addEdge(const string className, const string baseClassName,
                      CHEdge::CHEDGETYPE edgeType) {
    CHNode *srcNode = getOrCreateNode(className);
    CHNode *dstNode = getOrCreateNode(baseClassName);

    if (!hasEdge(srcNode, dstNode, edgeType)) {
        CHEdge *edge = new CHEdge(srcNode, dstNode, edgeType);
        srcNode->addOutgoingEdge(edge);
        dstNode->addIncomingEdge(edge);
    }
}

CHNode *CHGraph::getNode(const string name) const {
    for (CHGraph::const_iterator it = this->begin(), eit = this->end();
            it != eit; ++it) {
        CHNode *node = it->second;
        if (node->getName() == name) {
            return node;
        }
    }
    return NULL;
}

CHNode *CHGraph::getOrCreateNode(const std::string name) {
    CHNode *node = getNode(name);
    if (node == NULL) {
        node = new CHNode(name, classNum++);
        addToNodeList(node);
    }
    return node;
}

void CHGraph::addToNodeList(CHNode *node) {
    addGNode(node->getId(), node);
    string className = node->getName();
    if (className.size() > 0 && className[className.size() - 1] == '>') {
        string templateName = getBeforeBrackets(className);
        getOrCreateNode(templateName);
    }
}

// collect ancestors and descendants for a given CHNode
void CHGraph::collectAncestorsDescendants(const CHNode *node) {
    stack<const CHNode*> nodeStack;
    CHNodeSetTy visitedNodes, ancestors, descendants;

    // ancestors
    nodeStack.push(node);
    while (!nodeStack.empty()) {
        const CHNode *curnode = nodeStack.top();
        nodeStack.pop();
        ancestors.insert(curnode);
        if (visitedNodes.find(curnode) == visitedNodes.end()) {
            for (set<CHEdge*>::const_iterator it = curnode->getOutEdges().begin(),
                    eit = curnode->getOutEdges().end(); it != eit; ++it) {
                CHNode *node = (*it)->getDstNode();
                if ((*it)->getEdgeType() == CHEdge::INHERITANCE)
                    nodeStack.push(node);
            }
            visitedNodes.insert(curnode);
        }
    }
    ancestors.erase(node);

    // clear visitedNodes;
    visitedNodes.clear();

    // descendants
    nodeStack.push(node);
    while (!nodeStack.empty()) {
        const CHNode *curnode = nodeStack.top();
        nodeStack.pop();
        descendants.insert(curnode);
        if (visitedNodes.find(curnode) == visitedNodes.end()) {
            for (set<CHEdge*>::const_iterator it = curnode->getInEdges().begin(),
                    eit = curnode->getInEdges().end(); it != eit; ++it) {
                CHNode *node = (*it)->getSrcNode();
                if ((*it)->getEdgeType() == CHEdge::INHERITANCE)
                    nodeStack.push(node);
            }
            visitedNodes.insert(curnode);
        }
    }
    descendants.erase(node);

    string className = node->getName();
    classNameToAncestorsMap[className] = ancestors;
    classNameToDescendantsMap[className] = descendants;
}

/*
 * build the following two maps:
 * classNameToDescendantsMap
 * classNameToAncestorsMap
 */
void CHGraph::buildClassNameToAncestorsDescendantsMap() {
    for (CHGraph::const_iterator it = this->begin(), eit = this->end();
            it != eit; ++it) {
        CHNode *node = it->second;
        collectAncestorsDescendants(node);
    }
}

/*
 * build the following three maps:
 * classNameToAncestorsMap
 * classNameToInstancesMap
 * classNameToDescendantsMap
 */
void CHGraph::buildClassNameToNamesMap() {
    for (CHGraph::const_iterator it = this->begin(), eit = this->end();
            it != eit; ++it) {
        CHNode *node = it->second;
        string className = node->getName();
        /// Ancestors
        if (hasAncestors(className)) {
            set<string> ancestorsNames;
            const CHNodeSetTy &ancestors = getAncestors(className);
            for (CHNodeSetTy::const_iterator nit = ancestors.begin(),
                    neit = ancestors.end(); nit != neit; ++nit) {
                const CHNode *curnode = *nit;
                ancestorsNames.insert(curnode->getName());
            }
            classNameToAncestorsNamesMap[className] = ancestorsNames;
        }
        /// Instances
        if (hasInstances(className)) {
            set<string> instancesNames;
            const CHNodeSetTy &instances = getInstances(className);
            for (CHNodeSetTy::const_iterator nit = instances.begin(),
                    neit = instances.end(); nit != neit; ++nit) {
                const CHNode *curnode = *nit;
                instancesNames.insert(curnode->getName());
            }
            templateNameToInstancesNamesMap[className] = instancesNames;
        }
        /// Descendants
        CHNodeSetTy descendants;
        set<string> descendantsName;
        if (node->isTemplate()) {
            descendants = getTemplateInstancesAndDescendants(className);
        } else if (hasDescendants(className)) {
            descendants = getDescendants(className);
        } else {
        }
        descendants.insert(node);
        for (CHNodeSetTy::const_iterator nit = descendants.begin(),
                neit = descendants.end(); nit != neit; ++nit) {
            descendantsName.insert((*nit)->getName());
        }
        classNameToDescendantsNamesMap[className] = descendantsName;
    }
}

void CHGraph::buildTemplateNameToInstancesMap() {
    for (CHGraph::const_iterator it = this->begin(), eit = this->end();
            it != eit; ++it) {
        CHNode *node = it->second;
        string className = node->getName();
        if (className.size() > 0 && className[className.size() - 1] == '>') {
            string templateName = getBeforeBrackets(className);
            CHNode *templateNode = getNode(templateName);
            assert(templateNode != NULL);
            addEdge(className, templateName, CHEdge::INSTANTCE);
            templateNode->setTemplate();
            map<string, CHNodeSetTy>::iterator it =
                templateNameToInstancesMap.find(templateName);
            if (it != templateNameToInstancesMap.end())
                it->second.insert(node);
            else {
                CHNodeSetTy instances;
                instances.insert(node);
                templateNameToInstancesMap[templateName] = instances;
            }
        }
    }
}

bool CHGraph::hasAncestors(const string className) const {
    map<string, CHNodeSetTy>::const_iterator it;
    it = classNameToAncestorsMap.find(className);
    return it != classNameToAncestorsMap.end();
}

const CHGraph::CHNodeSetTy &CHGraph::getAncestors(const string className) const {
    map<string, CHNodeSetTy>::const_iterator it;
    it = classNameToAncestorsMap.find(className);
    assert(it != classNameToAncestorsMap.end());
    return it->second;
}

set<string> CHGraph::getAncestorsNames(const string className) const {
    map<string, set<string>>::const_iterator it;
    it = classNameToAncestorsNamesMap.find(className);
    assert(it != classNameToAncestorsNamesMap.end());
    return it->second;
}

bool CHGraph::hasDescendants(const string className) const {
    map<string, CHNodeSetTy>::const_iterator it;
    it = classNameToDescendantsMap.find(className);
    return it != classNameToDescendantsMap.end();
}

const CHGraph::CHNodeSetTy &CHGraph::getDescendants(const string className) const {
    map<string, CHNodeSetTy>::const_iterator it;
    it = classNameToDescendantsMap.find(className);
    assert(it != classNameToDescendantsMap.end());
    return it->second;
}

set<string> CHGraph::getDescendantsNames(const string className) const {
    map<string, set<string>>::const_iterator it;
    it = classNameToDescendantsNamesMap.find(className);
    assert(it != classNameToDescendantsNamesMap.end());
    return it->second;
}

bool CHGraph::hasInstances(const string className) const {
    map<string, CHNodeSetTy>::const_iterator it;
    it = templateNameToInstancesMap.find(className);
    return it != templateNameToInstancesMap.end();
}

const CHGraph::CHNodeSetTy &CHGraph::getInstances(const string className) const {
    map<string, CHNodeSetTy>::const_iterator it;
    it = templateNameToInstancesMap.find(className);
    assert(it != templateNameToInstancesMap.end());
    return it->second;
}

set<string> CHGraph::getInstancesNames(const string className) const {
    map<string, set<string>>::const_iterator it;
    it = templateNameToInstancesNamesMap.find(className);
    assert(it != templateNameToInstancesNamesMap.end());
    return it->second;
}

CHGraph::CHNodeSetTy CHGraph::getTemplateInstancesAndDescendants(const string className) const {
    CHNode *thisNode = getNode(className);
    assert(thisNode->isTemplate());
    CHNodeSetTy descendants, instances;
    if (hasDescendants(className))
        descendants = getDescendants(className);
    if (hasInstances(className))
        instances = getInstances(className);
    for (CHNodeSetTy::const_iterator it = instances.begin(),
            eit = instances.end(); it != eit; ++it) {
        const CHNode *node = *it;
        descendants.insert(node);
        if (hasDescendants(node->getName())) {
            CHNodeSetTy instance_descendants =
                getDescendants(node->getName());
            CHNodeSetTy::const_iterator dit, deit;
            for (dit = instance_descendants.begin(), deit = instance_descendants.end();
                    dit != deit; ++dit) {
                const CHNode *dnode = *dit;
                descendants.insert(dnode);
            }
        }
    }
    return descendants;
}

s32_t CHGraph::getVirtualFunctionID(const llvm::Function *vfn) const {
    map<const Function*, s32_t>::const_iterator it =
        virtualFunctionToIDMap.find(vfn);
    if (it != virtualFunctionToIDMap.end())
        return it->second;
    else
        return -1;
}

const Function *CHGraph::getVirtualFunctionBasedonID(s32_t id) const {
    map<const Function*, s32_t>::const_iterator it, eit;
    for (it = virtualFunctionToIDMap.begin(), eit = virtualFunctionToIDMap.end();
            it != eit; ++it) {
        if (it->second == id)
            return it->first;
    }
    return NULL;
}

/*
 * do the following things:
 * 1. initialize virtualFunctions for each class
 * 2. mark multi-inheritance classes
 * 3. mark pure abstract classes
 *
 * Layout of VTables:
 *
 * 1. single inheritance
 * class A {...};
 * class B: public A {...};
 * B's vtable: {i8 *null, _ZTI1B, ...}
 *
 * 2. normal multiple inheritance
 * class A {...};
 * class B {...};
 * class C: public A, public B {...};
 * C's vtable: {i8 *null, _ZTI1C, ..., inttoptr xxx, _ZTI1C, ...}
 * "inttoptr xxx" servers as a delimiter for dividing virtual methods inherited
 * from "class A" and "class B"
 *
 * 3. virtual diamond inheritance
 * class A {...};
 * class B: public virtual A {...};
 * class C: public virtual A {...};
 * class D: public B, public C {...};
 * D's vtable: {i8 *null, _ZTI1C, ..., inttoptr xxx, _ZTI1C, i8 *null, ...}
 * there will several "i8 *null" following "inttoptr xxx, _ZTI1C,", and the
 * number of "i8 *null" is the same as the number of virtual methods in
 * "class A"
 */
void CHGraph::analyzeVTables(const Module &M) {
    for (Module::const_global_iterator I = M.global_begin(),
            E = M.global_end(); I != E; ++I) {
        const GlobalValue *globalvalue = dyn_cast<const GlobalValue>(I);
        if (isValVtbl(globalvalue) && globalvalue->getNumOperands() > 0) {
            const ConstantStruct *vtblStruct =
                dyn_cast<ConstantStruct>(globalvalue->getOperand(0));
            assert(vtblStruct && "Initializer of a vtable not a struct?");

            string vtblClassName = getClassNameFromVtblObj(globalvalue);
            CHNode *node = getOrCreateNode(vtblClassName);

            node->setVTable(globalvalue);

            for (int ei = 0; ei < (int)vtblStruct->getNumOperands(); ++ei) {
                const ConstantArray *vtbl =
                    dyn_cast<ConstantArray>(vtblStruct->getOperand(ei));
                assert(vtbl && "Element of initializer not an array?");

                /*
                 * items in vtables can be classified into 3 categories:
                 * 1. i8* null
                 * 2. i8* inttoptr xxx
                 * 3. i8* bitcast xxx
                 */
                bool pure_abstract = true;
                u32_t i = 0;
                while (i < vtbl->getNumOperands()) {
                    vector<const Function*> virtualFunctions;
                    bool is_virtual = false; // virtual inheritance
                    int null_ptr_num = 0;
                    for (; i < vtbl->getNumOperands(); ++i) {
                        if (isa<ConstantPointerNull>(vtbl->getOperand(i))) {
                            if (i > 0 && !isa<ConstantPointerNull>(vtbl->getOperand(i-1))) {
                                const ConstantExpr *ce =
                                    dyn_cast<ConstantExpr>(vtbl->getOperand(i-1));
                                if (ce->getOpcode() == Instruction::BitCast) {
                                    const Value *bitcastValue = ce->getOperand(0);
                                    string bitcastValueName = bitcastValue->getName().str();
                                    if (bitcastValueName.compare(0, ztiLabel.size(), ztiLabel) == 0) {
                                        is_virtual = true;
                                        null_ptr_num = 1;
                                        while (i+null_ptr_num < vtbl->getNumOperands()) {
                                            if (isa<ConstantPointerNull>(vtbl->getOperand(i+null_ptr_num)))
                                                null_ptr_num++;
                                            else
                                                break;
                                        }
                                    }
                                }
                            }
                            continue;
                        }
                        const ConstantExpr *ce =
                            dyn_cast<ConstantExpr>(vtbl->getOperand(i));
                        assert(ce != NULL && "item in vtable not constantexp or null");
                        u32_t opcode = ce->getOpcode();
                        assert(opcode == Instruction::IntToPtr ||
                               opcode == Instruction::BitCast);
                        assert(ce->getNumOperands() == 1 &&
                               "inttptr or bitcast operand num not 1");
                        if (opcode == Instruction::IntToPtr) {
                            node->setMultiInheritance();
                            ++i;
                            break;
                        }
                        if (opcode == Instruction::BitCast) {
                            const Value *bitcastValue = ce->getOperand(0);
                            string bitcastValueName = bitcastValue->getName().str();
                            /*
                             * value in bitcast:
                             * _ZTIXXX
                             * Function
                             * GlobalAlias (alias to other function)
                             */
                            assert(isa<Function>(bitcastValue) ||
                                   isa<GlobalValue>(bitcastValue));
                            if (const Function *func = dyn_cast<Function>(bitcastValue)) {
                                node->addVirtualFunction(func);
                                virtualFunctions.push_back(func);
                                if (func->getName().str().compare(pureVirtualFunName) == 0) {
                                    pure_abstract &= true;
                                } else {
                                    pure_abstract &= false;
                                }
                                struct DemangledName dname = demangle(func->getName().str());
                                if (dname.className.size() > 0 &&
                                        vtblClassName.compare(dname.className) != 0) {
                                    addEdge(vtblClassName, dname.className, CHEdge::INHERITANCE);
                                }
                            } else {
                                if (const GlobalAlias *alias =
                                            dyn_cast<GlobalAlias>(bitcastValue)) {
                                    const Constant *aliasValue = alias->getAliasee();
                                    if (const Function *aliasFunc =
                                                dyn_cast<Function>(aliasValue)) {
                                        node->addVirtualFunction(aliasFunc);
                                        virtualFunctions.push_back(aliasFunc);
                                    } else if (const ConstantExpr *aliasconst =
                                                   dyn_cast<ConstantExpr>(aliasValue)) {
                                        u32_t aliasopcode = aliasconst->getOpcode();
                                        assert(aliasopcode == Instruction::BitCast &&
                                               "aliased constantexpr in vtable not a bitcast");
                                        const Function *aliasbitcastfunc =
                                            dyn_cast<Function>(aliasconst->getOperand(0));
                                        assert(aliasbitcastfunc &&
                                               "aliased bitcast in vtable not a function");
                                        node->addVirtualFunction(aliasbitcastfunc);
                                        virtualFunctions.push_back(aliasbitcastfunc);
                                    } else {
                                        assert(false && "alias not function or bitcast");
                                    }

                                    pure_abstract &= false;
                                } else if (bitcastValueName.compare(0, ztiLabel.size(),
                                                                    ztiLabel) == 0) {
                                } else {
                                    assert("what else can be in bitcast of a vtable?");
                                }
                            }
                        }
                    }
                    if (is_virtual && virtualFunctions.size() > 0) {
                        for (int i = 0; i < null_ptr_num; ++i) {
                            const Function *fun = virtualFunctions[i];
                            virtualFunctions.insert(virtualFunctions.begin(), fun);
                        }
                    }
                    if (virtualFunctions.size() > 0)
                        node->addVirtualFunctionVector(virtualFunctions);
                }
                if (pure_abstract == true) {
                    node->setPureAbstract();
                }
            }
        }
    }
}

/*
 * build argsizeToVFunMap for each CHNode;
 */
void CHGraph::buildArgsizeToVFunMap() {
    for (CHGraph::iterator it = this->begin(), eit = this->end();
            it != eit; ++it) {
        CHNode *node = it->second;
        const vector<vector<const Function*>> vecs =
                                               node->getVirtualFunctionVectors();
        u32_t max_argsize = 0;
        for (vector<vector<const Function*>>::const_iterator vit = vecs.begin(),
                veit = vecs.end(); vit != veit; ++vit) {
            for (vector<const Function*>::const_iterator fit = (*vit).begin(),
                    feit = (*vit).end(); fit != feit; ++fit) {
                const Function *func = *fit;
                if (func->arg_size() > max_argsize)
                    max_argsize = func->arg_size();
            }
        }

        for (u32_t argsize = 1; argsize <= max_argsize; ++argsize) {
            set<const Function*> functions;
            for (vector<vector<const Function*>>::const_iterator vit = vecs.begin(),
                    veit = vecs.end(); vit != veit; ++vit) {
                for (vector<const Function*>::const_iterator fit = (*vit).begin(),
                        feit = (*vit).end(); fit != feit; ++fit) {
                    const Function *func = *fit;
                    if (func->arg_size() == argsize)
                        functions.insert(func);
                }
            }
            if (functions.size() > 0)
                node->setArgsizeToVFunMap(argsize, functions);
        }
    }

}

string CHGraph::getClassNameOfThisPtr(CallSite cs) const {
    string thisPtrClassName;
    Instruction *inst = cs.getInstruction();
    if (MDNode *N = inst->getMetadata("VCallPtrType")) {
        MDString *mdstr = cast<MDString>(N->getOperand(0));
        thisPtrClassName = mdstr->getString().str();
    }
    if (thisPtrClassName.size() == 0) {
        const Value *thisPtr = getVCallThisPtr(cs);
        if (thisPtr!=NULL)
            thisPtrClassName = getClassNameFromType(thisPtr->getType());
    }

    size_t found = thisPtrClassName.find_last_not_of("0123456789");
    if (found != string::npos) {
        if (found != thisPtrClassName.size() - 1 && thisPtrClassName[found] == '.') {
            return thisPtrClassName.substr(0, found);
        }
    }

    return thisPtrClassName;
}

string CHGraph::getFunNameOfVCallSite(CallSite cs) const {
    string funName;
    Instruction *inst = cs.getInstruction();
    if (MDNode *N = inst->getMetadata("VCallFunName")) {
        MDString *mdstr = cast<MDString>(N->getOperand(0));
        funName = mdstr->getString().str();
    }
    return funName;
}



void CHGraph::buildVirtualFunctionToIDMap() {
    /*
     * 1. Divide classes into groups
     * 2. Get all virtual functions in a group
     * 3. Assign consecutive IDs to virtual functions that have
     * the same name (after demangling) in a group
     */
    CHNodeSetTy visitedNodes;
    for (CHGraph::const_iterator nit = this->begin(),
            neit = this->end(); nit != neit; ++nit) {
        CHNode *node = nit->second;
        if (visitedNodes.find(node) != visitedNodes.end())
            continue;

        string className = node->getName();

        /*
         * get all the classes in a specific group
         */
        CHNodeSetTy group;
        stack<const CHNode*> nodeStack;
        nodeStack.push(node);
        while (!nodeStack.empty()) {
            const CHNode *curnode = nodeStack.top();
            nodeStack.pop();
            group.insert(curnode);
            if (visitedNodes.find(curnode) != visitedNodes.end())
                continue;
            for (set<CHEdge*>::const_iterator it = curnode->getOutEdges().begin(),
                    eit = curnode->getOutEdges().end(); it != eit; ++it) {
                CHNode *tmpnode = (*it)->getDstNode();
                nodeStack.push(tmpnode);
                group.insert(tmpnode);
            }
            for (set<CHEdge*>::const_iterator it = curnode->getInEdges().begin(),
                    eit = curnode->getInEdges().end(); it != eit; ++it) {
                CHNode *tmpnode = (*it)->getSrcNode();
                nodeStack.push(tmpnode);
                group.insert(tmpnode);
            }
            visitedNodes.insert(curnode);
        }

        /*
         * get all virtual functions in a specific group
         */
        set<const Function*> virtualFunctions;
        for (CHNodeSetTy::iterator it = group.begin(),
                eit = group.end(); it != eit; ++it) {
            const CHNode *tmpnode = *it;
            const set<const Function*> &vfns = tmpnode->getAllVirtualFunctions();
            for (set<const Function*>::iterator fit = vfns.begin(),
                    feit = vfns.end(); fit != feit; ++fit) {
                virtualFunctions.insert(*fit);
            }
        }

        /*
         * build a set of pairs of demangled function name and function in a
         * specific group, items in the set will be sort by the first item of the
         * pair, so all the virtual functions in a group will be sorted by the
         * demangled function name
         * <f, A::f>
         * <f, B::f>
         * <g, A::g>
         * <g, B::g>
         * <g, C::g>
         * <~A, A::~A>
         * <~B, B::~B>
         * <~C, C::~C>
         * ...
         */
        set<pair<string, const Function*>> fNameSet;
        for (set<const Function*>::iterator fit = virtualFunctions.begin(),
                feit = virtualFunctions.end(); fit != feit; ++fit) {
            const Function *f = *fit;
            struct DemangledName dname = demangle(f->getName().str());
            fNameSet.insert(pair<string, const Function*>(dname.funcName, f));
        }
        for (set<pair<string, const Function*>>::iterator it = fNameSet.begin(),
                eit = fNameSet.end(); it != eit; ++it) {
            virtualFunctionToIDMap[it->second] = vfID++;
        }
    }
}

void CHGraph::getCSClasses(CallSite cs, CHNodeSetTy &chClasses) const {

    assert(isVirtualCallSite(cs) && "not virtual callsite!");

    string thisPtrClassName = getClassNameOfThisPtr(cs);

    CHNode *thisNode = getNode(thisPtrClassName);
    if (thisNode == NULL)
        return;

    ////// get descendants based cha
    CHNodeSetTy descendants;
    if (thisNode->isTemplate()) {
        descendants = getTemplateInstancesAndDescendants(thisPtrClassName);
    } else if (hasDescendants(thisPtrClassName)) {
        descendants = getDescendants(thisPtrClassName);
    } else {
    }
    descendants.insert(thisNode);

    for (CHNodeSetTy::const_iterator it = descendants.begin(),
            eit = descendants.end(); it != eit; ++it) {
        const CHNode *child = *it;
        chClasses.insert(child);
    }
}

/*
 * Is this virtual call inside its own constructor or destructor?
 */
bool CHGraph::VCallInCtorOrDtor(CallSite cs) const {
    std::string classNameOfThisPtr = getClassNameOfThisPtr(cs);
    const Function *func = cs.getInstruction()->getParent()->getParent();
    if (isConstructor(func) || isDestructor(func)) {
        struct DemangledName dname = demangle(func->getName().str());
        if (classNameOfThisPtr.compare(dname.className) == 0)
            return true;
    }
    return false;
}

/*
 * Get virtual functions for callsite "cs" based on vtbls (calculated
 * based on pointsto set)
 */
void CHGraph::getVFnsFromVtbls(llvm::CallSite cs,
                               const std::set<const llvm::Value*> &vtbls,
                               std::set<const llvm::Function*> &virtualFunctions) const {

    /// get target virtual functions
    size_t idx = cppUtil::getVCallIdx(cs);
    /// get the function name of the virtual callsite
    string funName = getFunNameOfVCallSite(cs);
    for (std::set<const llvm::Value*>::iterator it = vtbls.begin(),
            eit = vtbls.end(); it != eit; ++it) {
        const CHNode *child = getNode(getClassNameFromVtblObj(*it));
        if (child == NULL)
            continue;
        set<const Function*> vfns;
        child->getVirtualFunctions(idx, vfns);
        for (set<const Function*>::const_iterator fit = vfns.begin(),
                feit = vfns.end(); fit != feit; ++fit) {
            const Function* callee = *fit;
            if (cs.arg_size() == callee->arg_size() ||
                    (cs.getFunctionType()->isVarArg() && callee->isVarArg())) {
                cppUtil::DemangledName dname =
                    cppUtil::demangle(callee->getName().str());
                string calleeName = dname.funcName;

                /*
                 * The compiler will add some special suffix (e.g.,
                 * "[abi:cxx11]") to the end of some virtual function:
                 * In dealII
                 * function: FE_Q<3>::get_name
                 * will be mangled as: _ZNK4FE_QILi3EE8get_nameB5cxx11Ev
                 * after demangling: FE_Q<3>::get_name[abi:cxx11]
                 * The special suffix ("[abi:cxx11]") needs to be removed
                 */
                const std::string suffix("[abi:cxx11]");
                size_t suffix_pos = calleeName.rfind(suffix);
                if (suffix_pos != string::npos)
                  calleeName.erase(suffix_pos, suffix.size());

                /*
                 * if we can't get the function name of a virtual callsite, all virtual
                 * functions calculated by idx will be valid
                 */
                if (funName.size() == 0) {
                    virtualFunctions.insert(callee);
                } else if (funName[0] == '~') {
                    /*
                     * if the virtual callsite is calling a destructor, then all
                     * destructors in the ch will be valid
                     * class A { virtual ~A(){} };
                     * class B: public A { virtual ~B(){} };
                     * int main() {
                     *   A *a = new B;
                     *   delete a;  /// the function name of this virtual callsite is ~A()
                     * }
                     */
                    if (calleeName[0] == '~') {
                        virtualFunctions.insert(callee);
                    }
                } else {
                    /*
                     * for other virtual function calls, the function name of the callsite
                     * and the function name of the target callee should match exactly
                     */
                    if (funName.compare(calleeName) == 0) {
                        virtualFunctions.insert(callee);
                    }
                }
            }
        }
    }
}

/*!
 * Dump call graph into dot file
 */
void CHGraph::dump(const std::string& filename) {
    if(dumpGraph)
        GraphPrinter::WriteGraphToFile(llvm::outs(), filename, this);

}

void CHGraph::collectVirtualCallSites() {
    for (SVFModule::iterator F = svfMod.begin(),
            EF = svfMod.end(); F != EF; ++F) {
        Function *fn = *F;
        for (inst_iterator I = inst_begin(*fn),
                EI = inst_end(*fn); I != EI; ++I) {
            Instruction *inst = &*I;
            if (isa<CallInst>(inst) || isa<InvokeInst>(inst)) {
                CallSite cs = analysisUtil::getLLVMCallSite(inst);
                if (cppUtil::isVirtualCallSite(cs))
                    virtualCallSites.insert(cs);
            }
        }
    }
}

void CHGraph::buildCSToCHAVtblsAndVfnsMap() {
    for (set<CallSite>::iterator it = virtualCallSites.begin(),
            eit = virtualCallSites.end(); it != eit; ++it) {
        CallSite cs = *it;
        set<const Value*> vtbls;
        CHNodeSetTy chClasses;
        getCSClasses(cs, chClasses);
        for (CHNodeSetTy::const_iterator it = chClasses.begin(),
                eit = chClasses.end(); it != eit; ++it) {
            const CHNode *child = *it;
            const Value *vtbl = child->getVTable();
            if (vtbl != NULL) {
                vtbls.insert(vtbl);
            }
        }
        if (vtbls.size() > 0) {
            csToCHAVtblsMap[cs] = vtbls;
            set<const Function*> virtualFunctions;
            getVFnsFromVtbls(cs, vtbls, virtualFunctions);
            if (virtualFunctions.size() > 0)
                csToCHAVFnsMap[cs] = virtualFunctions;
        }
    }
}

const bool CHGraph::csHasVtblsBasedonCHA(llvm::CallSite cs) const {
    map<CallSite, set<const Value*>>::const_iterator it = csToCHAVtblsMap.find(cs);
    return it != csToCHAVtblsMap.end();
}

const bool CHGraph::csHasVFnsBasedonCHA(llvm::CallSite cs) const {
    map<CallSite, set<const Function*>>::const_iterator it = csToCHAVFnsMap.find(cs);
    return it != csToCHAVFnsMap.end();
}

const set<const Value*> &CHGraph::getCSVtblsBasedonCHA(llvm::CallSite cs) const {
    map<CallSite, set<const Value*>>::const_iterator it = csToCHAVtblsMap.find(cs);
    assert(it != csToCHAVtblsMap.end() && "cs does not have vtabls based on CHA.");
    return it->second;
}

const set<const Function*> &CHGraph::getCSVFsBasedonCHA(llvm::CallSite cs) const {
    map<CallSite, set<const Function*>>::const_iterator it = csToCHAVFnsMap.find(cs);
    assert(it != csToCHAVFnsMap.end() && "cs does not have vfns based on CHA.");
    return it->second;
}

namespace llvm {

/*!
 * Write value flow graph into dot file for debugging
 */
template<>
struct DOTGraphTraits<CHGraph*> : public DefaultDOTGraphTraits {

    typedef CHNode NodeType;
    DOTGraphTraits(bool isSimple = false) :
        DefaultDOTGraphTraits(isSimple) {
    }

    /// Return name of the graph
    static std::string getGraphName(CHGraph *graph) {
        return "Class Hierarchy Graph";
    }
    /// Return function name;
    static std::string getNodeLabel(CHNode *node, CHGraph *graph) {
        return node->getName();
    }

    static std::string getNodeAttributes(CHNode *node, CHGraph *CHGraph) {
        if (node->isPureAbstract()) {
            return "shape=Mcircle";
        } else
            return "shape=circle";
    }

    template<class EdgeIter>
    static std::string getEdgeAttributes(CHNode *node, EdgeIter EI, CHGraph *CHGraph) {

        CHEdge* edge = *(EI.getCurrent());
        assert(edge && "No edge found!!");
        if (edge->getEdgeType() == CHEdge::INHERITANCE) {
            return "style=solid";
        } else {
            return "style=dashed";
        }
    }
};
}
