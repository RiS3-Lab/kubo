//===- PTAType.h -- PTAType class---------------------------------------------//
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
 * PTAType.h
 *
 *  Created on: Oct 06, 2016
 *      Author: Xiaokang Fan
 */

#ifndef PTATYPE_H_
#define PTATYPE_H_

#include <llvm/IR/Type.h>
#include <llvm/Support/raw_ostream.h>

#include <unordered_map>
#include <set>

#include "Util/BasicTypes.h"

class PAGNode;
class PAG;

class PTAType {
public:
    /// Constructor
    PTAType(const llvm::Type *ty): type(ty) {}

    /// Get the contained llvm type
    inline const llvm::Type *getLLVMType() const {
        return type;
    }

    /// Dump the type
    inline void dump() const {
        type->dump();
    }

    /// Operator overloading
    //@{
    inline bool operator==(const PTAType &ty) const {
        return type == ty.getLLVMType();
    }

    inline bool operator!=(const PTAType &ty) const {
        return type != ty.getLLVMType();
    }

    inline bool operator<(const PTAType &ty) const {
        return type < ty.getLLVMType();
    }

    inline bool operator>(const PTAType &ty) const {
        return type > ty.getLLVMType();
    }
    //@}

private:
    const llvm::Type *type;
};

namespace std
{
    template <>
    struct hash<PTAType>
    {
        std::size_t operator()(const PTAType& k) const
        {
            return std::hash<unsigned long>{}((unsigned long)(void*)k.getLLVMType());
        }
    };
}

class TypeSet {
public:

    typedef std::set<PTAType> TypeSetTy;

    typedef TypeSetTy::iterator iterator;
    typedef TypeSetTy::const_iterator const_iterator;

    // Iterators
    //@{
    inline iterator begin() {
        return typeSet.begin();
    }
    inline iterator end() {
        return typeSet.end();
    }
    inline const_iterator begin() const {
        return typeSet.begin();
    }
    inline const_iterator end() const {
        return typeSet.end();
    }
    //@}

    /// Number of types contained
    inline u32_t size() const {
        return typeSet.size();
    }

    /// Add a ptatype
    inline bool addType(const PTAType &type) {
        std::pair<iterator, bool> ret = typeSet.insert(type);
        return ret.second;
    }

    /// Contain a ptatype or not
    inline bool containType(const PTAType &type) const {
        return typeSet.find(type) != typeSet.end();
    }

    /// Intersect with another typeset or not
    // Algorithm set_intersection
    // Complexity: 2 * (N1 + N2) - 1
    // N1, N2: number of element in the two typeset
    inline bool intersect(const TypeSet *typeset) const {
        if (size() == 1) {
            const_iterator first = begin();
            return typeset->containType(*first);
        } else if (typeset->size() == 1) {
            const_iterator first = typeset->begin();
            return containType(*first);
        } else {
            const_iterator first1 = typeset->begin(), first2 = begin(),
                           last1 = typeset->end(), last2 = end();
            const_iterator largest1 = last1, largest2 = last2;
            largest1--;
            largest2--;
            if (*largest1 < *first2 || *largest2 < *first1)
                return false;

            while (first1 != last1 && first2 != last2) {
                if (*first1 < *first2)
                    first1++;
                else if (*first2 < *first1)
                    first2++;
                else
                    return true;
            }
            return false;
        }
    }

    /// Dump all types in the typeset
    inline void dumpTypes() const {
        for (const_iterator it = begin(), eit = end(); it != eit; ++it) {
            const PTAType &type = *it;
            type.dump();
        }
    }

private:
    TypeSetTy typeSet;
};

class TypeSystem {
public:

    typedef std::unordered_map<NodeID, TypeSet*> VarToTypeSetMapTy;
    typedef std::unordered_map<PTAType, NodeBS> TypeToVarsMapTy;

    typedef typename VarToTypeSetMapTy::iterator iterator;
    typedef typename VarToTypeSetMapTy::const_iterator const_iterator;

    /// Iterators
    //@{
    inline iterator begin() {
        return VarToTypeSetMap.begin();
    }
    inline iterator end() {
        return VarToTypeSetMap.end();
    }
    inline const_iterator begin() const {
        return VarToTypeSetMap.begin();
    }
    inline const_iterator end() const {
        return VarToTypeSetMap.end();
    }
    //}@

    /// Constructor
    TypeSystem(const PAG *pag) {
        translateLLVMTypeToPTAType(pag);
    }
    ~TypeSystem()
    {
    }

    /// Has typeset or not
    inline bool hasTypeSet(NodeID var) const {
        const_iterator it = VarToTypeSetMap.find(var);
        return it != VarToTypeSetMap.end();
    }

    /// Get a var's typeset
    inline const TypeSet *getTypeSet(NodeID var) const {
        const_iterator it = VarToTypeSetMap.find(var);
        assert(it != VarToTypeSetMap.end() && "Can not find typeset for var");
        return it->second;
    }

    /// Add a ptatype for a var
    /// Return true if the ptatype is new for this var
    inline bool addTypeForVar(NodeID var, const PTAType &type) {
        iterator it = VarToTypeSetMap.find(var);
        if (it != VarToTypeSetMap.end()) {
            TypeSet *typeSet = it->second;
            return typeSet->addType(type);
        } else {
            TypeSet *typeSet = new TypeSet;
            typeSet->addType(type);
            VarToTypeSetMap[var] = typeSet;
            return true;
        }
    }

    /// Add a ptatype for a var
    /// Return true if the ptatype is new for this var
    inline bool addTypeForVar(NodeID var, const llvm::Type *type) {
        PTAType ptaTy(type);
        return addTypeForVar(var, ptaTy);
    }

    void addVarForType(NodeID var, const PTAType &type) {
        auto it = typeToVarsMap.find(type);
        if (it == typeToVarsMap.end()) {
            NodeBS nodes;
            nodes.set(var);
            typeToVarsMap[type] = nodes;
        } else {
            NodeBS &nodes = it->second;
            nodes.set(var);
        }
    }

    void addVarForType(NodeID var, const llvm::Type *type) {
        PTAType ptaTy(type);
        return addVarForType(var, ptaTy);
    }

    inline bool hasVarsForType(const PTAType &type) const {
        auto it = typeToVarsMap.find(type);
        return it != typeToVarsMap.end();
    }

    inline NodeBS &getVarsForType(const PTAType &type) {
        auto it = typeToVarsMap.find(type);
        assert(it != typeToVarsMap.end() && "Can not find vars for type");
        return it->second;
    }

    //// Debugging function
    //@{
    /// Print each var's id and all its types
    void printTypeSystem() const {
        for (const_iterator it = VarToTypeSetMap.begin(),
                eit = VarToTypeSetMap.end(); it != eit; ++it) {
            llvm::errs() << "Var: " << it->first << '\n';
            llvm::errs() << "types:\n";
            const TypeSet *typeSet = it->second;
            typeSet->dumpTypes();
            llvm::errs() << '\n';
        }
    }
    //@}

private:
    /*
     * Translate llvm type into ptatype and build the pagnode to ptatype map
     *
     * Kinds of PAGNode:
     * ValPN: GepValPN, DummyValPN
     * ObjPN: GepObjPN, FIObjPN, DummyObjPN
     * RetPN
     * VarArgPN
     */
    void translateLLVMTypeToPTAType(const PAG *pag)
    {
        llvm::errs()<<"translateLLVMTypeToPTAType\n";
        for (PAG::const_iterator it = pag->begin(), ite = pag->end(); it != ite; ++it)
        {
            const PAGNode *pagNode = it->second;
            const llvm::Value *value = pagNode->getValue();
            if (!value)
                continue;
            const llvm::Type *valType = value->getType();
            const llvm::Type *nodeType = valType;

            if (const GepValPN *gepvalnode = llvm::dyn_cast<GepValPN>(pagNode))
            {
                nodeType = gepvalnode->getType();
            } else if (llvm::isa<RetPN>(pagNode))
            {
                const llvm::PointerType *ptrTy = 
                    llvm::dyn_cast<llvm::PointerType>(valType);
                const llvm::FunctionType *funTy = 
                    llvm::dyn_cast<llvm::FunctionType>(ptrTy->getElementType());
                nodeType = funTy->getReturnType();
            }

            PTAType ptaType(nodeType);
            int id = pagNode->getId();
            addTypeForVar(id, ptaType);
            //if (addTypeForVar(id, ptaType))
            //    addVarForType(id, ptaType);
        }
        //NOTE: SparseBitVector: make sure we are adding elements in order
        //addVarForType(id, ptaType);
        std::list<int> ids;
        for (auto I: VarToTypeSetMap)
            ids.push_back(I.first);
        ids.sort();
        for (auto i: ids)
        {
            TypeSet * typeset = VarToTypeSetMap[i];
            for (auto type: *typeset)
                addVarForType(i, type);
        }
    }

private:
    VarToTypeSetMapTy VarToTypeSetMap;
    std::set<PTAType> allPTATypes;
    TypeToVarsMapTy typeToVarsMap;
};

#endif /* PTATYPE_H_ */
