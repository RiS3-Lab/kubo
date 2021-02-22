//
// Created by lcm on 1/20/2021.
//

#ifndef PASS_TRACESTATUS_H
#define PASS_TRACESTATUS_H

#endif //PASS_TRACESTATUS_H

#include "Project.h"
enum UB_types {
    uadd,
    usub,
    umul,
    sadd,
    ssub,
    smul,
    negate_overflow,
    align_err,
    bool_err,
    array_bounds,
    enum_err,
    div_by_0,
    not_null_err,
    shift_out_of_bound,
    ub_notype// just for initialization purpose

};
enum UB_types seed2UBType(CallInst*seed);

Instruction * seed2root(CallInst*);

typedef struct _TraceStatus{
    // represents the status of the UBSan seed
    bool traceIncompleteConstraints = false;

    bool satisfiable = false;
    bool controledByUser = false;
    bool postBug = false;

    // internal used data
    enum UB_types bugType = ub_notype;
    set<int> depArgs;
    SEGraph* seg = nullptr;
    // the ub handler
    CallInst* seed = nullptr;
    // the conditional variable for this ub
    Instruction * root = nullptr;
    //SymExec* sym = nullptr;
    _TraceStatus(CallInst * ubhandler, Instruction* i);
    _TraceStatus(_TraceStatus * other);
    ~_TraceStatus();
} TraceStatus;