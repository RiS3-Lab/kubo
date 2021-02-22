//===- WPASolver.h -- Generic WPA solver--------------------------------------//
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
 * WPASolver.h
 *
 *  Created on: Oct 25, 2013
 *      Author: Yulei Sui
 */

#ifndef GRAPHSOLVER_H_
#define GRAPHSOLVER_H_

#include "Util/WorkList.h"
#include <llvm/ADT/GraphTraits.h>

#include "stopwatch.h"


/*
 * Generic graph solver for whole program pointer analysis
 * - Notes by Tong -
 * This class is used by several WPA Pointer analysis class: Andersen* etc
 * This class provides an SCC detection algorithm and methods for merging SCC
 * nodes, WPA Pointer Analysis classes may use binary vector to represent nodes
 * and use this class to detect SCC and merge nodes and update the vector
 * The WPA Pointer analysis class will need to construct a initial binary vector 
 * by iterating through the IR then run the WPASolver
 *
 * This classis also the base class of WPAFSSolver, the flow sensitive solver
 *
 * There are two types of graphs implemented: ConstraintGraph and SVFG,
 * Andersen* use ConstraintGraph + WPASolver
 * FlowSensitive uses SVFG + WPAFSolver
 *
 * ConstraintGraph - Constraint Graph for Andersen*
 *  - related classes: ConsG, ConsGEdge, ConsGNode, PAGBuilder
 * SVFG - Sparse Value Flow Graph - use with SVFGBuilder 
 *  - related classes: SVFG, SVFGNode, SVFGEdge, SVFGBuilder, SVFGStat
 */
template<class GraphType>
class WPASolver {

public:
    ///Define the GTraits and node iterator for printing
    typedef llvm::GraphTraits<GraphType> GTraits;
    typedef typename GTraits::NodeRef           GNODE;
    typedef typename GTraits::EdgeType          GEDGE;
    typedef typename GTraits::ChildIteratorType child_iterator;

    typedef SCCDetection<GraphType> SCC;

    typedef FIFOWorkList<NodeID> WorkList;

protected:

    /// Constructor
    WPASolver(): _graph(NULL),scc(NULL)
    {
    }
    /// Destructor
    virtual ~WPASolver() {
        delete scc;
        scc = NULL;
    }
    /// Get SCC detector
    inline SCC* getSCCDetector() const {
        return scc;
    }

    /// Get/Set graph methods
    //@{
    const inline GraphType graph() {
        return _graph;
    }
    inline void setGraph(GraphType g) {
        _graph = g;
        if(!scc)
            scc = new SCC(_graph);
    }
    //@}

    /// SCC detection
    virtual inline NodeStack& SCCDetect() {
        getSCCDetector()->find();
        return getSCCDetector()->topoNodeStack();
    }

    /// Constraint Solving
    virtual void solve()
    {
        llvm::errs()<<"Solve: prepare\n";
        STOP_WATCH(1);
        STOP_WATCH_START(0);
        /// SCC detection
        /// Nodes in nodeStack are in topological order by default.
        /// This order can be changed by overwritten SCCDetect() in sub-classes
        NodeStack& nodeStack = SCCDetect();

        /// initial worklist
        /// process nodes in nodeStack.
        while (!nodeStack.empty())
        {
            NodeID nodeId = nodeStack.top();
            nodeStack.pop();
            processNode(nodeId);
        }
        STOP_WATCH_STOP(0);
        STOP_WATCH_REPORT(0);

        /// start solving
        /// New nodes may be inserted into work list during processing.
        /// Keep solving until it's empty.
        llvm::errs()<<"Solve: start\n";
        STOP_WATCH_START(0);
        while (!isWorklistEmpty())
        {
            NodeID nodeId = popFromWorklist();
            postProcessNode(nodeId);
        }
        worklist.clear();
        STOP_WATCH_STOP(0);
        STOP_WATCH_REPORT(0);

    }

    /// Following methods are to be implemented in child class, in order to achieve a fully worked PTA
    //@{
    /// Process each node on the graph, to be implemented in the child class
    virtual inline void processNode(NodeID node) {
    }
    virtual inline void postProcessNode(NodeID node) {
        processNode(node);
    }

    /// Propagation for the solving, to be implemented in the child class
    virtual void propagate(GNODE* v) {
        child_iterator EI = GTraits::direct_child_begin(*v);
        child_iterator EE = GTraits::direct_child_end(*v);
        for (; EI != EE; ++EI) {
            if (propFromSrcToDst(*(EI.getCurrent())))
                pushIntoWorklist(Node_Index(*EI));
        }
    }
    /// Propagate information from source to destination node, to be implemented in the child class
    virtual bool propFromSrcToDst(GEDGE* edge) {
        return false;
    }
    //@}

    virtual NodeID sccRepNode(NodeID id) const {
        return getSCCDetector()->repNode(id);
    }

    /// Worklist operations
    //@{
    inline NodeID popFromWorklist() {
        return sccRepNode(worklist.pop());
    }
    inline void pushIntoWorklist(NodeID id) {
        worklist.push(sccRepNode(id));
    }
    inline bool isWorklistEmpty() {
        return worklist.empty();
    }
    inline bool isInWorklist(NodeID id) {
        return worklist.find(id);
    }
    //@}

protected:
    /// Get node on the graph
    inline GNODE* Node(NodeID id) {
        return GTraits::getNode(_graph, id);
    }

    /// Get node ID
    inline NodeID Node_Index(GNODE node) {
        return GTraits::getNodeID(node);
    }

private:
    /// Graph
    GraphType _graph;

    /// SCC
    SCC* scc;

    /// Worklist for resolution
    WorkList worklist;
};

#endif /* GRAPHSOLVER_H_ */
