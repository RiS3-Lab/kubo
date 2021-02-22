#include "Project.h"

extern const int MAX_NUM_TRACES_PER_SEED;
// parameter to control loop unrolling
#define KSYM_CONFIG_UNROLL_LINK_LIMIT         64
#define KSYM_CONFIG_UNROLL_LATCH_LIMIT        64
#define KSYM_CONFIG_UNROLL_TOTAL_LIMIT        256
#define CONFIG_MAX_UNROLL_PATH MAX_NUM_TRACES_PER_SEED

UnrollPath *UnrollCache::getUnrolled(DAPath *path, LLVMSliceBlock *term,
    DAGraph *graph) {

  auto k = make_pair(path, term);

  auto i = cache.find(k);
  if(i != cache.end()){
    return i->second;
  }

  DAItem *mark = graph->query(term);
  assert(mark != nullptr);

  // create new unrolled path
  UnrollPath *unrolled = new UnrollPath;

  DAPath::iterator ti = path->begin(), te = path->end();
  for(; ti != te; ++ti){
    DATrace *trace = *ti;
    assert(mark == *(trace->rbegin()));

    blist *blks = new blist;
    // errs()<<"unroll one path\n";
    unrollRecursive(trace->begin(), trace->end(),
        term, graph, blks, unrolled);
  }

  cache.insert(make_pair(k, unrolled));
  return unrolled;
}

static bool isLinked(LLVMSliceBlock *from, LLVMSliceBlock *to) {
  LLVMSliceBlock::succ_iterator si = from->succ_begin(), se = from->succ_end();
  for(; si != se; ++si){
    if(*si == to){
      return true;
    }
  }

  return false;
}

// Unroll each each path contains SCC components,
// Use loop to handle normal basic block components,
// Use recusion to handle loop components.

void UnrollCache::unrollRecursive(DATrace::iterator cur, DATrace::iterator end,
    LLVMSliceBlock *term, DAGraph *graph, blist *blks, UnrollPath *unrolled) {

  if (unrolled->size() >= CONFIG_MAX_UNROLL_PATH) {
    return;
  }

  // test if reached the end of the DATrace
  if(cur == end){
      unrolled->add(blks);
      return;
  }

  DAItem *item = *cur;
  cur++;

  // BUG(yaohway/using recusion to handle normal blocks is too wasty.)
  // if(DABlock *dab = dyn_cast<DABlock>(item)){
  //   blks->push_back(dab->getBlock());
  //   unrollRecursive(cur, end, term, graph, blks, unrolled);
  // }

  while(DABlock *dab = dyn_cast<DABlock>(item)){
      blks->push_back(dab->getBlock());
      if(cur == end){
          unrolled->add(blks);
          return;
      }
      item = *cur;
      cur++;
  }

  if (DALoop *dal = dyn_cast<DALoop>(item)){
    LLVMSliceLoop *loop = dal->getLoop();
    DAGraph *sub = graph->subgraph(dal);

    // collect and unroll paths to links
    set<LLVMSliceBlock *> links;
    // cur is the next DAItem to the current item, cur and item are not pointing to the same element
    if(cur != end){
      // the loop is not the final step
      SmallVector<LLVMSliceBlock *, 32> exits;
      loop->getExitingBlocks(exits);

      LLVMSliceBlock *next = (*cur)->entrance();
      for(LLVMSliceBlock *e : exits){
        //YH: how can exit blocks link to loopheader?
        //LCM: next here is not in the loop, it's the LLVMSliceBasicBlock right after the loop
        if(isLinked(/*from=*/e, /*to=*/next)){
          links.insert(e);
        }
      }
    } else {
      //although current DAItem is a Loop
      // and the next item is NONE(the end of the iterator)
      // which means, the term BB should be where the loop ends
      // however, since this terminator BB is where the seed instruction resides, it might not be
      //a legal exit BB for a loop. As long as it's in the loop, there should be a path to that
      links.insert(term);
    }

    map<LLVMSliceBlock *, UnrollPath *> linkPaths;
    for(LLVMSliceBlock *l : links){
      linkPaths.insert(make_pair(l, getUnrolled(/*path=*/sub->getPath(l),
                                                /*term=*/l,
                                                /*graph=*/sub)));
    }

#ifdef KSYM_CONFIG_UNROLL_ONCE
    // unroll paths to latches
    SmallVector<LLVMSliceBlock *, 32> latches;
    loop->getLoopLatches(latches);

    map<LLVMSliceBlock *, UnrollPath *> latchPaths;
    for(LLVMSliceBlock *l : latches){
      latchPaths.insert(make_pair(l, getUnrolled(sub->getPath(l), l, sub)));
    }

    // pick one latch path and one link path to unroll the loop
    for(auto const &i : linkPaths){
      UnrollPath *ips = i.second;
      for(auto const &j : latchPaths){
        UnrollPath *jps = j.second;

        unsigned ic = 0;
        UnrollPath::iterator ipi = ips->begin(), ipe = ips->end();
        for(; ipi != ipe; ++ipi){
          if(ic++ >= KSYM_CONFIG_UNROLL_LINK_LIMIT){
            continue;
          }

          unsigned jc = 0;
          UnrollPath::iterator jpi = jps->begin(), jpe = jps->end();
          for(; jpi != jpe; ++jpi){
            if(jc++ >= KSYM_CONFIG_UNROLL_LATCH_LIMIT){
              continue;
            }

            blist *nbl = new blist(*blks);
            nbl->insert(nbl->end(), (*jpi)->begin(), (*jpi)->end());
            nbl->insert(nbl->end(), (*ipi)->begin(), (*ipi)->end());

            unrollRecursive(cur, end, term, graph, nbl, unrolled);
          }
        }
      }
    }
#else
    // pick one link path to unroll the loop
    for(auto const &i : linkPaths){
        UnrollPath *ps = i.second;

        unsigned c = 0;
        UnrollPath::iterator pi = ps->begin(), pe = ps->end();
        for(; pi != pe; ++pi){
            if(c++ >= KSYM_CONFIG_UNROLL_TOTAL_LIMIT){
                continue;
            }

            blist *nbl = new blist(*blks);
            nbl->insert(nbl->end(), (*pi)->begin(), (*pi)->end());

            // check if more unrolling needed
            unrollRecursive(cur, end, term, graph, nbl, unrolled);
        }
    }
#endif

    delete blks;
  }

  else {
    llvm_unreachable("Unknown DAItem type");
  }
}
