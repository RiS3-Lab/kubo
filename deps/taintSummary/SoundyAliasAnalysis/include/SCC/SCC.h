#ifndef PROJECT_SCCANALYZE_H
#define PROJECT_SCCANALYZE_H

struct SCC_ENT{
    uint32_t id;
    size_t count;
    uint32_t* successors;
};

int analyze_scc(int num_ent,struct SCC_ENT *ents[],bool write_dot,FILE* out_f,uint32_t** result);

#endif