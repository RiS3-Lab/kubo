#ifndef HOPSCOTCH_INTERNAL_H
#define HOPSCOTCH_INTERNAL_H

#include "hopscotch.h"

#include <string.h>
#include <assert.h>

/* These are all default log2 ceiling sizes for arrays/buffers that
 * realloc and double on demand. */
#define DEF_STACK_CEIL2 2
#define DEF_NODE_CEIL2 2
#define DEF_SUCC_CEIL2 2
#define DEF_SCC_BUF_CEIL2 2

#define NO_INDEX (UINT32_MAX)

/* #define USE_LOG */

#ifdef USE_LOG
#include <stdio.h>
#define LOG(...) fprintf(stderr, __VA_ARGS__)
#else
#define LOG(...)
#endif

struct node;

enum hopscotch_state {
    HOPSCOTCH_CREATED,
    HOPSCOTCH_SEALED,
    HOPSCOTCH_SOLVED,
};

struct hopscotch {
    enum hopscotch_state state;
    enum hopscotch_error error;
    uint8_t node_ceil2;
    struct node *nodes;

    uint32_t index;
    uint32_t link;

    uint8_t stack_ceil2;
    size_t stack_top;
    uint32_t *stack;
};

struct node {
    const uint32_t id;
    uint32_t index;
    uint32_t lowlink;

    /* Note: This vector-based implementation just automatically
     * collapses duplicates, because it only adds successor nodes
     * that have already been processed. */
    uint8_t succ_ceil;
    bool used;
    bool stacked;
    bool connected;
    size_t succ_count;
    uint32_t *succ;
};

struct solve_env {
    struct hopscotch *t;
    uint8_t scc_buf_ceil;
    uint32_t *scc_buf;
    uint32_t scc_id;
    size_t max_depth;
    hopscotch_solve_cb *cb;
    void *udata;
};

static bool init_node(struct hopscotch *t, uint32_t node_id,
    uint8_t hint, bool connected);

static bool grow_nodes(struct hopscotch *t, uint32_t new_max_id);

static void report_disconnected(struct solve_env *env, uint32_t node_id);
static bool strongconnect(struct solve_env *env,
    uint32_t node_id, size_t depth);

static bool is_stacked(struct hopscotch *t, uint32_t node_id);
static bool push_node(struct hopscotch *t, uint32_t node_id);
static uint32_t pop_node(struct hopscotch *t);

#endif
