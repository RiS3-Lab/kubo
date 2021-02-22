#include "hopscotch_internal.h"

struct hopscotch *
hopscotch_new(void) {
    struct hopscotch *res = calloc(1, sizeof(*res));
    if (res == NULL) { return NULL; }

    res->state = HOPSCOTCH_CREATED;
    res->node_ceil2 = DEF_NODE_CEIL2;

    res->nodes = calloc(1LLU << res->node_ceil2, sizeof(res->nodes[0]));

    if (res->nodes == NULL) {
        free(res);
        return NULL;
    }

    res->stack_ceil2 = DEF_STACK_CEIL2;
    res->stack = calloc(1LLU << res->stack_ceil2, sizeof(res->stack[0]));
    if (res->stack == NULL) {
        free(res);
        free(res->nodes);
        return NULL;
    }
    res->stack_top = 0;

    const size_t node_count = 1LLU << res->node_ceil2;
    for (size_t i = 0; i < node_count; i++) {
        struct node n = {
            .id = i,
            .index = NO_INDEX,
        };
        memcpy(&res->nodes[i], &n, sizeof(n));
    }

    LOG("%s: returning %p\n", __func__, (void *)res);
    return res;
}

void hopscotch_free(struct hopscotch *t) {
    const size_t node_ceil = (1LLU << t->node_ceil2);
    for (size_t i = 0; i < node_ceil; i++) {
        struct node *n = &t->nodes[i];
        if (n->used) {
            free(n->succ);
        } else {
            assert(n->succ == NULL);
            //if(n->succ != NULL){
            //    free(n->succ);
            //}
        }
    }
    free(t->nodes);
    free(t->stack);
    free(t);
}

bool hopscotch_add(struct hopscotch *t, uint32_t node_id,
    size_t succ_count, const uint32_t *successors) {
    assert(t);
    if (t->state != HOPSCOTCH_CREATED) {
        t->error = HOPSCOTCH_ERROR_MISUSE;
        return false;
    }

    uint32_t max_node = 0;
    for (size_t i = 0; i < succ_count; i++) {
        const uint32_t succ_id = successors[i];
        if (succ_id > max_node) { max_node = succ_id; }
    }

    if (node_id > max_node) { max_node = node_id; }

    if (max_node >= (1LLU << t->node_ceil2)) {
        if (!grow_nodes(t, max_node)) {
            return false;
        }
    }

    struct node *n = &t->nodes[node_id];
    const bool connected = (succ_count > 1
        || (succ_count == 1 && successors[0] != node_id));

    if (n->succ == NULL) {  /* init node */
        LOG("%s: initializing node %u, adding %zd successors\n",
            __func__, node_id, succ_count);

        uint8_t ceil2 = 1;
        while ((1LLU << ceil2) < succ_count) { ceil2++; }
        if (!init_node(t, node_id, ceil2, connected)) {
            return false;
        }

        assert((1LLU << n->succ_ceil) >= succ_count);
        memcpy(n->succ, successors,
            succ_count * sizeof(uint32_t));
        n->succ_count = succ_count;

        for (size_t i = 0; i < succ_count; i++) {
            if (!init_node(t, successors[i], 0, connected)) { return false; }
        }
    } else {
        /* append, growing if necessary */
        const size_t ncount = n->succ_count + succ_count;

        if (ncount >= (1LLU << n->succ_ceil)) {
            uint8_t nceil2 = n->succ_ceil + 1;
            while ((1LLU << nceil2) < ncount) { nceil2++; }
            const size_t nceil = 1LLU << nceil2;
            uint32_t *nsucc = realloc(n->succ,
                nceil * sizeof(*nsucc));
            if (nsucc == NULL) {
                t->error = HOPSCOTCH_ERROR_MEMORY;
                return false;
            }
            n->succ_ceil = nceil2;
            n->succ = nsucc;
        }
        for (size_t i = 0; i < succ_count; i++) {
            if (!init_node(t, successors[i], 0, connected)) { return false; }
            n->succ[n->succ_count + i] = successors[i];
        }
        n->succ_count += succ_count;
    }

#ifdef USE_LOG
    n = &t->nodes[node_id];     /* n may be stale, reload */
    LOG("%s: node %u: %zu successors:\n", __func__, n->id, n->succ_count);
    for (size_t i = 0; i < n->succ_count; i++) {
        LOG(" -- %u: %u\n", node_id, n->succ[i]);
    }
#endif

    return true;
}

bool hopscotch_seal(struct hopscotch *t) {
    /* canonicalize: eliminate duplicates, sort, ? */
    t->state = HOPSCOTCH_SEALED;
    LOG("%s: %p\n", __func__, (void *)t);
    return true;
}

bool hopscotch_get_successors(struct hopscotch *t, uint32_t node_id,
    size_t *succ_count, const uint32_t **successors) {
    assert(t);
    assert(successors);
    assert(succ_count);
    if (t->state != HOPSCOTCH_SEALED) { return false; }

    assert(node_id < (1LLU << t->node_ceil2));
    struct node *n = &t->nodes[node_id];
    *successors = n->succ;
    *succ_count = n->succ_count;
    return true;
}


bool hopscotch_solve(struct hopscotch *t, size_t max_depth,
    hopscotch_solve_cb *cb, void *udata) {
    LOG("%s: %p -- ceil2 %u, state %d\n",
        __func__, (void *)t, t->node_ceil2, t->state);
    if (t->state != HOPSCOTCH_SEALED) {
        t->error = HOPSCOTCH_ERROR_MISUSE;
        return false;
    }

    const uint8_t scc_buf_ceil = DEF_SCC_BUF_CEIL2;
    uint32_t *buf = calloc(1LLU << scc_buf_ceil, sizeof(*buf));
    if (buf == NULL) { return false; }

    if (max_depth == 0) { max_depth = HOPSCOTCH_SOLVE_DEFAULT_MAX_DEPTH; }

    struct solve_env env = {
        .t = t,
        .scc_buf_ceil = scc_buf_ceil,
        .scc_buf = buf,
        .max_depth = max_depth,
        .cb = cb,
        .udata = udata,
    };

    const size_t node_ceil = (1LLU << t->node_ceil2);

    /* First pass: emit any nodes that have no references to them */
    for (size_t i = 0; i < node_ceil; i++) {
        if (!t->nodes[i].used) { continue; }
        if (!t->nodes[i].connected) { report_disconnected(&env, i); }
    }

    for (size_t i = 0; i < node_ceil; i++) {
        if (!t->nodes[i].used) { continue; }
        if (!strongconnect(&env, i, 0)) {
            LOG("%s: strongconnect failure\n", __func__);
            free(env.scc_buf);
            return false;
        }
    }

    free(env.scc_buf);
    return true;
}

enum hopscotch_error
hopscotch_error(struct hopscotch *t) {
    return t->error;
}

static bool init_node(struct hopscotch *t, uint32_t node_id,
    uint8_t hint, bool connected) {
    if (hint == 0) { hint = DEF_SUCC_CEIL2; }
    assert(node_id < (1LLU << t->node_ceil2));
    struct node *n = &t->nodes[node_id];
    if (n->succ != NULL) {
        LOG("%s: already initialized, returning\n", __func__);
        if (connected) { n->connected = true; }
        return true;
    }

    uint32_t *succ = calloc(1LLU << hint, sizeof(uint32_t));
    if (succ == NULL) {
        t->error = HOPSCOTCH_ERROR_MEMORY;
        return false;
    }
    n->succ = succ;
    n->succ_ceil = hint;
    n->succ_count = 0;
    n->used = true;
    n->connected = connected;
    return true;
}

static bool grow_nodes(struct hopscotch *t, uint32_t new_max_id) {
    uint8_t nceil2 = t->node_ceil2;
    while ((1LLU << nceil2) <= new_max_id) {
        nceil2++;
    }
    const size_t ncount = 1LLU << nceil2;
    const size_t nsize = ncount * sizeof(t->nodes[0]);

    struct node *nnodes = realloc(t->nodes, nsize);
    LOG("%s: growing from %u to %u, %p\n",
        __func__, t->node_ceil2, nceil2, (void *)nnodes);
    if (nnodes == NULL) {
        t->error = HOPSCOTCH_ERROR_MEMORY;
        return false;
    }

    const size_t ocount = (1LLU << t->node_ceil2);
    for (size_t i = ocount; i < ncount; i++) {
        struct node n = {
            .id = i,
            .index = NO_INDEX,
        };
        memcpy(&nnodes[i], &n, sizeof(n));
    }

    t->nodes = nnodes;
    t->node_ceil2 = nceil2;
    return true;
}

static void report_disconnected(struct solve_env *env, uint32_t node_id) {
    struct node *n = &env->t->nodes[node_id];
    if (env->cb != NULL) {
        uint32_t buf[1] = { n->id, };
        env->cb(env->scc_id, 1, buf, env->udata);
    }
    env->scc_id++;
    n->used = false;
    free(n->succ);
    n->succ = NULL;
}

#define MIN(X, Y) (X < Y ? X : Y)

static int cmp_uint32_t(const void *va, const void *vb) {
    uint32_t a = *(const uint32_t *)va;
    uint32_t b = *(const uint32_t *)vb;
    return (a < b ? -1 : a > b ? 1 : 0);
}

static bool strongconnect(struct solve_env *env,
    uint32_t node_id, size_t depth) {
    LOG("%s: node_id %u\n", __func__, node_id);

    if (depth >= env->max_depth) {
        env->t->error = HOPSCOTCH_ERROR_RECURSION_DEPTH;
        return false;
    }

    struct hopscotch *t = env->t;
    struct node *n = &t->nodes[node_id];
    assert(n->id == node_id);
    assert(n->used);

    if (n->index != NO_INDEX) {
        LOG("%s: already processed\n", __func__);
        return true;            /* node already processed */
    } else {
        assert(n->index == NO_INDEX);
        assert(n->lowlink == 0);
        n->index = t->index;
        n->lowlink = t->index;
        t->index++;
        if (!push_node(t, node_id)) { return false; }

        LOG("%s: processing node %u, (index %u, lowlink %u, succ_count %zu)\n",
            __func__, node_id, n->index, n->lowlink, n->succ_count);

        /* consider successors of node */
        for (size_t si = 0; si < n->succ_count; si++) {
            uint32_t s_id = n->succ[si];
            assert(s_id < (1LLU << t->node_ceil2));

            struct node *s = &t->nodes[s_id];

            LOG("%s: checking successor %u\n", __func__, s_id);

            if (s->index == NO_INDEX) {
                /* not yet visited -- recurse on it */
                LOG("%s: not yet visited, recursing\n", __func__);
                if (!strongconnect(env, s_id, depth + 1)) {
                    return false;
                }

                n->lowlink = MIN(n->lowlink, s->lowlink);
                LOG("%s: node %u lowlink now %u\n", __func__, n->id, n->lowlink);
            } else if (is_stacked(t, s_id)) {
                LOG("%s: successor already stacked\n", __func__);

                /* "Note: The next line may look odd - but is correct.
                 * It says w.index not w.lowlink; that is deliberate
                 * and from the original paper." where 'w' is 's') */
                n->lowlink = MIN(n->lowlink, s->index);
                LOG("%s: node %u lowlink now %u\n", __func__, n->id, n->lowlink);
            }
        }

        /* If n is a root node, then pop the stack and generate an SCC. */
        if (n->lowlink == n->index) {
            /* start a new SCC */
            LOG("%s: root node found, starting group\n", __func__);

            size_t used = 0;
            uint32_t edge;
            do {
                edge = pop_node(t);
                assert(edge < (1LLU << t->node_ceil2));
                if (used >= (1LLU << env->scc_buf_ceil)) {
                    uint8_t nceil = env->scc_buf_ceil + 1;
                    uint32_t *nbuf = realloc(env->scc_buf,
                        (1LLU << nceil) * sizeof(*nbuf));
                    if (nbuf == NULL) { return false; }
                    env->scc_buf_ceil = nceil;
                    env->scc_buf = nbuf;
                }
                env->scc_buf[used] = edge;
                used++;
                LOG("%s: added node %u, %zd in group\n",
                    __func__, edge, used);
            } while (edge != node_id);

            /* Sorting could be optional, but commenting out sorting has
             * very little impact on benchmarks. */
            qsort(env->scc_buf, used, sizeof(env->scc_buf[0]), cmp_uint32_t);

            /* Note: The SCCs are output in reverse topological order. */
            if (env->cb) {
                env->cb(env->scc_id, used, env->scc_buf, env->udata);
            }
            env->scc_id++;
        }
        return true;
    }
}

static bool is_stacked(struct hopscotch *t, uint32_t node_id) {
    return t->nodes[node_id].stacked;
}

static bool push_node(struct hopscotch *t, uint32_t node_id) {
    if (t->stack_top == (1LLU << t->stack_ceil2)) { /* grow? */
        const uint8_t nceil2 = t->stack_ceil2 + 1;
        LOG("%s: growing stack from %zu to %zu\n",
            __func__, (size_t)(1LLU << t->stack_ceil2),
            (size_t)(1LLU << nceil2));
        uint32_t *nstack = realloc(t->stack,
            (1LLU << nceil2) * sizeof(t->stack[0]));
        if (nstack == NULL) {
            LOG("%s: stack realloc failure\n", __func__);
            return false;
        }
        t->stack_ceil2 = nceil2;
        t->stack = nstack;
    }

    t->stack[t->stack_top++] = node_id;
    t->nodes[node_id].stacked = true;
    return true;
}

static uint32_t pop_node(struct hopscotch *t) {
    assert(t->stack_top > 0);
    uint32_t res = t->stack[--t->stack_top];
    LOG("%s: popping %u\n", __func__, res);
    assert(t->nodes[res].stacked);
    t->nodes[res].stacked = false;
    return res;
}
