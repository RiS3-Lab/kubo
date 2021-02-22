/*
 * Copyright (c) 2017-18 Scott Vokes <vokes.s@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <stdio.h>

#include <stdint.h>
#include <stdbool.h>

#include <assert.h>
#include <err.h>
#include <errno.h>
#include <string.h>

#include <getopt.h>

#include "hopscotch.h"
#include "symtab.h"
#include "SCC.h"

char buf[64 * 1024LLU];

#define DEF_LINE_IDS_CEIL 30

struct main_env {
    struct hopscotch *t;
    struct symtab *s;
    bool dot;

    FILE *in;

    /* Buffer for IDs read from the current line, grown on demand */
    uint8_t line_ids_ceil;
    uint32_t *line_ids;
};

/* static bool
 * intern(struct main_env *env, size_t len, const char *name,
 *     struct symtab_symbol **s, uint32_t *id); */

static void usage(const char *msg) {
    if (msg) { fprintf(stderr, "%s\n\n", msg); }
    fprintf(stderr, "hopscotch v. %d.%d.%d by %s\n",
        HOPSCOTCH_VERSION_MAJOR, HOPSCOTCH_VERSION_MINOR,
        HOPSCOTCH_VERSION_PATCH, HOPSCOTCH_AUTHOR);
    fprintf(stderr,
        "Usage: hopscotch [-d] [input_file]\n"
        );
    exit(1);
}

static void handle_args(struct main_env *env, int argc, char **argv) {
    int fl;
    while ((fl = getopt(argc, argv, "dh")) != -1) {
        switch (fl) {
        case 'd':               /* dot */
            env->dot = true;
            break;
        case 'h':               /* help */
            usage(NULL);
            break;
        case '?':
        default:
            usage(NULL);
        }
    }

    argc -= (optind - 1);
    argv += (optind - 1);

    if (argc > 1) {
        env->in = fopen(argv[1], "r");
        if (env->in == NULL) {
            err(1, "fopen: %s", argv[1]);
        }
    }
}

static void
print_cb(uint32_t group_id, size_t count, const uint32_t *group, void *udata);

#define MAX_LENGTH 256

struct symbol {
    uint8_t len;
    char name[MAX_LENGTH];
};

static const char *
getenv_attr(const char *key) {
    const char *res = getenv(key);
    if (res == NULL) { return ""; }
    return res;
}

static const char *indent_regular = "    ";
static const char *indent_cluster = "        ";

FILE* out_file;
uint32_t** result_buf;
int result_idx;

static void print_cb(uint32_t group_id,
        size_t group_count, const uint32_t *group, void *udata) {
    (void)udata;
    struct main_env *env = (struct main_env *)udata;
    assert(env);

    if (env->dot) {
        bool cluster = group_count > 1;
        const char *indent = indent_regular;
        if (cluster) {
            fprintf(out_file,"%ssubgraph cluster_%d {\n", indent, group_id);
            indent = indent_cluster;
            fprintf(out_file,"%sgraph [%s];\n", indent, getenv_attr("HOPSCOTCH_DOT_CLUSTER_ATTR"));
        }
        result_buf[result_idx] = (uint32_t*)malloc((group_count + 1) * sizeof(uint32_t));
        result_buf[result_idx][0] = group_count;
        for (size_t g_i = 0; g_i < group_count; g_i++) {
            const uint32_t root_id = group[g_i];
            result_buf[result_idx][g_i+1] = root_id;
            fprintf(out_file,"%sn%u [label=\"%u\"];\n", indent, root_id, root_id);
        }
        result_idx++;

        if (cluster) {
            fprintf(out_file,"    }\n");
            indent = indent_regular;
        }

        for (size_t g_i = 0; g_i < group_count; g_i++) {
            const uint32_t root_id = group[g_i];

            const uint32_t *successors = NULL;
            size_t succ_count = 0;
            if (!hopscotch_get_successors(env->t, root_id, &succ_count, &successors)) {
                assert(false);
            }

            for (size_t s_i = 0; s_i < succ_count; s_i++) {
                const uint32_t edge_id = successors[s_i];
                fprintf(out_file,"%sn%u -> n%u\n", indent, root_id, edge_id);
            }
        }

        fprintf(out_file,"\n");

    } else {
        fprintf(out_file,"%u: ", group_id);
        
        result_buf[result_idx] = (uint32_t*)malloc((group_count + 1) * sizeof(uint32_t));
        result_buf[result_idx][0] = group_count;
        
        for (size_t i = 0; i < group_count; i++) {
            
            const uint32_t root_id = group[i];

            //const struct symtab_symbol *sym = symtab_get(env->s, root_id);
            //assert(sym);
            
            result_buf[result_idx][i+1] = root_id;
            fprintf(out_file,"%u ", root_id);
        }
        result_idx++;
        fprintf(out_file,"\n");
    }
}

static void print_cb1(uint32_t group_id,
        size_t group_count, const uint32_t *group, void *udata) {
    (void)udata;
    struct main_env *env = (struct main_env *)udata;
    assert(env);

    if (env->dot) {
        bool cluster = group_count > 1;
        const char *indent = indent_regular;
        if (cluster) {
            fprintf(out_file,"%ssubgraph cluster_%d {\n", indent, group_id);
            indent = indent_cluster;
            fprintf(out_file,"%sgraph [%s];\n", indent, getenv_attr("HOPSCOTCH_DOT_CLUSTER_ATTR"));
        }

        for (size_t g_i = 0; g_i < group_count; g_i++) {
            const uint32_t root_id = group[g_i];
            const struct symtab_symbol *sym = symtab_get(env->s, root_id);
            assert(sym);
            fprintf(out_file,"%sn%u [label=\"%s\"];\n", indent, root_id, sym->str);
        }

        if (cluster) {
            fprintf(out_file,"    }\n");
            indent = indent_regular;
        }

        for (size_t g_i = 0; g_i < group_count; g_i++) {
            const uint32_t root_id = group[g_i];

            const uint32_t *successors = NULL;
            size_t succ_count = 0;
            if (!hopscotch_get_successors(env->t, root_id, &succ_count, &successors)) {
                assert(false);
            }

            for (size_t s_i = 0; s_i < succ_count; s_i++) {
                const uint32_t edge_id = successors[s_i];
                fprintf(out_file,"%sn%u -> n%u\n", indent, root_id, edge_id);
            }
        }

        fprintf(out_file,"\n");

    } else {
        fprintf(out_file,"%u: ", group_id);
        for (size_t i = 0; i < group_count; i++) {
            const struct symtab_symbol *sym = symtab_get(env->s, group[i]);
            assert(sym);
            fprintf(out_file,"%s ", sym->str);
        }
        fprintf(out_file,"\n");
    }
}

int analyze_scc(int num_ent,struct SCC_ENT *ents[],bool write_dot,FILE* out_f,uint32_t** result) {
    int res = EXIT_SUCCESS;
    struct main_env env = {
        .in = stdin,
    };
    //handle_args(&env, argc, argv);

    result_buf = result;
    result_idx = 0;
    env.in = NULL;
    out_file = out_f;
    env.dot = write_dot;
    env.t = hopscotch_new();
    assert(env.t);
    env.s = symtab_new(NULL);
    assert(env.s);

    env.line_ids_ceil = DEF_LINE_IDS_CEIL;
    env.line_ids = malloc((1LLU << DEF_LINE_IDS_CEIL) * sizeof(*env.line_ids));

    int counter_ent = 0;
    for (;counter_ent < num_ent; counter_ent++) {
        struct SCC_ENT* ent = ents[counter_ent];
        if (!hopscotch_add(env.t, ent->id, ent->count, ent->successors)) {
            res = EXIT_FAILURE;
            goto cleanup;
        }
    }

    if (!hopscotch_seal(env.t)) {
        res = EXIT_FAILURE;
        goto cleanup;
    }

    if (env.dot) {
        const char *indent = "    ";
        fprintf(out_file,"digraph {\n");
        fprintf(out_file,"%sgraph [%s];\n", indent, getenv_attr("HOPSCOTCH_DOT_GRAPH_ATTR"));
        fprintf(out_file,"%snode [%s];\n", indent, getenv_attr("HOPSCOTCH_DOT_NODE_ATTR"));
        fprintf(out_file,"%sedge [%s];\n", indent, getenv_attr("HOPSCOTCH_DOT_EDGE_ATTR"));
    }

    if (!hopscotch_solve(env.t, 0, print_cb, &env)) {
        res = EXIT_FAILURE;
        goto cleanup;
    }

    if (env.dot) { fprintf(out_file,"}\n"); }

cleanup:
    symtab_free(env.s);
    hopscotch_free(env.t);
    return res;
}
