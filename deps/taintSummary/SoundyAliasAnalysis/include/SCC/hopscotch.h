#ifndef HOPSCOTCH_H
#define HOPSCOTCH_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

/* Version 0.1.2. */
#define HOPSCOTCH_VERSION_MAJOR 0
#define HOPSCOTCH_VERSION_MINOR 1
#define HOPSCOTCH_VERSION_PATCH 2
#define HOPSCOTCH_AUTHOR "Scott Vokes <vokes.s@gmail.com>"

/* Opaque handle to a Tarjan's Strongly-Connected Components solver */
struct hopscotch;

/* Allocate a new handle. Returns NULL on error. */
struct hopscotch *
hopscotch_new(void);

/* Free a handle. */
void
hopscotch_free(struct hopscotch *t);

/* Add a node and a set of successors to it.
 * If succ_count is 0, then *successors can be NULL.
 * Return false on error (see hopscotch_error). */
bool
hopscotch_add(struct hopscotch *t, uint32_t node_id,
    size_t succ_count, const uint32_t *successors);

/* Note that all nodes / edges have been added to the
 * graph. This must be called before `hopscotch_solve`. */
bool
hopscotch_seal(struct hopscotch *t);

/* Get successors for a node.
 * Only usable after the graph has been sealed. */
bool
hopscotch_get_successors(struct hopscotch *t, uint32_t node_id,
    size_t *succ_count, const uint32_t **successors);

/* hopscotch_solve callback type -- it will be called with
 * a group ID (which increments for each group), the
 * group count, an array of members, and a void pointer
 * passed in to hopscotch_solve. */
typedef void
hopscotch_solve_cb(uint32_t group_id,
    size_t group_count, const uint32_t *group, void *udata);

/* Default limit for recursion in `hopscotch_solve`. */
#define HOPSCOTCH_SOLVE_DEFAULT_MAX_DEPTH 100000LU

/* Attempt to solve the strongly connected components from the assembled
 * and sealed graph. Returns true on success (and calls the callback CB,
 * if provided, with information about each strongly-connected
 * subgraph).
 *
 * On error, returns false and sets the handle's error state.
 *
 * MAX_DEPTH can be used to protect against overflowing the stack via
 * excessive recursion. If set to 0, the default limit will be used.
 *
 * Note: The graph handle is modified during the solving process,
 * so calling `hopscotch_solve` on T multiple times is an error. */
bool
hopscotch_solve(struct hopscotch *t, size_t max_depth,
    hopscotch_solve_cb *cb, void *udata);

/* Get the error for the HOPSCOTCH handle, if any. */
enum hopscotch_error {
    HOPSCOTCH_ERROR_NONE,            /* no error */
    HOPSCOTCH_ERROR_MISUSE,          /* API misuse */
    HOPSCOTCH_ERROR_MEMORY,          /* allocation failure */
    HOPSCOTCH_ERROR_RECURSION_DEPTH, /* exceeded recursion limit */
};
enum hopscotch_error
hopscotch_error(struct hopscotch *t);

#endif
