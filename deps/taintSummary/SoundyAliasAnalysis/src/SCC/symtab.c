#include "symtab.h"

#include <string.h>
#include <assert.h>

#include <stdio.h>

#if 0
#define LOG(...) fprintf(stderr, __VA_ARGS__)
#else
#define LOG(...)
#endif

#define DEF_BUCKET_CEIL22 6
#define DEF_BUCKET_SYMBOLS_HINT 3
#define DEF_SYMBOLS_HINT 2

/* Hash bucket, which contains a vector of symbols.
 * The vector starts out as NULL, and is allocated / grown on demand. */
struct bucket {
    uint8_t symbols_ceil;
    struct symtab_symbol **symbols;
};

struct symtab {
    const uint8_t bucket_bits;
    const uint8_t def_bucket_symbols_ceil;
    const size_t bucket_count;
    struct bucket *buckets;
    size_t next_id;

    uint8_t symbols_ceil;
    struct symtab_symbol **symbols;
};

struct symtab *
symtab_new(struct symtab_config *config) {
    struct symtab *res = malloc(sizeof(*res));
    if (res == NULL) { return NULL; }

#define DEF(FIELD, DEFAULT) (config && config->FIELD ? config->FIELD : DEFAULT)
    const uint8_t bucket_ceil2 = DEF(bucket_ceil2, DEF_BUCKET_CEIL22);
    const uint8_t symbols_ceil = DEF(symbols_hint, DEF_SYMBOLS_HINT);
    const uint8_t bucket_symbols_ceil2 = DEF(bucket_symbols_ceil2, DEF_BUCKET_SYMBOLS_HINT);
    const size_t bucket_count = 1LLU << bucket_ceil2;
#undef DEF

    struct bucket *buckets = calloc(bucket_count, sizeof(*buckets));
    if (buckets == NULL) {
        free(res);
        return NULL;
    }

    struct symtab_symbol **symbols = calloc(1LLU << symbols_ceil,
        sizeof(*symbols));
    if (symbols == NULL) {
        free(res);
        free(buckets);
        return NULL;
    }

    struct symtab s = {
        .bucket_bits = bucket_ceil2,
        .def_bucket_symbols_ceil = bucket_symbols_ceil2,
        .bucket_count = bucket_count,
        .buckets = buckets,

        .symbols_ceil = symbols_ceil,
        .symbols = symbols,
    };
    memcpy(res, &s, sizeof(s));
    return res;
}

void
symtab_free(struct symtab *s) {
    for (size_t i = 0; i < s->bucket_count; i++) {
        free(s->buckets[i].symbols);
    }
    free(s->buckets);

    size_t limit = 1LLU << s->symbols_ceil;
    for (size_t i = 0; i < limit; i++) {
        struct symtab_symbol *sym = s->symbols[i];
        if (sym == NULL) { break; }
        free(sym);
    }
    free(s->symbols);
    free(s);
}

/* Fowler/Noll/Vo hash, 64-bit FNV-1a.
 * This hashing algorithm is in the public domain.
 * For more details, see: http://www.isthe.com/chongo/tech/comp/fnv/. */
static const uint64_t fnv64_prime = 1099511628211L;
static const uint64_t fnv64_offset_basis = 14695981039346656037UL;

static uint64_t get_hash(uint8_t len, const uint8_t *str) {
    uint64_t h = fnv64_offset_basis;
    h = (h ^ len) * fnv64_prime;
    for (size_t i = 0; i < len; i++) {
        h = (h ^ str[i]) * fnv64_prime;
    }
    return h;
}

enum symtab_intern_res
symtab_intern(struct symtab *s, uint8_t len, const uint8_t *str,
        struct symtab_symbol **sym_out) {
    if (s == NULL || str == NULL) {
        return SYMTAB_INTERN_ERROR_NULL;
    }

    /* do hash table -> array lookup */
    /* if it doesn't exist, assign an ID, create it,
     * and grow the array (if necessary) / add to the hash table
     * */
    const uint64_t hash = get_hash(len, str);

    const uint64_t bucket_count = 1LLU << s->bucket_bits;
    const size_t b_id = hash & (bucket_count - 1);
    struct bucket *b = &s->buckets[b_id];

    size_t i = 0;

    /* find symbol and return if it already exists */
    size_t symbols_count = 0;
    if (b->symbols != NULL) {
        symbols_count = 1LLU << b->symbols_ceil;
        for (i = 0; i < symbols_count; i++) {
            struct symtab_symbol *sym = b->symbols[i];
            if (sym == NULL) { break; }
            if (sym->len == len && 0 == memcmp(sym->str, str, len)) {
                if (sym_out != NULL) { *sym_out = sym; }
                return SYMTAB_INTERN_EXISTING;
            }
        }
    }

    /* not present, so it needs to be added; ensure there is room. */

    if (b->symbols == NULL) {           /* lazily allocating vector */
        const size_t count = (1LLU << s->def_bucket_symbols_ceil);
        struct symtab_symbol **symbols = calloc(count, sizeof(*symbols));
        if (symbols == NULL) { return SYMTAB_INTERN_ERROR_MEMORY; }
        b->symbols = symbols;
        b->symbols_ceil = s->def_bucket_symbols_ceil;
    } else if (i == symbols_count) {    /* grow vector */
        const uint8_t nceil = b->symbols_ceil + 1;
        const size_t ncount = (1LLU << nceil);
        struct symtab_symbol **nsymbols = realloc(b->symbols,
            ncount * sizeof(*nsymbols));
        if (nsymbols == NULL) { return SYMTAB_INTERN_ERROR_MEMORY; }
        for (size_t n_i = ncount/2; n_i < ncount; n_i++) {
            nsymbols[n_i] = NULL; /* init realloc'd memory */
        }

        b->symbols = nsymbols;
        b->symbols_ceil = nceil;
    } else {
        assert(b->symbols[i] == NULL);  /* can just append */
    }

    /* allocate new symbol */
    struct symtab_symbol *res = malloc(sizeof(*res) + len + 1);
    if (res == NULL) { return SYMTAB_INTERN_ERROR_MEMORY; }
    struct symtab_symbol head = {
        .id = s->next_id,
        .len = len,
    };
    memcpy(res, &head, sizeof(head));
    memcpy((uint8_t *)res->str, str, len);
    memset((uint8_t *)&res->str[len], 0x00, 1);

    /* insert in hash table */
    b->symbols[i] = res;

    /* grow symbol vector, if necessary */
    LOG("res->id %lu, sceil %u, scount %llu\n",
        res->id, s->symbols_ceil, (1LLU << s->symbols_ceil));

    if (res->id == (1LLU << s->symbols_ceil)) {
        const uint8_t nceil = s->symbols_ceil + 1;
        const size_t ncount = 1LLU << nceil;
        LOG("ncount %zu\n", ncount);
        struct symtab_symbol **nsymbols = realloc(s->symbols,
            ncount * sizeof(*nsymbols));
        if (nsymbols == NULL) {
            free(res);
            return SYMTAB_INTERN_ERROR_MEMORY;
        }

        const size_t cur_count = 1LLU << s->symbols_ceil;
        for (size_t i = cur_count; i < ncount; i++) {
            nsymbols[i] = NULL;
        }

        s->symbols_ceil = nceil;
        s->symbols = nsymbols;
    }

    /* insert in symbol vector */
    LOG("== created symbol %p '%s'(%zu) id %zu\n",
        (void *)res, res->str, res->len, res->id);

    assert(s->symbols[res->id] == NULL);
    s->symbols[res->id] = res;

    if (sym_out != NULL) { *sym_out = res; }
    s->next_id++;
    return SYMTAB_INTERN_CREATED;
}

struct symtab_symbol *
symtab_get(struct symtab *s, size_t id) {
    assert(s);
    if (id >= (1LLU << s->symbols_ceil)) { return NULL; }
    return s->symbols[id];
}
