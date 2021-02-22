#ifndef SYMTAB_H
#define SYMTAB_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

struct symtab;

struct symtab_symbol {
    const size_t id;
    const size_t len;
    const uint8_t str[];
};

struct symtab_config {
    uint8_t bucket_ceil2;
    uint8_t bucket_symbols_ceil2;
    uint8_t symbols_hint;
};

struct symtab *
symtab_new(struct symtab_config *config);

void
symtab_free(struct symtab *s);

enum symtab_intern_res {
    SYMTAB_INTERN_EXISTING,
    SYMTAB_INTERN_CREATED,
    SYMTAB_INTERN_ERROR_NULL = -1,
    SYMTAB_INTERN_ERROR_MEMORY = -2,
};

enum symtab_intern_res
symtab_intern(struct symtab *s, uint8_t len, const uint8_t *str,
    struct symtab_symbol **s_out);

struct symtab_symbol *
symtab_get(struct symtab *s, size_t id);

#endif
