#ifndef __INTCODE_H__
#define __INTCODE_H__

#include <stddef.h>
#include <stdint.h>

typedef struct vec_t vec_t;

vec_t  *vec_new();
vec_t  *vec_cpy(vec_t *v);
size_t  vec_size(vec_t *v);
int64_t vec_get(vec_t *v, size_t n);
void    vec_set(vec_t *v, size_t n, int64_t d);
void    vec_append(vec_t *v, int64_t d);
void    vec_free(vec_t *v);

typedef struct bt_t bt_t;
bt_t *bt_new();
bt_t *bt_cpy(bt_t *bt);
void bt_insert(bt_t *bt, int64_t k, int64_t v);
int64_t bt_get(bt_t *bt, int64_t k);
size_t bt_size(bt_t *bt);
vec_t *bt_keys(bt_t *bt);
void bt_free(bt_t *bt);

typedef struct intcode_t intcode_t;

#define HALTED      0
#define GAVE_OUTPUT 1
#define NEEDS_INPUT 2

intcode_t *intcode_new(int64_t *prog, size_t sz);
intcode_t *intcode_cpy(intcode_t *m);
vec_t     *intcode_in(intcode_t *m);
vec_t     *intcode_out(intcode_t *m);
int64_t    intcode_mem_get(intcode_t *m, int64_t k);
void       intcode_mem_set(intcode_t *m, int64_t k, int64_t v);
int        intcode_run(intcode_t *m, int io);
void       intcode_free(intcode_t *m);

#endif
