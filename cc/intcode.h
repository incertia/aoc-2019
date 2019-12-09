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

typedef struct intcode_t intcode_t;

intcode_t *intcode_new(int64_t *prog, size_t sz);
intcode_t *intcode_cpy(intcode_t *m);
vec_t     *intcode_out(intcode_t *m);
int64_t    intcode_mem_get(intcode_t *m, int64_t k);
void       intcode_mem_set(intcode_t *m, int64_t k, int64_t v);
void       intcode_run(intcode_t *m, int64_t *in);
void       intcode_free(intcode_t *m);

#endif
