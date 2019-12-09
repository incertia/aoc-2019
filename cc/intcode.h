#ifndef __INTCODE_H__
#define __INTCODE_H__

#include <stddef.h>
#include <stdint.h>

typedef struct vec_t vec_t;

vec_t  *vec_new();
size_t  vec_size(vec_t *v);
int64_t vec_get(vec_t *v, size_t n);
void    vec_set(vec_t *v, size_t n, int64_t d);
void    vec_append(vec_t *v, int64_t d);
void    vec_free(vec_t *v);

vec_t *intcode_run(int64_t *prog, size_t sz, int64_t *in, size_t insz);

#endif
