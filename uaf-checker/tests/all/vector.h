#include <stdlib.h>

struct Vector {
    size_t size;
    double *data;
};

struct Vector *malloc_vector(size_t sz);

void lib_free_vector(struct Vector *vector);
