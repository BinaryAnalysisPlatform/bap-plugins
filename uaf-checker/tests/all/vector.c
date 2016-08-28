#include <stdlib.h>

struct Vector {
    size_t size;
    double *data;
};

struct Vector *malloc_vector (size_t sz) {
    // Try to allocate vector structure.
    struct Vector *retVal = malloc (sizeof (struct Vector));
//    if (retVal == NULL)
//        return NULL;

    // Try to allocate vector data, free structure if fail.
    retVal->data = malloc (sz * sizeof (double));
//    if (retVal->data == NULL) {
//        free (retVal);
//        return NULL;
//    }

    // Set size and return.
    retVal->size = sz;
    return retVal;
}

void lib_free_vector (struct Vector *vector) {
    // Can safely assume vector is NULL or fully built.
    if (vector != NULL) {
        free (vector->data);
        free (vector);
    }
}
