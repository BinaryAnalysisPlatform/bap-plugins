/*
 In this test, we see both explicit mallocs, and so detect the use in printf.
 In the second example, we will see only one malloc which wraps the real malloc (as with the gnu-nettool example).
 In this case, we need to track explicitly the sizes of the original malloc, if we have any hopes of accurately detecting a free.
 */

/* Also: bug in conqueror on return values */

#include <stdlib.h>
#include <stdio.h>

struct Vector {
    double *data;
    size_t size;
};

struct Vector *newVector (size_t sz) {
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

void delVector (struct Vector *vector) {
    // Can safely assume vector is NULL or fully built.
    if (vector != NULL) {
        free (vector->data);
        free (vector);
    }
}

int main(void) {
  struct Vector *v = newVector(1);
  delVector(v);

  printf("%f\n", *v->data);
}

/*
vagrant@vagrant-ubuntu-trusty-64:~/rvantonder-bap-plugins/uaf/tests/all$ gcc -o structs structs.c 
vagrant@vagrant-ubuntu-trusty-64:~/rvantonder-bap-plugins/uaf/tests/all$ valgrind --tool=memcheck ./structs
==27309== Memcheck, a memory error detector
==27309== Copyright (C) 2002-2013, and GNU GPL'd, by Julian Seward et al.
==27309== Using Valgrind-3.10.0.SVN and LibVEX; rerun with -h for copyright info
==27309== Command: ./structs
==27309== 
==27309== Invalid read of size 8
==27309==    at 0x400686: main (in /home/vagrant/rvantonder-bap-plugins/uaf/tests/all/structs)
==27309==  Address 0x51fc040 is 0 bytes inside a block of size 16 free'd
==27309==    at 0x4C2BDEC: free (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
==27309==    by 0x40065D: delVector (in /home/vagrant/rvantonder-bap-plugins/uaf/tests/all/structs)
==27309==    by 0x400681: main (in /home/vagrant/rvantonder-bap-plugins/uaf/tests/all/structs)
==27309== 
==27309== Invalid read of size 8
==27309==    at 0x400689: main (in /home/vagrant/rvantonder-bap-plugins/uaf/tests/all/structs)
==27309==  Address 0x51fc090 is 0 bytes inside a block of size 8 free'd
==27309==    at 0x4C2BDEC: free (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
==27309==    by 0x400651: delVector (in /home/vagrant/rvantonder-bap-plugins/uaf/tests/all/structs)
==27309==    by 0x400681: main (in /home/vagrant/rvantonder-bap-plugins/uaf/tests/all/structs)
==27309== 
0.000000
==27309== 
==27309== HEAP SUMMARY:
==27309==     in use at exit: 0 bytes in 0 blocks
==27309==   total heap usage: 2 allocs, 2 frees, 24 bytes allocated
==27309== 
==27309== All heap blocks were freed -- no leaks are possible
==27309== 
==27309== For counts of detected and suppressed errors, rerun with: -v
==27309== ERROR SUMMARY: 2 errors from 2 contexts (suppressed: 0 from 0)
*/
