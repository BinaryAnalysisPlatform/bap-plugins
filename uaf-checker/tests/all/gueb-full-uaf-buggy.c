#include <stdio.h>
#include <stdlib.h>

#define MIN 0
#define MAX 10
#define SECRET_PASS 100
#define MODE_EASY 1

int * p_global;

int cmp () { return * p_global >= MIN && * p_global <= MAX ; }

void index_user ( int *p) {
  int * p_global_save ;
  p_global_save = p_global ;
  p_global = p;
  if( cmp () <=0) {
    printf ("The secret is greater than 50\n");
    return;
  }
  printf ("The secret is less than 50 \n") ;
  p_global = p_global_save;
  return;
}

int main (int argc , char * argv []) {
  int * p_index ,* p_pass ;
  if( argc !=2) {
    printf (" ./ uaf MODE \nMODE EASY =1\nMODE HARD !=1\n");
    return 0;
  }

  p_global = (int *) malloc (sizeof (int)) ;
  * p_global = SECRET_PASS ;

  if( atoi (argv [1]) == MODE_EASY) {
    p_index = (int *) malloc (0xff) ;
    printf ("Give a number between 0 and 100\n") ;
    scanf ("%d", p_index) ;
    index_user (p_index) ;
    printf("freeing\n");
    free (p_index) ;
  } else {
    printf ("Good luck !\n") ;
  }

  p_pass = (int *) malloc (0xaa) ;

  printf ("Give the secret\n") ;
  scanf ("%d",p_pass ) ;

  printf("pindex: %d",*p_index);
  printf("pass: %d",*p_pass);
  printf("glbl: %d",*p_global);

  if (* p_pass ==* p_global ) {
    printf ("Congrats !\n") ;
  } else {
    printf ("Sorry ...\n");
  }

  return 0;
}
