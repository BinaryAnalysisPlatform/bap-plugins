#include <stdio.h>
#include <stdlib.h>

#define MIN 0
#define MAX 10
#define SECRET_PASS 0
#define MODE_EASY 1

int * p_global;

int cmp () { return * p_global >= MIN && * p_global <= MAX ; }

void index_user ( int *p) {
  int * p_global_save ;
  p_global_save = p_global ;
  p_global =p;
  if( cmp () <=0) {
   return;
  }
   p_global = p_global_save ; return ;
}

int main (int argc , char * argv []) {
 int * p_index;
 int val;

 p_global =( int *) malloc ( sizeof ( int ) ) ;
 * p_global = SECRET_PASS ;

 p_index =( int *) malloc ( sizeof ( int ) ) ;
 *p_index = 100;
 index_user(p_index) ;
 free(p_index);

 val = *p_global;

 return 0;
}
