#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main(int argc, char* argv[]){
  int status;
  status=system("sbcl --noinform --load ./test/test-path-checker.lisp");
  assert(status==0);
  return 0;
    }
