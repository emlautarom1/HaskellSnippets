#include <stdio.h>

typedef struct _
{
  int a;
  int b;
} MyStruct;

int do_something(MyStruct *x)
{
  return x->a * x->b;
}