#include "cs.h"

int sizeof_cs_ci = sizeof(cs_ci);

void mk_cs_ci(cs_ci* cs, int nzmax, int m, int n, int* p, int* i, cs_complex_t* x, int nz)
{
  cs->nzmax = nzmax;
  cs->m = m;
  cs->n = n;
  cs->p = p;
  cs->i = i;
  cs->x = x;
  cs->nz = nz;
}

void match_cs_ci(cs_ci* cs, int* nzmax, int* m, int* n, int** p, int** i, cs_complex_t** x, int* nz)
{
  if (nzmax)
    *nzmax = cs->nzmax;
  if (m)
    *m = cs->m;
  if (n)
    *n = cs->n;
  if (p)
    *p = cs->p;
  if (i)
    *i = cs->i;
  if (x)
    *x = cs->x;
  if (nz)
    *nz = cs->nz;
}
