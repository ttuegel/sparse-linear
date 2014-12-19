#include "cs.h"

cs_ci* cs_ci_kron(const cs_ci* A, const cs_ci* B)
{
  if (A->nz >= 0 || B->nz >= 0)
    return 0;

  const int nr = A->m * B->m;
  const int nc = A->n * B->n;
  const int nz = A->nzmax * B->nzmax;
  cs_ci* C = cs_ci_spalloc(nr, nc, nz, nz, 0);

  /* ci is always the next index in C->x that will be written */
  int ca, cb, ai, bi, ci = 0;
  const int nca = A->n;
  const int ncb = B->n;
  /* iterate over the columns of each matrix */
  for (ca = 0; ca < nca; ++ca)
  {
    for (cb = 0; cb < ncb; ++cb)
    {
      /* write the start index of the current column of C */
      C->p[ca * B->n + cb] = ci;

      /* iterate over the values in each column */
      const int aiend = A->p[ca + 1];
      const int biend = B->p[cb + 1];
      for (ai = A->p[ca]; ai < aiend; ++ai)
      {
        for (bi = B->p[cb]; bi < biend; ++bi)
        {
          C->x[ci] = A->x[ai] * B->x[bi];
          C->i[ci] = A->i[ai] * B->m + B->i[bi];
          ++ci;
        }
      }

    }
  }

  /* write the length of C->x into the trailing column pointer */
  C->p[nc] = ci;

  return C;
}
