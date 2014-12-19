#include "cs.h"

cs_complex_t* cs_ci_diag(const cs_ci* A)
{
  const int nd = A->m < A->n ? A->m : A->n;
  cs_complex_t* D = cs_ci_malloc(nd, sizeof(cs_complex_t));

  int c;
  for (c = 0; c < nd; ++c)
  {
    D[c] = 0;

    const int ixend = A->p[c + 1];
    int ix;
    for (ix = A->p[c]; ix < ixend; ++ix)
    {
      if (A->i[ix] == c)
      {
        D[c] = A->x[ix];
        break;
      }
    }
  }

  return D;
}
