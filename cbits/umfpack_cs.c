#include "cs.h"
#include "umfpack.h"

int umfpack_cs_zi_symbolic(const cs_ci* A, void** symbolic, const double* control, double* info)
{
  int nr = A->m;
  int nc = A->n;
  return (umfpack_zi_symbolic(nr, nc, A->p, A->i, (double*) A->x, 0, symbolic, control, info));
}

int umfpack_cs_zi_numeric(const cs_ci* A, void* symbolic, void** numeric, const double* control, double* info)
{
  return (umfpack_zi_numeric(A->p, A->i, (double*) A->x, 0, symbolic, numeric, control, info));
}

int umfpack_cs_zi_solve(const cs_ci* A, cs_complex_t* X, const cs_complex_t* B, void* numeric, const double* control, double* info)
{
  return (umfpack_zi_solve(UMFPACK_A, A->p, A->i, (double*) A->x, 0, (double*) X, 0, (double*) B, 0, numeric, control, info));
}
