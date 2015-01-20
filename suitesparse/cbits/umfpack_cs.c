#include "cs.h"
#include "umfpack.h"

int umfpack_cs_ci_symbolic(const cs_ci* A, void** symbolic, const double* control, double* info)
{
  int nr = A->m;
  int nc = A->n;
  return (umfpack_zi_symbolic(nr, nc, A->p, A->i, (double*) A->x, 0, symbolic, control, info));
}

int umfpack_cs_ci_numeric(const cs_ci* A, void* symbolic, void** numeric, const double* control, double* info)
{
  return (umfpack_zi_numeric(A->p, A->i, (double*) A->x, 0, symbolic, numeric, control, info));
}

int umfpack_cs_ci_solve(const cs_ci* A, cs_complex_t* X, const cs_complex_t* B, void* numeric, const double* control, double* info)
{
  return (umfpack_zi_solve(UMFPACK_A, A->p, A->i, (double*) A->x, 0, (double*) X, 0, (double*) B, 0, numeric, control, info));
}

int umfpack_cs_di_symbolic(const cs_di* A, void** symbolic, const double* control, double* info)
{
  int nr = A->m;
  int nc = A->n;
  return (umfpack_di_symbolic(nr, nc, A->p, A->i, A->x, symbolic, control, info));
}

int umfpack_cs_di_numeric(const cs_di* A, void* symbolic, void** numeric, const double* control, double* info)
{
  return (umfpack_di_numeric(A->p, A->i, A->x, symbolic, numeric, control, info));
}

int umfpack_cs_di_solve(const cs_di* A, double* X, const double* B, void* numeric, const double* control, double* info)
{
  return (umfpack_di_solve(UMFPACK_A, A->p, A->i, A->x, X, B, numeric, control, info));
}
