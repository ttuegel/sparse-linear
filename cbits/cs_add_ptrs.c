#include "cs.h"

cs_ci* cs_ci_add_ptrs (const cs_ci* A, const cs_ci* B, cs_complex_t* alpha, cs_complex_t* beta)
{
  return (cs_ci_add(A, B, *alpha, *beta));
}

cs_di* cs_di_add_ptrs (const cs_di* A, const cs_di* B, double* alpha, double* beta)
{
  return (cs_di_add(A, B, *alpha, *beta));
}
