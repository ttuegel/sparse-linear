#include "cs.h"
#include "feast.h"
#include "umfpack_cs.h"
#include <string.h>

void cs_memcpy(void* dst, void* src, int rec, int size)
{
  memcpy(dst, src, size * rec);
}

int feast_cs_ci(const cs_ci* A, const cs_ci* B, double* Emin, double* Emax, int* M, double* E, cs_complex_t* X)
{
  int info = 0;
  char ijob = -1;

  int fpm[64];
  feastinit_(fpm);

  double epsout;
  int loop;
  double res;

  int N = A->n;
  int M0 = *M;
  cs_complex_t Ze;
  cs_complex_t* W1 = cs_malloc(N * M0, sizeof(cs_complex_t));
  cs_complex_t* W2 = cs_malloc(N * M0, sizeof(cs_complex_t));
  cs_complex_t* Aq = cs_malloc(M0 * M0, sizeof(cs_complex_t));
  cs_complex_t* Bq = cs_malloc(M0 * M0, sizeof(cs_complex_t));

  cs_ci* C = 0;
  cs_ci* D = 0;
  void* symbolic;
  void* numeric;

  int c2, xi, xj;
  cs_complex_t* V = cs_calloc(N, sizeof(cs_complex_t));

  while(ijob != 0)
  {
    zfeast_hrci_(&ijob, &N, (double*) &Ze, (double*) W1, (double*) W2, (double*) Aq, (double*) Bq, fpm, &epsout, &loop, Emin, Emax, &M0, E, (double*) X, M, &res, &info);
    switch (ijob)
    {
      case 10:
        /* free up persistent data */
        if (C)
          cs_ci_spfree(C);
        if (symbolic)
          umfpack_zi_free_symbolic(&symbolic);
        if (numeric)
          umfpack_zi_free_numeric(&numeric);

        /* factorize (Ze * B - A) */
        C = cs_ci_add(A, B, -1, Ze);
        umfpack_cs_zi_symbolic(C, &symbolic, 0, 0);
        umfpack_cs_zi_numeric(C, symbolic, &numeric, 0, 0);
        break;
      case 11:
        /* solve (Ze * B - A) * y = work2(1:N, 1:M0)
         * result into work2
         */
        for (c2 = 0; c2 < M0; ++c2)
        {
          for (xi = 0; xi < N; ++xi)
            V[xi] = 0;
          umfpack_cs_zi_solve(C, V, W2 + c2 * N, numeric, 0, 0);
          cs_memcpy(W2 + c2 * N, V, N, sizeof(cs_complex_t));
        }
        break;
      case 20:
        /* factorize (Ze * B - A)^H
         * cannot overwrite factorization from cases 10, 11
         * do nothing if case 21 can reuse factorization from case 10
         */
        D = cs_ci_transpose(C, C->nzmax);
        for (xi = 0; xi < C->nzmax; ++xi)
          D->x[xi] = conj(D->x[xi]);

        /* can reuse symbolic part of factorization only */
        if (numeric)
          umfpack_zi_free_numeric(&numeric);
        umfpack_cs_zi_numeric(D, symbolic, &numeric, 0, 0);
        break;
      case 21:
        /* solve (Ze * B - A)^H * y = work2(1:N, 1:M0)
         * result into work2
         */
        for (c2 = 0; c2 < M0; ++c2)
        {
          for (xi = 0; xi < N; ++xi)
            V[xi] = 0;
          umfpack_cs_zi_solve(D, V, W2 + c2 * N, numeric, 0, 0);
          cs_memcpy(W2 + c2 * N, V, N, sizeof(cs_complex_t));
        }
        break;
      case 30:
        /* multiply A * X(1:N, i:j)
         * result in work1(1:N, i:j)
         * i = fpm(24)
         * j = fpm(24) + fpm(25) - 1
         */
        xj = fpm[23] + fpm[24] - 1;
        for (xi = fpm[23] - 1; xi < xj; ++xi)
        {
          for (xi = 0; xi < N; ++xi)
            V[xi] = 0;
          cs_ci_gaxpy(A, X + xi * N, V);
          cs_memcpy(W1 + xi * N, V, N, sizeof(cs_complex_t));
        }
        break;
      case 40:
        /* multiply B * X(1:N, i:j)
         * result in work1(1:N, i:j)
         * i = fpm(24)
         * j = fpm(24) + fpm(25) - 1
         */
        xj = fpm[23] + fpm[24] - 1;
        for (xi = fpm[23] - 1; xi < xj; ++xi)
        {
          for (xi = 0; xi < N; ++xi)
            V[xi] = 0;
          cs_ci_gaxpy(B, X + xi * N, V);
          cs_memcpy(W1 + xi * N, V, N, sizeof(cs_complex_t));
        }
        break;
      default:
        break;
    }
  }

  if(numeric)
    umfpack_zi_free_numeric(&numeric);

  if(symbolic)
    umfpack_zi_free_symbolic(&symbolic);

  if(C)
    cs_ci_spfree(C);

  if(D)
    cs_ci_spfree(D);

  cs_ci_free(W1);
  cs_ci_free(W2);
  cs_ci_free(Aq);
  cs_ci_free(Bq);
  cs_ci_free(V);

  return info;
}
