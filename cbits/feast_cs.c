#include "cs.h"
#include "feast.h"
#include "umfpack_cs.h"
#include <string.h>

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
  cs_complex_t* work1 = cs_malloc(N * M0, sizeof(cs_complex_t));
  cs_complex_t* work2 = cs_malloc(N * M0, sizeof(cs_complex_t));
  cs_complex_t* Aq = cs_malloc(M0 * M0, sizeof(cs_complex_t));
  cs_complex_t* Bq = cs_malloc(M0 * M0, sizeof(cs_complex_t));

  cs_ci* C = 0;
  cs_ci* D = 0;
  void* symbolic;
  void* numeric;

  int c2, xi, xj;
  cs_complex_t* tmp = cs_calloc(N, sizeof(cs_complex_t));

  while(ijob != 0)
  {
    zfeast_hrci_(&ijob, &N, (double*) &Ze, (double*) work1, (double*) work2, (double*) Aq, (double*) Bq, fpm, &epsout, &loop, Emin, Emax, &M0, E, (double*) X, M, &res, &info);
    switch (ijob)
    {
      case 10:
        /* free up persistent data */
        if (C)
          cs_ci_spfree(C);
        if (symbolic)
          umfpack_zi_free_symbolic(symbolic);
        if (numeric)
          umfpack_zi_free_numeric(numeric);

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
            tmp[xi] = 0;
          umfpack_cs_zi_solve(C, tmp, work2 + c2 * N, numeric, 0, 0);
          memcpy(work2 + c2 * N, tmp, sizeof(cs_complex_t) * N);
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
          umfpack_zi_free_numeric(numeric);
        umfpack_cs_zi_numeric(D, symbolic, &numeric, 0, 0);
        break;
      case 21:
        /* solve (Ze * B - A)^H * y = work2(1:N, 1:M0)
         * result into work2
         */
        for (c2 = 0; c2 < M0; ++c2)
        {
          for (xi = 0; xi < N; ++xi)
            tmp[xi] = 0;
          umfpack_cs_zi_solve(D, tmp, work2 + c2 * N, numeric, 0, 0);
          memcpy(work2 + c2 * N, tmp, sizeof(cs_complex_t) * N);
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
            tmp[xi] = 0;
          cs_ci_gaxpy(A, X + xi * N, tmp);
          memcpy(work1 + xi * N, tmp, sizeof(cs_complex_t) * N);
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
            tmp[xi] = 0;
          cs_ci_gaxpy(B, X + xi * N, tmp);
          memcpy(work1 + xi * N, tmp, sizeof(cs_complex_t) * N);
        }
        break;
      default:
        break;
    }
  }

  if(numeric)
    umfpack_zi_free_numeric(numeric);

  if(symbolic)
    umfpack_zi_free_symbolic(symbolic);

  if(C)
    cs_ci_spfree(C);

  if(D)
    cs_ci_spfree(D);

  cs_free(work1);
  cs_free(work2);
  cs_free(Aq);
  cs_free(Bq);
  cs_free(tmp);

  return info;
}
