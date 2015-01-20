#ifndef UMFPACK_CS_H
#define UMFPACK_CS_H

#include "cs.h"
#include "umfpack.h"

int umfpack_cs_zi_symbolic(const cs_ci*, void**, const double*, double*);
int umfpack_cs_zi_numeric(const cs_ci*, void*, void**, const double*, double*);
int umfpack_cs_zi_solve(const cs_ci*, cs_complex_t*, const cs_complex_t*, void*, const double*, double*);

#endif
