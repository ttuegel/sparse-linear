# sparse-linear

The libraries herein are works-in-progress, but suitable for
experimentation.

`sparse-linear` aims to be a basic set of routines for constructing
and manipulating sparse and dense matrices in a variety of
formats. Matrices can be constructed using high-level combinators
because stream fusion (provided by the `vector` library) is effective
at eliminating intermediate values. Low-level operations akin to
BLAS are also provided.

`suitesparse` is a set of bindings to the SuiteSparse libraries. At this
time, only bindings for the UMFPACK library (sparse, multifrontal LU
factorization) are implemented. The bindings utilize the data formats
provided by `sparse-linear`.

`feast` is a set of bindings to the FEAST eigensolver. The bindings
utilize `sparse-linear` for data formats and `suitesparse` for
factorization.