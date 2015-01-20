sparse-linear
=============

Traditional sparse linear algebra in Haskell

[![Build Status](https://travis-ci.org/ttuegel/sparse-linear.svg?branch=master)](https://travis-ci.org/ttuegel/sparse-linear)

`sparse-linear` provides sparse matrices in Haskell. Traditional matrix formats
(coordinate list, compressed row, compressed column) are provided for
interoperability with existing libraries in other languages.

Unlike other sparse linear algebra libraries in Haskell, `sparse-linear` uses
the type system to ensure efficiency. The provided `Matrix` type reflects the
storage format used so that the most efficient types for each operation can be
required at compile-time. By supporting multiple matrix formats, `sparse-linear`
can also efficiently support a wider variety of operations than other Haskell
sparse linear algebra libraries. For example, `sparse-linear` is the only
library at this time to support sparse-matrix/dense-vector products, a common
idiom in sparse algorithms.
