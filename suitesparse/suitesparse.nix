{ mkDerivation, base, hmatrix, hspec, mono-traversable, openblas
, primitive, QuickCheck, sparse-linear, stdenv, suitesparse, vector
, vector-algorithms
}:
mkDerivation {
  pname = "suitesparse";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base hmatrix mono-traversable primitive sparse-linear vector
    vector-algorithms
  ];
  testDepends = [ base hspec QuickCheck sparse-linear vector ];
  extraLibraries = [ openblas suitesparse ];
  description = "Haskell bindings to the SuiteSparse library of sparse linear algebra routines";
  license = stdenv.lib.licenses.gpl2;
}
