{ mkDerivation, amd, base, cholmod, colamd, hmatrix, hspec
, mono-traversable, primitive, QuickCheck, sparse-linear, stdenv
, suitesparseconfig, umfpack, vector, vector-algorithms
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
  extraLibraries = [ amd cholmod colamd suitesparseconfig umfpack ];
  description = "Haskell bindings to the SuiteSparse library of sparse linear algebra routines";
  license = stdenv.lib.licenses.gpl2;
}
