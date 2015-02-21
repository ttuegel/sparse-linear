{ mkDerivation, atlas, base, feast, gfortran, global-lock, hmatrix
, hspec, lapack, mtl, parallel-io-simple, primitive, ptcblas
, ptf77blas, QuickCheck, sparse-linear, stdenv, suitesparse
, transformers, vector
}:
mkDerivation {
  pname = "feast";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base global-lock hmatrix mtl parallel-io-simple primitive
    sparse-linear suitesparse transformers vector
  ];
  testDepends = [
    base hspec QuickCheck sparse-linear suitesparse vector
  ];
  extraLibraries = [ atlas feast gfortran lapack ptcblas ptf77blas ];
  description = "Haskell bindings to the FEAST eigensolver library";
  license = stdenv.lib.licenses.gpl2;
}
