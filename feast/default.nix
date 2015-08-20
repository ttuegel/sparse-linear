{ mkDerivation, base, feast, global-lock, hmatrix, hspec, mtl
, primitive, QuickCheck, sparse-linear, stdenv, suitesparse
, transformers, vector
}:
mkDerivation {
  pname = "feast";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base global-lock hmatrix mtl primitive sparse-linear suitesparse
    transformers vector
  ];
  librarySystemDepends = [ feast ];
  testHaskellDepends = [
    base hspec QuickCheck sparse-linear suitesparse vector
  ];
  description = "Haskell bindings to the FEAST eigensolver library";
  license = stdenv.lib.licenses.gpl2;
}
