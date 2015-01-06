{ cabal, feast, gfortran, globalLock, hmatrix, hspec, monoTraversable
, QuickCheck, sparseLinear, storableComplex, suitesparse, vector }:

cabal.mkDerivation (self: {
  pname = "feast";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    globalLock hmatrix monoTraversable sparseLinear suitesparse vector
  ];
  testDepends = [ hspec QuickCheck vector ];
  extraLibraries = [ feast gfortran.gcc ];
  meta = {
    description = "Haskell bindings to the FEAST eigensolver library";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
