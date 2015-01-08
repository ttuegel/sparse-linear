{ cabal, hmatrix, hspec, monoTraversable, QuickCheck, sparseLinear, suitesparse
, vector }:

cabal.mkDerivation (self: {
  pname = "suitesparse";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ hmatrix monoTraversable sparseLinear vector ];
  testDepends = [ hspec QuickCheck vector ];
  extraLibraries = [ suitesparse ];
  configureFlags = [ "-O2" ];
  meta = {
    description = "Haskell bindings to the SuiteSparse library of sparse linear algebra routines";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
