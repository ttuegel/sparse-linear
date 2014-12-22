{ cabal, feast, hspec, QuickCheck, storableComplex, suitesparse, vector
}:

cabal.mkDerivation (self: {
  pname = "suitesparse";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ storableComplex vector ];
  testDepends = [ hspec QuickCheck ];
  extraLibraries = [ feast suitesparse ];
  meta = {
    description = "Haskell bindings to the SuiteSparse library of sparse linear algebra routines";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
