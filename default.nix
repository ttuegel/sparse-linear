{ cabal, hspec, monoTraversable, QuickCheck, storableComplex
, tagged, vector }:

cabal.mkDerivation (self: {
  pname = "suitesparse";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ monoTraversable storableComplex tagged vector ];
  testDepends = [ hspec QuickCheck vector ];
  extraLibraries = [ ];
  meta = {
    description = "Haskell bindings to the SuiteSparse library of sparse linear algebra routines";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
