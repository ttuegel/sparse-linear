{ cabal, hspec, monoTraversable, QuickCheck, storableComplex
, tagged, vector }:

cabal.mkDerivation (self: {
  pname = "sparse-linear";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ monoTraversable storableComplex tagged vector ];
  testDepends = [ hspec QuickCheck vector ];
  extraLibraries = [ ];
  meta = {
    description = "Sparse linear algebra primitives in Haskell";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
