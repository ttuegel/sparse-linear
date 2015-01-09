{ cabal, hspec, monoTraversable, QuickCheck, storableComplex
, tagged, vector, vectorAlgorithms }:

cabal.mkDerivation (self: {
  pname = "sparse-linear";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    monoTraversable storableComplex tagged vector vectorAlgorithms
  ];
  testDepends = [ hspec QuickCheck vector ];
  extraLibraries = [ ];
  configureFlags = [ "-O2" ];
  meta = {
    description = "Sparse linear algebra primitives in Haskell";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
