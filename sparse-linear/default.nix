{ mkDerivation, base, binary, hspec, mono-traversable, primitive
, QuickCheck, stdenv, storable-complex, tagged, vector
, vector-algorithms
}:
mkDerivation {
  pname = "sparse-linear";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base binary mono-traversable primitive storable-complex tagged
    vector vector-algorithms
  ];
  testDepends = [ base hspec mono-traversable QuickCheck vector ];
  description = "Sparse linear algebra primitives in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
