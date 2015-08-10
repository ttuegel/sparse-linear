{ mkDerivation, base, base-orphans, binary, hspec, mono-traversable
, primitive, QuickCheck, stdenv, tagged, vector, vector-algorithms
}:
mkDerivation {
  pname = "sparse-linear";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base base-orphans binary mono-traversable primitive tagged vector
    vector-algorithms
  ];
  testDepends = [ base hspec mono-traversable QuickCheck vector ];
  description = "Sparse linear algebra primitives in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
