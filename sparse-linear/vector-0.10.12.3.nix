{ mkDerivation, base, deepseq, ghc-prim, primitive, QuickCheck
, random, stdenv, template-haskell, test-framework
, test-framework-quickcheck2, transformers
}:
mkDerivation {
  pname = "vector";
  version = "0.10.12.3";
  sha256 = "16p8i0gvc9d4n9mxlhlnvrl2s0gmgd7kcsk5czdzz2cd4gh5qxhg";
  libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
  testHaskellDepends = [
    base QuickCheck random template-haskell test-framework
    test-framework-quickcheck2 transformers
  ];
  homepage = "https://github.com/haskell/vector";
  description = "Efficient Arrays";
  license = stdenv.lib.licenses.bsd3;
}
