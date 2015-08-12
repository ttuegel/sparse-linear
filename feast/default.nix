{ mkDerivation, base, feast, global-lock, hmatrix, hspec, mtl
, primitive, QuickCheck, stdenv, transformers, vector
, openblasCompat, haskellPackages
}:

let
  inherit (haskellPackages) callPackage;
  parallel-io-simple = callPackage ./parallel-io-simple {};
  sparse-linear = callPackage ../sparse-linear {};
  suitesparse = callPackage ../suitesparse {};
in
import ./feast.nix {
  inherit mkDerivation base feast global-lock hmatrix hspec mtl
          parallel-io-simple primitive QuickCheck sparse-linear stdenv
          suitesparse transformers vector;
}
