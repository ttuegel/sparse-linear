{ mkDerivation, base, hmatrix, hspec, mono-traversable
, primitive, QuickCheck, stdenv, suitesparse, vector
, vector-algorithms
, openblasCompat, haskellPackages
}:

let
  inherit (haskellPackages) callPackage;
  sparse-linear = callPackage ../sparse-linear {};
  openblas = openblasCompat;
in
callPackage ./suitesparse.nix {
  inherit mkDerivation base hmatrix hspec mono-traversable openblas
          primitive QuickCheck sparse-linear stdenv suitesparse vector
          vector-algorithms;
}
