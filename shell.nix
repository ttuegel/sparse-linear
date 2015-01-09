{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
let
  atlas = atlasWithLapack;
  f77blas = atlasWithLapack;
  gfortran = pkgs.gfortran.gcc;
  lapack = atlasWithLapack;
  sparseLinear = haskellPackages.callPackage ./sparse-linear {};
in
haskellPackages.callPackage ./. {
  inherit atlas f77blas lapack gfortran sparseLinear;
  suitesparse = haskellPackages.callPackage ./suitesparse {
    inherit sparseLinear;
    suitesparse = suitesparse_4_4_1;
  };
  globalLock = haskellPackages.callPackage ./global-lock.nix {};
}
