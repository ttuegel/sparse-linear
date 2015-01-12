{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
let
  atlas = atlasWithLapack;
  gfortran = pkgs.gfortran.gcc;
  lapack = atlasWithLapack;
  ptcblas = atlasWithLapack;
  ptf77blas = atlasWithLapack;
  sparseLinear = haskellPackages.callPackage ./sparse-linear {};
in
haskellPackages.callPackage ./. {
  inherit atlas lapack gfortran ptcblas ptf77blas sparseLinear;
  suitesparse = haskellPackages.callPackage ./suitesparse {
    inherit sparseLinear;
    suitesparse = suitesparse_4_4_1;
  };
  globalLock = haskellPackages.callPackage ./global-lock.nix {};
}
