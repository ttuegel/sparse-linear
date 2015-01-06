{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;

haskellPackages.callPackage ./. {
  suitesparse = suitesparse_4_4_1;
  sparseLinear = haskellPackages.callPackage ../sparse-linear {};
}
