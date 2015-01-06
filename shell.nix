{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;

haskellPackages.callPackage ./. {
  suitesparse = suitesparse_4_4_1;
  globalLock = haskellPackages.callPackage ./global-lock.nix {};
  sparseLinear = haskellPackages.callPackage ../sparse-linear {};
}
