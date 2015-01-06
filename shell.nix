{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
let
  sparseLinear = haskellPackages.callPackage ../sparse-linear {};
in
haskellPackages.callPackage ./. {
  inherit sparseLinear;
  suitesparse = haskellPackages.callPackage ../suitesparse {
    inherit sparseLinear;
    suitesparse = suitesparse_4_4_1;
  };
  globalLock = haskellPackages.callPackage ./global-lock.nix {};
}
