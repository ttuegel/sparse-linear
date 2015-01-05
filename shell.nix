{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;

haskellPackages.callPackage ./. {
  atlas = atlasWithLapack;
  suitesparse = suitesparse_4_4_1;
  globalLock = haskellPackages.callPackage ./global-lock.nix {};
}
