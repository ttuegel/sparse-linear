{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;

haskellPackages.callPackage ./. {
  globalLock = haskellPackages.callPackage ./global-lock.nix {};
}
