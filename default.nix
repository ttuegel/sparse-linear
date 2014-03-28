{ pkgs ? (import ./nixpkgs {}) }:
let
  inherit (pkgs) haskellPackages;
in
haskellPackages.cabal.mkDerivation (self: {
  pname = "sparse-linear";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = with haskellPackages; [
    primitive
    vector
    vectorAlgorithms
  ];
  buildTools = with haskellPackages; [ cabalInstall_1_18_0_3 ];
  enableSplitObjs = false;
})
