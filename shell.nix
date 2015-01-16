with (import <nixpkgs> {});
let
  amd = null;
  cholmod = null;
  colamd = null;
  sparse-linear = haskellngPackages.callPackage ./sparse-linear {};
  suitesparseconfig = suitesparse_4_4_1;
  umfpack = null;
  suitesparse = haskellngPackages.callPackage ./. {
    inherit amd cholmod colamd sparse-linear suitesparseconfig umfpack;
  };
in
suitesparse.env
