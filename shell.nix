with (import <nixpkgs> {});
with stdenv.lib;
let
  enableOptimization = ttuegel.enableOptimization or id;
  enableProfiling = ttuegel.enableProfiling or id;
  amd = null;
  cholmod = null;
  colamd = null;
  sparse-linear = haskellngPackages.callPackage ./sparse-linear {};
  suitesparseconfig = suitesparse_4_4_1;
  umfpack = null;
  pkg = haskellngPackages.callPackage ./. {
    inherit amd cholmod colamd sparse-linear suitesparseconfig umfpack;
  };
in
  (fold (f: x: f x) pkg
    [
      enableOptimization
      enableProfiling
    ]).env
