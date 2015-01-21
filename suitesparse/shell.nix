with (import <nixpkgs> {});
(haskellngPackages.callPackage ./. {
  amd = null;
  cholmod = null;
  colamd = null;
  sparse-linear = haskellngPackages.callPackage ../sparse-linear {};
  suitesparseconfig = suitesparse_4_4_1;
  umfpack = null;
}).env
