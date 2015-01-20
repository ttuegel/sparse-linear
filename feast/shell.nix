with (import <nixpkgs> {});
with haskell-ng.lib;
with stdenv.lib;
let
  enableOptimization = ttuegel.haskell.enableOptimization or id;
  enableProfiling = ttuegel.haskell.enableProfiling or id;
  sparse-linear = haskellngPackages.callPackage ./sparse-linear {};
  suitesparse = haskellngPackages.callPackage ./suitesparse {
    inherit sparse-linear;
    amd = null;
    cholmod = null;
    colamd = null;
    suitesparseconfig = suitesparse_4_4_1;
    umfpack = null;
  };
  pkg = haskellngPackages.callPackage
    ./.
    {
      inherit sparse-linear suitesparse;
      atlas = atlasWithLapack;
      ptf77blas = null;
      ptcblas = null;
      lapack = null;
      gfortran = gfortran.gcc;
      global-lock = haskellngPackages.callPackage ./global-lock.nix {};
    };
in
  (fold (f: x: f x) pkg
    [
      enableOptimization
      enableProfiling
    ]).env
