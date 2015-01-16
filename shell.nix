with (import <nixpkgs> {});
let
  atlas = atlasWithLapack;
  gfortran = pkgs.gfortran.gcc;
  lapack = null;
  ptcblas = null;
  ptf77blas = null;
  sparse-linear = haskellngPackages.callPackage ./sparse-linear {};
  suitesparse = haskellngPackages.callPackage ./suitesparse {
    inherit sparse-linear;
    amd = null;
    cholmod = null;
    colamd = null;
    suitesparseconfig = suitesparse_4_4_1;
    umfpack = null;
  };
  global-lock = haskellngPackages.callPackage ./global-lock.nix {};
in
(haskellngPackages.callPackage ./. {
  inherit atlas lapack gfortran global-lock ptcblas ptf77blas sparse-linear
    suitesparse;
}).env
