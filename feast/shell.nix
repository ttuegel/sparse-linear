with (import <nixpkgs> {});
let
  parallel-io-simple = haskellngPackages.callPackage ./parallel-io-simple {};
  sparse-linear = haskellngPackages.callPackage ../sparse-linear {};
  suitesparse = haskellngPackages.callPackage ../suitesparse {
    inherit sparse-linear;
    amd = null;
    cholmod = null;
    colamd = null;
    suitesparseconfig = suitesparse_4_4_1;
    umfpack = null;
  };
in
  (haskellngPackages.callPackage
    ./.
    {
      inherit parallel-io-simple sparse-linear suitesparse;
      atlas = atlasWithLapack;
      ptf77blas = null;
      ptcblas = null;
      lapack = null;
      gfortran = gfortran.cc;
    }).env
