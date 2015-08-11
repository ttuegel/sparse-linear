with (import <nixpkgs> {});
(haskellngPackages.callPackage ./. {
  sparse-linear = haskellngPackages.callPackage ../sparse-linear {};
  suitesparse = suitesparse_4_4;
  openblas = openblasCompat;
}).env
