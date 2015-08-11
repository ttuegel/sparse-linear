with (import <nixpkgs> {});
let
  parallel-io-simple = haskellngPackages.callPackage ./parallel-io-simple {};
  sparse-linear = haskellngPackages.callPackage ../sparse-linear {};
  suitesparse = haskellngPackages.callPackage ../suitesparse {
    inherit sparse-linear;
    openblas = openblasCompat;
  };
in
(haskellngPackages.callPackage ./. {
  inherit parallel-io-simple sparse-linear suitesparse;
}).env
