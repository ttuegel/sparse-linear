with (import <nixpkgs> {});
let sparse-linear = haskellngPackages.callPackage ./. {};
in
sparse-linear.env
