with (import <nixpkgs> {});
with stdenv.lib;
with haskell-ng.lib;
let
  enableOptimization = ttuegel.haskell.enableOptimization or id;
  enableProfiling = ttuegel.haskell.enableProfiling or id;
  pkg =
    fold (f: x: f x) (haskellngPackages.callPackage ./. {})
      [
        enableOptimization
        enableProfiling
      ];
in
  pkg.env
