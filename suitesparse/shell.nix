with (import <nixpkgs> {});
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      sparse-linear = self.callPackage ../sparse-linear {};
    };
  };
in
(haskellPackages.callPackage ./. { openblas = pkgs.openblasCompat; }).env
