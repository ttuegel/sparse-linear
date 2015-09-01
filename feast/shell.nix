with (import <nixpkgs> {});
let
  inherit (pkgs.haskell.lib) dontCheck;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      feast = self.callPackage ./. {
        inherit (pkgs) feast;
      };
      sparse-linear = self.callPackage ../sparse-linear {};
      suitesparse = self.callPackage ../suitesparse {
        inherit (pkgs) suitesparse;
        openblas = pkgs.openblasCompat;
      };
    };
  };
in
haskellPackages.feast.env
