with (import <nixpkgs> {});
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      sparse-linear = self.callPackage ../sparse-linear {};
      suitesparse = self.callPackage ../suitesparse {
        inherit (pkgs) suitesparse;
        openblas = pkgs.openblasCompat;
      };
      feast = self.callPackage ./. {
        inherit (pkgs) feast;
      };
    };
  };
in
haskellPackages.feast.env
