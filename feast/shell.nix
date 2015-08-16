with (import <nixpkgs> {});
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      parallel-io-simple = self.callPackage ./parallel-io-simple {};
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
