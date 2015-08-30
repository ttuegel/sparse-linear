with (import <nixpkgs> {});
let
  inherit (pkgs.haskell.lib) dontCheck;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      sparse-linear = self.callPackage ../sparse-linear {};
      suitesparse = self.callPackage ../suitesparse {
        inherit (pkgs) suitesparse;
        openblas = pkgs.openblasCompat;
      };
      vector = dontCheck (self.callPackage ../sparse-linear/vector-0.10.12.3.nix {});
    };
  };
in
haskellPackages.suitesparse.env
