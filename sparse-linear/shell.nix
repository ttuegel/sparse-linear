with (import <nixpkgs> {});
let
  inherit (pkgs.haskell.lib) dontCheck;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      vector = dontCheck (self.callPackage ./vector-0.10.12.3.nix {});
    };
  };
in
(haskellPackages.callPackage ./. {}).env
