with (import <nixpkgs> {});
let
  enableProfiling = drv: drv.overrideScope (self: super: {
    mkDerivation = drv: super.mkDerivation (drv // {
      enableLibraryProfiling = true;
    });
    inherit (pkgs) stdenv;
  });
in (enableProfiling (haskellngPackages.callPackage ./. {})).env
