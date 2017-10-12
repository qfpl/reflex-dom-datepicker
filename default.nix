{ reflex-platform ? import ./reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  adjust = drv: {
    doHaddock = false;
    postInstall = ''
    mkdir -p $out/css
    cp ./css/* "$out/css/"
    '';
  };

  reflex-dom-datepicker = pkgs.haskell.lib.overrideCabal (
    reflex-platform.${compiler}.callPackage ./reflex-dom-datepicker.nix {}
  ) adjust;
in
  reflex-dom-datepicker
