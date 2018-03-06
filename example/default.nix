{ reflex-platform ? import ./reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      reflex-dom-datepicker = ( self.callPackage ../reflex-dom-datepicker.nix {} );
    });
  };

  adjust = drv: {
    doHaddock = true;
    postInstall = ''
    mkdir -p $out/css
    cp ./css/* "$out/css/"
    '';
  };

  example = pkgs.haskell.lib.overrideCabal (
    haskellPackages.callPackage ./example.nix {}
  ) adjust;
in
  example
