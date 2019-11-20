{ reflex-platform ? import ./nix/reflex-platform.nix }:
reflex-platform.project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    reflex-dom-datepicker = ./.;
  };

  shells = {
    ghcjs = ["reflex-dom-datepicker"];
  };
})
