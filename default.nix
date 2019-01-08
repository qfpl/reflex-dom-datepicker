{ nixpkgs ? import <nixpkgs> {}
}:
let
  reflex-platform = import (nixpkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "01f111397db4c3e8893cc91b66dab895b4bd4e67";
    sha256 = "13r58505gjkr8smmwkq0v1fb2zr1a6k8vd7k4jrk7wzsfxm4557r";
  }) {};
in
  reflex-platform.project ({ pkgs, ... }: {
    useWarp = true;

    packages = {
      reflex-dom-datepicker = ./.;
    };

    shells = {
      ghc   = ["reflex-dom-datepicker"];
      ghcjs = ["reflex-dom-datepicker"];
    };
  })
