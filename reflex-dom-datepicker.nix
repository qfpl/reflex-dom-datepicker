{ mkDerivation, base, containers, ghcjs-dom, lens, reflex
, reflex-dom, stdenv, text, time, jsaddle-warp, reflex-dom-core
}:
mkDerivation {
  pname = "reflex-dom-datepicker";
  version = "0.0.3";
  src = ./.;
  libraryHaskellDepends = [
    base containers ghcjs-dom lens reflex reflex-dom text time
  ];
  description = "A date picker UI widget for the reflex FRP platform";
  license = stdenv.lib.licenses.bsd3;
}
