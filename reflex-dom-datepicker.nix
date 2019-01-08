{ mkDerivation, base, containers, ghcjs-dom, jsaddle-warp, lens
, reflex, reflex-dom, stdenv, text, time, wai-app-static
}:
mkDerivation {
  pname = "reflex-dom-datepicker";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers ghcjs-dom jsaddle-warp lens reflex reflex-dom text
    time wai-app-static
  ];
  description = "A date picker UI widget for the reflex FRP platform";
  license = stdenv.lib.licenses.bsd3;
}
