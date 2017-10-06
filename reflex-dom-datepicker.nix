{ mkDerivation, base, containers, ghcjs-dom, jsaddle, lens, reflex
, reflex-dom, stdenv, text, time
}:
mkDerivation {
  pname = "reflex-dom-datepicker";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers ghcjs-dom jsaddle lens reflex reflex-dom text time
  ];
  description = "A date picker UI widget for the reflex FRP platform";
  license = stdenv.lib.licenses.bsd3;
}
