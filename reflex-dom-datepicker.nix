{ mkDerivation, base, containers, jsaddle-warp, lens, reflex
, reflex-dom-core, stdenv, text, time
}:
mkDerivation {
  pname = "reflex-dom-datepicker";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers jsaddle-warp lens reflex reflex-dom-core text time
  ];
  description = "A date picker UI widget for the reflex FRP platform";
  license = stdenv.lib.licenses.bsd3;
}
