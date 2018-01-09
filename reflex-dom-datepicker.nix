{ mkDerivation, base, containers, ghcjs-dom, jsaddle-warp, lens
, reflex, reflex-dom, reflex-dom-core, stdenv, text, time
}:
mkDerivation {
  pname = "reflex-dom-datepicker";
  version = "0.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers ghcjs-dom lens reflex reflex-dom text time
  ];
  executableHaskellDepends = [
    base jsaddle-warp lens reflex reflex-dom reflex-dom-core text time
  ];
  description = "A date picker UI widget for the reflex FRP platform";
  license = stdenv.lib.licenses.bsd3;
}
