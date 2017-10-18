{ mkDerivation, base, containers, ghcjs-dom, jsaddle, lens, reflex
, reflex-dom, stdenv, text, time
}:
mkDerivation {
  pname = "reflex-dom-datepicker";
  version = "0.0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base containers ghcjs-dom jsaddle lens reflex reflex-dom text time
  ];
  executableHaskellDepends = [
    base reflex reflex-dom text time lens
  ];
  description = "A date picker UI widget for the reflex FRP platform";
  license = stdenv.lib.licenses.bsd3;
}
