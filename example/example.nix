{ mkDerivation, base, jsaddle-warp, lens, reflex, reflex-dom
, reflex-dom-core, reflex-dom-datepicker, stdenv, text, time
}:
mkDerivation {
  pname = "example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base jsaddle-warp lens reflex reflex-dom reflex-dom-core
    reflex-dom-datepicker text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
