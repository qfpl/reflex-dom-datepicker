let
  reflex-platformJson = builtins.fromJSON (builtins.readFile ./reflex-platform.json);
  reflex-platform = import (builtins.fetchTarball (with reflex-platformJson; {
    url = "${url}/archive/${rev}.tar.gz";
    inherit sha256;
  }));
in
  reflex-platform {}
