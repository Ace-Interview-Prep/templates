{ mkDerivation, base, ClasshSS, containers, data-default, filepath
, lens, lib, reflex-classhss, reflex-dom-core, text
}:
mkDerivation {
  pname = "templates";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base ClasshSS containers data-default filepath lens reflex-classhss
    reflex-dom-core text
  ];
  description = "Ace templates library";
  license = lib.licenses.bsd3;
}
