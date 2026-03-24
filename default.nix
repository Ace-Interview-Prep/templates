{ mkDerivation, base, ClasshSS, containers, data-default, filepath
, henforcer, lens, lib, reflex-classhss, reflex-dom, text
, hedgehog, tasty, tasty-hedgehog, tasty-hunit
}:
mkDerivation {
  pname = "templates";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base ClasshSS containers data-default filepath henforcer lens
    reflex-classhss reflex-dom text
  ];
  testHaskellDepends = [
    base hedgehog tasty tasty-hedgehog tasty-hunit text filepath
  ];
  license = lib.licenses.mit;
}
