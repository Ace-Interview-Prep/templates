{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  haskellLib = pkgs.haskell.lib;

  hpkgs = haskellPackages.override {
    overrides = self: super: {
      henforcer = haskellLib.dontCheck (haskellLib.doJailbreak (self.callPackage ({ mkDerivation, base, containers, dlist, fetchzip, filepattern, ghc
        , lib, optparse-applicative, pollock, text, tomland
        }:
        mkDerivation {
          pname = "henforcer";
          version = "1.0.0.1";
          src = fetchzip {
            url = "https://hackage.haskell.org/package/henforcer-1.0.0.1/henforcer-1.0.0.1.tar.gz";
            sha256 = "1vnwvhszvqrypcvk8zvb7px2q78mn3lhbhj521vsrz6zj6459f0r";
          };
          libraryHaskellDepends = [
            base containers dlist filepattern ghc optparse-applicative pollock
            text tomland
          ];
          doHaddock = false;
          license = lib.licenses.mit;
        }) {}));
      ClasshSS = haskellLib.dontCheck (haskellLib.doJailbreak
        (haskellLib.overrideCabal
          (self.callPackage ../ClasshSS/ClasshSS/default.nix {})
          (old: {
            postPatch = (old.postPatch or "") + ''
              sed -i '/henforcer/d' ClasshSS.cabal
              sed -i '/-fplugin Henforcer/d' ClasshSS.cabal
            '';
          })));
      reflex-classhss = haskellLib.dontCheck (haskellLib.doJailbreak
        (self.callPackage ../reflex-classh/default.nix {}));
    };
  };

  variant = if doBenchmark then haskellLib.doBenchmark else pkgs.lib.id;
  pkg = import ./default.nix;
  drv = variant (hpkgs.callPackage pkg {});
in
pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install pkgs.haskellPackages.fourmolu ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
}
