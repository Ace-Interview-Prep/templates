{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  # Override haskellPackages to use local versions
  haskellPackages' = haskellPackages.override {
    overrides = self: super: {
      ClasshSS = self.callCabal2nix "ClasshSS" ../ClasshSS-dev {};
      reflex-classhss = self.callCabal2nix "reflex-classhss" ../reflex-classh {};
    };
  };

  templates = import ./default.nix;
  drv = variant (haskellPackages'.callPackage templates {});
in
pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
}
