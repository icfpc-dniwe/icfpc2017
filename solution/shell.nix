{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, conduit, conduit-extra, fgl
      , hspec, QuickCheck, stdenv, text
      }:
      mkDerivation {
        pname = "solution";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base conduit conduit-extra fgl text
        ];
        testHaskellDepends = [ base hspec QuickCheck text ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
