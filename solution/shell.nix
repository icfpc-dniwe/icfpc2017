{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, base64-bytestring
      , binary, bytestring, cereal, conduit, conduit-extra, containers
      , data-default, data-default-class, directory, fgl, filepath, hspec
      , QuickCheck, stdenv, text, transformers, unordered-containers
      }:
      mkDerivation {
        pname = "solution";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson attoparsec base base64-bytestring binary bytestring cereal
          conduit conduit-extra containers data-default-class fgl text
          unordered-containers
        ];
        executableHaskellDepends = [
          aeson base bytestring cereal conduit containers data-default
          data-default-class fgl transformers
        ];
        testHaskellDepends = [
          aeson base bytestring containers directory filepath hspec
          QuickCheck text
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
