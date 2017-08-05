{ mkDerivation, aeson, attoparsec, base, base64-bytestring0test
, bytestring, cereal, conduit, conduit-extra, containers, directory
, fgl, filepath, hspec, QuickCheck, stdenv, text, transformers
}:
mkDerivation {
  pname = "solution";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring0test bytestring cereal
    conduit conduit-extra containers fgl text
  ];
  executableHaskellDepends = [
    aeson base bytestring conduit transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath hspec
    QuickCheck text
  ];
  license = stdenv.lib.licenses.bsd3;
}
