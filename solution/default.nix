{ mkDerivation, aeson, attoparsec, base, base64-bytestring, binary
, bytestring, cereal, conduit, conduit-extra, containers
, data-default, data-default-class, deepseq, directory, fgl
, filepath, hashable, hspec, QuickCheck, stdenv, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "solution";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring binary bytestring cereal
    conduit conduit-extra containers data-default-class deepseq fgl
    hashable text unordered-containers
  ];
  executableHaskellDepends = [
    aeson base bytestring cereal conduit containers data-default
    data-default-class fgl transformers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath hspec
    QuickCheck text
  ];
  license = stdenv.lib.licenses.bsd3;
}
