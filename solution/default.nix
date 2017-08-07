{ mkDerivation, aeson, aeson-casing, attoparsec, base
, base64-bytestring, binary, bytestring, cereal, conduit
, conduit-extra, containers, data-default, data-default-class
, deepseq, directory, fgl, filepath, hashable, hspec, MonadRandom
, QuickCheck, stdenv, text, transformers, unordered-containers
, vector
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
    hashable MonadRandom text unordered-containers
  ];
  executableHaskellDepends = [
    aeson aeson-casing base bytestring cereal conduit containers
    data-default data-default-class fgl transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath hspec
    QuickCheck text
  ];
  license = stdenv.lib.licenses.bsd3;
}
