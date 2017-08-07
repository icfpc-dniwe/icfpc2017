{ mkDerivation, aeson, async, attoparsec, base, base64-bytestring
, binary, bytestring, cereal, conduit, conduit-extra, containers
, data-default, data-default-class, deepseq, directory, fgl
, filepath, hashable, hspec, MonadRandom, process, QuickCheck
, stdenv, stm, stm-chans, streaming-commons, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "solution";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base base64-bytestring binary bytestring
    cereal conduit conduit-extra containers data-default-class deepseq
    fgl hashable MonadRandom process stm stm-chans streaming-commons
    text unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson base bytestring cereal conduit containers data-default
    data-default-class fgl MonadRandom transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath hspec
    QuickCheck text
  ];
  license = stdenv.lib.licenses.bsd3;
}
