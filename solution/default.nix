{ mkDerivation, aeson, base, hspec, QuickCheck, stdenv, text }:
mkDerivation {
  pname = "solution";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base text ];
  testHaskellDepends = [ base hspec QuickCheck text ];
  license = stdenv.lib.licenses.bsd3;
}
