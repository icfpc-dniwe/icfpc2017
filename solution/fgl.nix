{ mkDerivation, array, base, containers, deepseq, fetchgit, hspec
, microbench, QuickCheck, stdenv, transformers
}:
mkDerivation {
  pname = "fgl";
  version = "5.5.3.1";
  src = fetchgit {
    url = "git://github.com/icfpc-dniwe/fgl/";
    sha256 = "1916qfwl71rx4pphav3i4vfn4nlglkg30ss6q28fcbxz4cbmv1a0";
    rev = "8c22e525792f634e3cf3cc2ddff871fa810ffb31";
  };
  libraryHaskellDepends = [
    array base containers deepseq transformers
  ];
  testHaskellDepends = [ base containers hspec QuickCheck ];
  benchmarkHaskellDepends = [ base deepseq microbench ];
  description = "Martin Erwig's Functional Graph Library";
  license = stdenv.lib.licenses.bsd3;
}
