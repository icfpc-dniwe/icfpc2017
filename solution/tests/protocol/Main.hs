{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (liftM2)
import Data.ByteString.Lazy (ByteString)
import Data.Char (chr)
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Data.Maybe (isJust)
import Data.Aeson (FromJSON, Value)

import qualified Data.Aeson as JSON (encode, decode)
import qualified Data.Text as T

import DNIWE.Punt.Interface.Offline (RawIncomingMessage(..), RawOutgoingMessage(..), IncomingMessage(..), OutgoingMessage(..), fromRaw, toRaw, SGameState(..), Move(..), RawClaim(..), RawPass(..), RawMoves(..), RawMove(..), RawMovesScores(..), RawScore(..), RawGameMap(..), Site(..), River(..))

import Test.Hspec (Expectation, Spec, hspec, describe, it, pending, shouldSatisfy, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, choose, suchThat, frequency, arbitrarySizedNatural, listOf, listOf1, elements)
import System.Directory (listDirectory)
import System.FilePath.Posix (dropExtensions)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set


samplesDir :: FilePath
samplesDir = "tests/protocol/json/"

loadJSON :: FilePath -> IO BSL.ByteString
loadJSON fn = BSLC8.filter (not . (`elem` [' ', '\n'])) <$> BSL.readFile (samplesDir ++ fn ++ ".json")


main :: IO ()
main = do
  samples <- fmap Map.fromList . mapM (\fn -> (fn,) <$> (loadJSON fn)) . map dropExtensions =<< listDirectory samplesDir
  hspec (spec samples)


checkDecodeRaw :: BSL.ByteString -> RawIncomingMessage -> Expectation
checkDecodeRaw sample msg = JSON.decode sample `shouldBe` (Just msg)

checkEncodeRaw :: BSL.ByteString -> RawOutgoingMessage -> Expectation
checkEncodeRaw sample msg = (JSON.encode msg) `shouldBe` sample

checkDecode :: BSL.ByteString -> IncomingMessage -> Expectation
checkDecode sample msg = (fmap fromRaw . JSON.decode $ sample) `shouldBe` (Just msg)

checkEncode :: BSL.ByteString -> OutgoingMessage -> Expectation
checkEncode sample msg = (JSON.encode . toRaw $ msg) `shouldBe` sample


spec :: Map FilePath BSL.ByteString -> Spec
spec samples = do
  describe "RawIncomingMessage" $ do
    it "decodes Handshake" $ checkDecodeRaw (samples ! "incoming-handshake") RIHandshake { rihYou = "pidar" }
    it "decodes Setup    " $ checkDecodeRaw (samples ! "incoming-setup") RISetup {
          risPunter = 4
        , risPunters = 8
        , risMap = RawGameMap {
              rgmSites = [Site 1, Site 2, Site 3, Site 4]
            , rgmRivers = [River 1 2, River 1 3, River 1 4]
            , rgmMines = Set.fromList [1,3]}}

    it "decodes Gameplay " $ checkDecodeRaw (samples ! "incoming-gameplay-1") RIGameplay {
        rigMove = RawMoves [
            RawMoveClaim RawClaim {rcPunter = 1, rcSource = 4, rcTarget = 2 }
          , RawMoveClaim RawClaim {rcPunter = 2, rcSource = 3, rcTarget = 4 }
          , RawMovePass  RawPass  {rpPunter = 3 }]
      , rigState = "" }

    it "decodes Score" $ checkDecodeRaw (samples ! "incoming-score-1") RIScore {
      risStop = RawMovesScores {
          rmsMoves = [
            RawMoveClaim RawClaim {rcPunter = 1, rcSource = 4, rcTarget = 2 }
          , RawMoveClaim RawClaim {rcPunter = 2, rcSource = 3, rcTarget = 4 }
          , RawMovePass  RawPass  {rpPunter = 3 }]
           , rmsScores = [
                 RawScore { rscPunter = 1, rscScore = 3 }
               , RawScore { rscPunter = 2, rscScore = 2 }
               , RawScore { rscPunter = 3, rscScore = 1 }]}
      , risState = ""}



    it "decodes Timeout  " $ checkDecodeRaw (samples ! "incoming-timeout") RITimeout { ritTimeout = "" }

  describe "RawOutgoingMessage" $ do
    it "encodes Handshake" $ checkEncodeRaw (samples ! "outgoing-handshake") ROHandshake { rohMe = "dniwe" }
    it "encodes Setup    " $ checkEncodeRaw (samples ! "outgoing-setup") ROSetup { rosReady = 42, rosState = "" }
    it "encodes Gameplay " $ do
      checkEncodeRaw (samples ! "outgoing-gameplay-1") ROGameplayClaim {rogcClaim = RawClaim {rcPunter = 1, rcSource = 4, rcTarget = 2 }, rogcState = "" }
      checkEncodeRaw (samples ! "outgoing-gameplay-2") ROGameplayPass  {rogpPass = RawPass { rpPunter = 1 }, rogpState = "" }

  describe "IncomingMessage" $ do
    it "decodes Handshake" $ checkDecode (samples ! "incoming-handshake") IHandshake { ihName = "pidar" }
    it "decodes Setup    " $ pending
    it "decodes Gameplay " $ checkDecode (samples ! "incoming-gameplay-1") IGameplay {
        igMoves = [
            Claim {claimPunterId = 1, claimEdge = (4, 2) }
          , Claim {claimPunterId = 2, claimEdge = (3, 4) }
          , Pass  {passPunterId = 3 }]
      , igState = SGameState }

    it "decodes Score" $ checkDecode (samples ! "incoming-score-1") IScore {
             isMoves = [
                 Claim {claimPunterId = 1, claimEdge = (4, 2) }
               , Claim {claimPunterId = 2, claimEdge = (3, 4) }
               , Pass  {passPunterId = 3 }]
           , isScores = [(1,3), (2,2), (3,1)]
           , isState = SGameState}

    it "decodes Timeout  " $ checkDecode (samples ! "incoming-timeout") ITimeout { itTimeout = SGameState }

  describe "OutgoingMessage" $ do
    it "encodes Handshake" $ checkEncode (samples ! "outgoing-handshake") OHandshake { ohName = "dniwe" }
    it "encodes Setup    " $ checkEncode (samples ! "outgoing-setup") OSetup { osPunterId = 42, osState = SGameState }
    it "encodes Gameplay " $ do
      checkEncode (samples ! "outgoing-gameplay-1") OGameplay {ogMove = Claim { claimPunterId = 1, claimEdge = (4, 2) }, ogState = SGameState }
      checkEncode (samples ! "outgoing-gameplay-2") OGameplay {ogMove = Pass { passPunterId = 1 }, ogState = SGameState }
