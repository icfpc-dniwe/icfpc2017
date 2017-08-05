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

import DNIWE.Punt.Interface.Offline (RawIncomingMessage(..), RawOutgoingMessage(..), IncomingMessage(..), OutgoingMessage(..), fromRaw, toRaw, GameState(..), RawMoveState(..), RawMove(..), Move(..), RawGameStateStandalone(..))

import Test.Hspec (Expectation, Spec, hspec, describe, it, pending, shouldSatisfy, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Arbitrary(..), sized, resize, oneof, choose, suchThat, frequency, arbitrarySizedNatural, listOf, listOf1, elements)
import System.Directory (listDirectory)
import System.FilePath.Posix (dropExtensions)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map



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
    it "decodes Setup    " $ pending
    it "decodes Gameplay " $ pending
    it "decodes Score    " $ pending
    it "decodes Timeout  " $ checkDecodeRaw (samples ! "incoming-timeout") RITimeout { ritTimeout = "" }

  describe "RawOutgoingMessage" $ do
    it "encodes Handshake" $ checkEncodeRaw (samples ! "outgoing-handshake") ROHandshake { rohMe = "dniwe" }
    it "encodes Setup    " $ checkEncodeRaw (samples ! "outgoing-setup") ROSetup { rosReady = 42, rosState = "" }
    it "encodes Gameplay " $ do

      checkEncodeRaw (samples ! "outgoing-gameplay-1") ROGameplay {rogMove = RawClaim { rcPunter = 1, rcSource = 4, rcTarget = 2 }, rogState = "" }
      
      -- checkEncodeRaw (samples ! "outgoing-gameplay-1") (ROGameplay $ RawMoveState (RawClaim { rcPunter = 1, rcSource = 2, rcTarget = 4 }, RawGameStateStandalone ""))
      -- checkEncodeRaw (samples ! "outgoing-gameplay-2") (ROGameplay $ RawMoveState (RawPass { rpPunter = 1 }, RawGameStateStandalone "" ))

  describe "IncomingMessage" $ do
    it "decodes Handshake" $ checkDecode (samples ! "incoming-handshake") IHandshake { ihName = "pidar" }
    it "decodes Setup    " $ pending
    it "decodes Gameplay " $ pending
    it "decodes Score    " $ pending
    it "decodes Timeout  " $ checkDecode (samples ! "incoming-timeout") ITimeout { itTimeout = GameState }

  describe "OutgoingMessage" $ do
    it "encodes Handshake" $ checkEncode (samples ! "outgoing-handshake") OHandshake { ohName = "dniwe" }
    it "encodes Setup    " $ checkEncode (samples ! "outgoing-setup") OSetup { osPunterId = 42, osState = GameState }
    it "encodes Gameplay " $ pending
