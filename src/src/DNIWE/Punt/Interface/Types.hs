{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module DNIWE.Punt.Interface.Types where
 -- export all

import Data.Char (isUpper, toLower)
import Control.Applicative
import Data.Default.Class

import Data.Aeson (ToJSON(..), FromJSON(..), genericParseJSON, genericToJSON, genericToEncoding)
import Data.Aeson.Types (Options(..), SumEncoding(..), defaultOptions)

import Data.IntSet (IntSet)
import Data.Text (Text)
import GHC.Generics (Generic)

import DNIWE.Punt.Solver.Types

{- TODO
  * issues

  * roadmap
    - [ ] message

    - [ ] online mode
      - [X] handshake
      - [X] setup
      - [x] gameplay
      - [ ] scoring

    - [ ] offline mode
-}


dropPrefix :: String -> String
dropPrefix "" = ""
dropPrefix (c:t)
  | isUpper c = toLower c : t
  | otherwise = dropPrefix t

jsonOptions :: Options
jsonOptions = defaultOptions {
    fieldLabelModifier     = dropPrefix
  , sumEncoding            = ObjectWithSingleField
  , constructorTagModifier = map toLower}


data Message = Message {
    msgNum  :: Int
  , msgData :: Text -- TODO parse?
  } deriving (Show, Eq, Generic)




-- Handshake

-- - P -> S: {"me" : name}
-- - S -> P: {"you" : name}
data HandshakeRequest = HandshakeRequest { hrMe :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON HandshakeRequest where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON HandshakeRequest where
  parseJSON = genericParseJSON jsonOptions


data HandshakeResponse = HandshakeResponse { hrYou :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON HandshakeResponse where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON HandshakeResponse where
  parseJSON = genericParseJSON jsonOptions


-- Setup

data Site = Site {
    siteId :: SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON Site where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Site where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

data River = River {
    riverSource :: SiteId
  , riverTarget :: SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON River where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON River where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


newtype GameBoard = GameBoard { getBoard :: Board }

data BoardMap = BoardMap {
    mapSites  :: [Site]
  , mapRivers :: [River]
  , mapMines :: IntSet
  } deriving (Show, Eq, Generic)

instance FromJSON BoardMap where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BoardMap where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

data SettingsResponse = SettingsResponse { setrespFutures :: Maybe Bool
                                         , setrespSplurges :: Maybe Bool
                                         , setrespOptions :: Maybe Bool
                                         }
                  deriving (Show, Eq, Generic)

instance Default SettingsResponse

instance FromJSON SettingsResponse where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SettingsResponse where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

-- S -> P: {"punter" : p, "punters" : n, "map" : map, "settings" : settings}
data SetupRequest = SetupRequest { srPunter :: PunterId, srPunters :: Int, srMap :: BoardMap, srSettings :: Maybe SettingsResponse }
    deriving (Show, Eq, Generic)

instance FromJSON SetupRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetupRequest where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

-- parser future
data PFuture = PFuture {
  pfutureSource :: SiteId,
  pfutureTarget :: SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON PFuture where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PFuture where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

-- P -> S: {"ready" : p, "futures" : futures}
data SetupResponse = SetupResponse { srReady :: PunterId, srFutures :: [PFuture] }
    deriving (Show, Eq, Generic)

instance FromJSON SetupResponse where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetupResponse where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


-- Gameplay

-- {"claim" : {"punter" : PunterId, "source" : SiteId, "target" : SiteId}}
-- {"pass" : {"punter" : PunterId}}
-- {"splurge" : {"punter" : PunterId, "route": [SiteId]}
-- {"option" : {"punter" : PunterId, "source" : SiteId, "target" : SiteId}}
data Move
  = Claim { claimPunter :: PunterId, claimSource :: SiteId, claimTarget :: SiteId }
  | Pass { passPunter :: PunterId }
  | Splurge { splurgePunter :: PunterId, splurgeRoute :: [SiteId] }
  | Option { optionPunter :: PunterId, optionSource :: SiteId, optionTarget :: SiteId }
  deriving (Show, Eq, Generic)

instance FromJSON Move where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Move where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


newtype Moves = Moves { movesMoves :: [Move] }
    deriving (Show, Eq, Generic)

instance FromJSON Moves where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Moves where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


-- S -> P: {"move" : {"moves" : moves}}
-- P -> S: move
data GameplayRequest = GameplayRequest { grMove :: Moves }
    deriving (Show, Eq, Generic)

instance FromJSON GameplayRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON GameplayRequest where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

type GameplayResponse = Move


-- Scoring

data ScoreResponse = ScoreResponse { scorePunter :: PunterId, scoreScore :: Score }
    deriving (Show, Eq, Generic)

instance FromJSON ScoreResponse where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ScoreResponse where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


data Stop = Stop { stopMoves :: [Move], stopScores :: [ScoreResponse] }
    deriving (Show, Eq, Generic)

instance FromJSON Stop where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Stop where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

newtype StopRequest = StopRequest { srStop :: Stop }
    deriving (Show, Eq, Generic)

instance FromJSON StopRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON StopRequest where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


-- Protocol

data Request = StopReq StopRequest
             | GameReq GameplayRequest
             deriving (Show, Eq)

instance FromJSON Request where
  parseJSON p = (StopReq <$> parseJSON p) <|> (GameReq <$> parseJSON p)

instance ToJSON Request where
  toJSON (StopReq stop) = toJSON stop
  toJSON (GameReq game) = toJSON game
