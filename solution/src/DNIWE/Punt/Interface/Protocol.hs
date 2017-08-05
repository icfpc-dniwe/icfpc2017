{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module DNIWE.Punt.Interface.Protocol where
 -- export all

import Data.Char (isUpper, toLower)
import Control.Applicative

import Data.Aeson (ToJSON(..), FromJSON(..), genericParseJSON, genericToJSON, genericToEncoding)
import Data.Aeson.Types (Options(..), SumEncoding(..), defaultOptions)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Graph.Inductive.Graph (mkGraph)

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
  , mapMines :: Set SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON BoardMap where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BoardMap where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

data Settings = Settings {
  futures :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON Settings where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Settings where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

-- S -> P: {"punter" : p, "punters" : n, "map" : map, "settings" : settings}
data SetupRequest = SetupRequest { srPunter :: PunterId, srPunters :: Int, srMap :: BoardMap, srSettings :: Maybe Settings }
    deriving (Show, Eq, Generic)

instance FromJSON SetupRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetupRequest where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

-- parser future
data PFuture = PFuture {
  futureSource :: SiteId,
  futureTarget :: SiteId
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


boardFromMap :: BoardMap -> IndexedBoard
boardFromMap (BoardMap {..}) = IndexedBoard { ibBoard = mkGraph nodes edges
                                            , ibMines = mapMines
                                            }
  where nodes = map (\(Site {..}) -> (siteId, NodeContext { isMine = siteId `S.member` mapMines })) mapSites
        edges = map (\(River {..}) -> (riverSource, riverTarget, notTaken)) mapRivers
        notTaken = EdgeContext { taken = Nothing }


-- Gameplay

-- {"claim" : {"punter" : PunterId, "source" : SiteId, "target" : SiteId}}
-- {"pass" : {"punter" : PunterId}}
data Move
  = Claim { claimPunter :: PunterId, claimSource :: SiteId, claimTarget :: SiteId }
  | Pass { passPunter :: PunterId }
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

data Score = Score { scorePunter :: PunterId, scoreScore :: Int }
    deriving (Show, Eq, Generic)

instance FromJSON Score where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Score where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


data Stop = Stop { stopMoves :: [Move], stopScores :: [Score] }
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
