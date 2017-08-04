{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module DNIWE.Punt.Interface.Protocol where
 -- export all

import Data.Char (isUpper, toLower)

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
data HandshakeResponse = HandshakeResponse { hrYou :: Text }
  deriving (Show, Eq, Generic)


instance ToJSON HandshakeRequest where
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


data River = River {
    riverSource :: SiteId
  , riverTarget :: SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON River where
  parseJSON = genericParseJSON jsonOptions

newtype GameBoard = GameBoard { getBoard :: Board }

data BoardMap = BoardMap {
    mapSites  :: [Site]
  , mapRivers :: [River]
  , mapMines :: Set SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON BoardMap where
  parseJSON = genericParseJSON jsonOptions


-- S -> P: {"punter" : p, "punters" : n, "map" : map}
-- P -> S: {"ready" : p}
data SetupRequest = SetupRequest { srPunter :: PunterId, srPunters :: Int, srMap :: BoardMap }
    deriving (Show, Eq, Generic)
data SetupResponse = SetupResponse { srReady :: PunterId }
    deriving (Show, Eq, Generic)

instance FromJSON SetupRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON SetupResponse where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


boardFromMap :: BoardMap -> Board
boardFromMap (BoardMap {..}) = mkGraph nodes edges
  where nodes = map (\(Site {..}) -> (siteId, NodeContext { isMine = siteId `S.member` mapMines })) mapSites
        edges = concatMap (\(River {..}) -> [(riverSource, riverTarget, notTaken), (riverTarget, riverSource, notTaken)]) mapRivers
        notTaken = EdgeContext { taken = Nothing }


-- Gameplay

-- {"claim" : {"punter" : PunterId, "source" : SiteId, "target" : SiteId}}
-- {"pass" : {"punter" : PunterId}}
data Move
  = Claim {claimPunter :: PunterId, claimSource :: SiteId, claimTarget :: SiteId}
  | Pass  {passPunter :: PunterId}
  deriving (Show, Eq, Generic)

instance FromJSON Move where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Move where
  toEncoding = genericToEncoding jsonOptions


newtype Moves = Moves { movesMoves :: [Move] }
    deriving (Show, Eq, Generic)

instance FromJSON Moves where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Moves where
  toEncoding = genericToEncoding jsonOptions


-- S -> P: {"move" : {"moves" : moves}}
-- P -> S: move
data GameplayRequest = GameplayRequest { grMove :: Moves }
    deriving (Show, Eq, Generic)

instance FromJSON GameplayRequest where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON GameplayRequest where
  toEncoding = genericToEncoding jsonOptions

type GameplayResponse = Move
