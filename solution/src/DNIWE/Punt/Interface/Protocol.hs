{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module DNIWE.Punt.Interface.Protocol where
 -- export all

import Control.Arrow ((&&&))
import Control.Monad (liftM2)

import Data.Aeson (ToJSON(..), FromJSON(..), genericParseJSON, genericToEncoding)
import Data.Aeson.Types (typeMismatch, Parser, Options(..), defaultOptions)
import Data.Char (isUpper, toLower)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Data.Graph.Inductive.Graph (mkGraph)

import DNIWE.Punt.Solver.Types


{- TODO

  * issues

  * roadmap
    - [ ] message

    - [ ] online mode
      - [X] handshake
      - [ ] setup
      - [ ] gameplay
      - [ ] scoring

    - [ ] offline mode


-}


data Message = Message {
    msgNum  :: Int
  , msgData :: Text -- TODO parse?
  } deriving (Show, Eq, Generic)


dropPrefix :: String -> String
dropPrefix "" = ""
dropPrefix (c:t)
  | isUpper c = toLower c : t
  | otherwise = dropPrefix t

dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix }


-- - P -> S: {"me" : name}
-- - S -> P: {"you" : name}
data HandshakeRequest = HandshakeRequest { hrMe :: Text }
  deriving (Show, Eq, Generic)
data HandshakeResponse = HandshakeResponse { hrYou :: Text }
  deriving (Show, Eq, Generic)


instance ToJSON HandshakeRequest where
  toEncoding = genericToEncoding dropPrefixOptions

instance FromJSON HandshakeResponse where
  parseJSON = genericParseJSON dropPrefixOptions


data Site = Site {
    siteId :: SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON Site where
  parseJSON = genericParseJSON dropPrefixOptions

  
data River = River {
    riverSource :: SiteId
  , riverTarget :: SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON River where
  parseJSON = genericParseJSON dropPrefixOptions

newtype GameBoard = GameBoard { getBoard :: Board }

data BoardMap = BoardMap {
    mapSites  :: [Site]
  , mapRivers :: [River]
  , mapMines :: Set SiteId
  } deriving (Show, Eq, Generic)
  
instance FromJSON BoardMap where
  parseJSON = genericParseJSON dropPrefixOptions


-- S -> P: {"punter" : p, "punters" : n, "map" : map}
-- P -> S: {"ready" : p}
data SetupRequest = SetupRequest { srPunter :: PunterId, srPunters :: Int, srMap :: BoardMap }
    deriving (Show, Eq, Generic)
data SetupResponse = SetupResponse { srReady :: PunterId }
    deriving (Show, Eq, Generic)

instance FromJSON SetupRequest where
  parseJSON = genericParseJSON dropPrefixOptions

instance ToJSON SetupResponse where
  toEncoding = genericToEncoding dropPrefixOptions


boardFromMap :: BoardMap -> Board
boardFromMap (BoardMap {..}) = mkGraph nodes edges
  where nodes = map (\(Site {..}) -> (siteId, NodeContext { isMine = siteId `S.member` mapMines })) mapSites
        edges = concatMap (\(River {..}) -> [(riverSource, riverTarget, notTaken), (riverTarget, riverSource, notTaken)]) mapRivers
        notTaken = EdgeContext { taken = Nothing }
