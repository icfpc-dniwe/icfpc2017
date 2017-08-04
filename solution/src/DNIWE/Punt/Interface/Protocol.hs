{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module DNIWE.Punt.Interface.Protocol where
 -- export all

import Control.Arrow ((&&&))
import Control.Monad (liftM2)

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), genericParseJSON, object, (.=), (.:), (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Options(..), defaultOptions)
import Data.Char(isUpper, toLower)

import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as T

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


-- - P -> S: {"me" : name}
-- - S -> P: {"you" : name}
data Handshake
  = HandshakeRequest  { hrName :: Text }
  | HandshakeResponse { hrName :: Text }
  deriving (Show, Eq)


instance ToJSON Handshake where
  toJSON (HandshakeRequest {..})  = object ["me" .= hrName]
  toJSON (HandshakeResponse {..}) = object ["you" .= hrName]

instance FromJSON Handshake where
  parseJSON (Object v) = liftM2 (,) (v .:? "me") (v .:? "you") >>= \case
    (Just name, Nothing) -> return $ HandshakeRequest name
    (Nothing, Just name) -> return $ HandshakeResponse name
    _                    -> fail "Handshake :: unexpected structure"

  parseJSON invalid = typeMismatch "Handshake" invalid



dropPrefix :: String -> String
dropPrefix "" = ""
dropPrefix (c:t)
  | isUpper c = toLower c : t
  | otherwise = dropPrefix t

dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix }


type SiteId = Int

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


data Map = Map {
    mapSites  :: [(Int, Bool)] -- TODO: replace with custom types
  , mapRivers :: [(Int, Int)]  -- TODO: replace with custom types
  } deriving (Show, Eq)
  
instance FromJSON Map where
  parseJSON (Object v) = do
    sites  <- (v .: "sites")  :: (Parser [Site])
    rivers <- (v .: "rivers") :: (Parser [River])
    mines  <- (v .: "mines")  :: (Parser [SiteId])

    let mapSites = map ((id &&& (`elem` mines)) .  siteId) sites
    let mapRivers = map (riverSource &&& riverTarget) rivers
    return Map {..}


-- S -> P: {"punter" : p, "punters" : n, "map" : map}
-- P -> S: {"ready" : p}
data Setup
  = SetupRequest  { srPunter :: Int, srPunters :: Int, srMap :: Map }
  | SetupResponse { srPunter :: Int }
    deriving (Show, Eq, Generic)

instance ToJSON Setup where
  toJSON (SetupResponse {..}) = object ["ready" .= srPunter ]

instance FromJSON Setup where
  parseJSON (Object v) = do
    srPunter  <- v .: "punter"
    srPunters <- v .: "punters"
    srMap     <- v .: "map"
    return SetupRequest {..}

  parseJSON invalid = typeMismatch "Setup" invalid
