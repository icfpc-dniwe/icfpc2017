{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}


module DNIWE.Punt.Interface.Offline where

import Data.Serialize (Serialize(..), runGet, runPut)

import Data.Char (isUpper, toLower)
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), genericParseJSON, genericToJSON, genericToEncoding, (.:), withObject, encode)
import Data.Aeson.Types (Options(..), SumEncoding(..), defaultOptions)

import Data.Aeson (fromEncoding)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Graph.Inductive.Graph (mkGraph)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

import DNIWE.Punt.Solver.Types


dropPrefix :: String -> String
dropPrefix "" = ""
dropPrefix (c:t)
  | isUpper c = toLower c : t
  | otherwise = dropPrefix t

jsonOptions :: Options
jsonOptions = defaultOptions {
    fieldLabelModifier     = dropPrefix
  , sumEncoding            = UntaggedValue
  , constructorTagModifier = map toLower}


-- TODO
-- Gameplay
-- Scoring
-- Timeout
-- SGameState
{-
------------
-- TODO rem
data Xxx = Xxx { }
  deriving (Show, Eq, Generic)

instance ToJSON Xxx where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Xxx where
  parseJSON = genericParseJSON jsonOptions

----------------



-- Handshake
data HandshakeIn = HandshakeIn { hiMe :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON HandshakeIn where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON HandshakeIn where
  parseJSON = genericParseJSON jsonOptions


data HandshakeOut = HandshakeOut { hoYou :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON HandshakeOut where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON HandshakeOut where
  parseJSON = genericParseJSON jsonOptions



-- Setup

data SetupIn = SetupIn { siPunter :: PunterId, siPunters :: Nat, siMap :: BoardMap }
  deriving (Show, Eq, Generic)

instance ToJSON SetupIn where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON SetupIn where
  parseJSON = genericParseJSON jsonOptions


data SetupOut = SetupOut { soReady :: PunterId, soSGameState :: SGameState }
  deriving (Show, Eq, Generic)

instance ToJSON SetupOut where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON SetupOut where
  parseJSON = genericParseJSON jsonOptions

-- Gameplay

data GameplayIn = GameplayIn { giMove}
  deriving (Show, Eq, Generic)

instance ToJSON GameplayIn where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON GameplayIn where
  parseJSON = genericParseJSON jsonOptions



data GameplayOut = GameplayOut { }
  deriving (Show, Eq, Generic)

instance ToJSON GameplayOut where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance FromJSON GameplayOut where
  parseJSON = genericParseJSON jsonOptions



S → P{"move" : {"moves" : moves}, "state" : state}
P → Smove ] {"state" : state0}
-}

-- BoardMap




type Nat = Int
type RawSGameState = Text


-- SGameState
data SGameState = SGameState -- TODO: here goes custom structure
  deriving (Show, Eq, Generic)

instance Serialize SGameState

putBase64 :: (Serialize a) => a -> Text
putBase64 = T.decodeUtf8 . Base64.encode . runPut . put

getBase64 :: (Serialize a) => Text -> a
getBase64 = fromRight . runGet get . fromRight . Base64.decode . T.encodeUtf8 where
  fromRight = either error id


-- Move
data Move
  = Claim { claimPunterId :: PunterId, claimEdge :: (Int, Int) }
  | Pass { passPunterId :: PunterId }
  deriving (Show, Eq, Generic)

-- Messages

data IncomingMessage
     = IHandshake { ihName :: Text }
     -- | ISetup
     | IGameplay  { igMoves :: [Move], igState :: SGameState }
     | IScore { isMoves :: [Move], isScores :: [(Int, Int)], isState :: SGameState }
     | ITimeout   { itTimeout :: SGameState }
  deriving (Show, Eq)

data OutgoingMessage
     = OHandshake { ohName     :: Text                           }
     | OSetup     { osPunterId :: PunterId, osState :: SGameState }
     | OGameplay  { ogMove     :: Move    , ogState :: SGameState }
  deriving (Show, Eq)

fromRaw :: RawIncomingMessage -> IncomingMessage
fromRaw (RIHandshake {..}) = IHandshake { ihName = rihYou }
-- fromRaw (RISetup    {..}) =
fromRaw (RIGameplay {..}) = IGameplay { igMoves = map fromRawMove . unwrapMoves $ rigMove, igState = getBase64 rigState }
fromRaw (RIScore    {..}) = IScore {
    isMoves = map fromRawMove . rmsMoves $ risStop
  , isScores = map (\(RawScore {..}) -> (rscPunter, rscScore)) . rmsScores $ risStop
  , isState = getBase64 risState }
fromRaw (RITimeout {..}) = ITimeout { itTimeout = getBase64 ritTimeout }

toRaw :: OutgoingMessage -> RawOutgoingMessage
toRaw (OHandshake {..}) = ROHandshake { rohMe = ohName }
toRaw (OSetup    {..}) = ROSetup { rosReady = osPunterId, rosState = putBase64 osState }
toRaw (OGameplay {..}) = case ogMove of
  Claim {..} -> ROGameplayClaim {
      rogcClaim = RawClaim { rcPunter = claimPunterId, rcSource = fst claimEdge, rcTarget = snd claimEdge}
    , rogcState = putBase64 ogState}
  Pass  {..} -> ROGameplayPass {
      rogpPass = RawPass { rpPunter = passPunterId }
    , rogpState = putBase64 ogState }


data RawIncomingMessage
     = RIHandshake { rihYou     :: Text                                                         }
     -- | RISetup     { risPunter  :: PunterId   , risPunters :: Nat       , risMap :: RawBoardMap }
     | RIGameplay  { rigMove    :: RawMoves      , rigState   :: RawSGameState                      }
     | RIScore     { risStop    :: RawMovesScores, risState   :: RawSGameState                         }
     | RITimeout   { ritTimeout :: RawSGameState                                                    }
  deriving (Show, Eq, Generic)

data RawOutgoingMessage
     = ROHandshake      { rohMe    :: Text                            }
     | ROSetup          { rosReady :: PunterId, rosState :: RawSGameState }
     | ROGameplayClaim  { rogcState :: RawSGameState, rogcClaim :: RawClaim }
     | ROGameplayPass   { rogpState :: RawSGameState, rogpPass :: RawPass }
  deriving (Show, Eq, Generic)


data RawClaim = RawClaim { rcPunter :: PunterId, rcSource :: SiteId, rcTarget :: SiteId }
  deriving (Show, Eq, Generic)

instance FromJSON RawClaim where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawClaim where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


data RawPass = RawPass { rpPunter :: PunterId }
  deriving (Show, Eq, Generic)

instance FromJSON RawPass where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawPass where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


data RawMove
  = RawMoveClaim { rmcClaim :: RawClaim }
  | RawMovePass { rmpPass :: RawPass }
  deriving (Show, Eq, Generic)

instance FromJSON RawMove where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawMove where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

fromRawMove :: RawMove -> Move
fromRawMove (RawMoveClaim (RawClaim {..})) = Claim { claimPunterId = rcPunter, claimEdge = (rcSource, rcTarget) }
fromRawMove (RawMovePass (RawPass {..})) = Pass { passPunterId = rpPunter }


newtype RawMoves = RawMoves {unwrapMoves :: [RawMove]}
  deriving (Show, Eq, Generic)

instance FromJSON RawMoves where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawMoves where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


data RawMovesScores = RawMovesScores { rmsMoves :: [RawMove], rmsScores :: [RawScore]}
  deriving (Show, Eq, Generic)

instance FromJSON RawMovesScores where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawMovesScores where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


data RawScore = RawScore { rscPunter :: PunterId, rscScore :: Nat}
  deriving (Show, Eq, Generic)

instance FromJSON RawScore where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawScore where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

---
{-
data RawMove
  = RawClaim { rcPunter :: PunterId, rcSource :: SiteId, rcTarget :: SiteId }
  | RawPass  { rpPunter :: PunterId }
  deriving (Show, Eq, Generic)

instance FromJSON RawMove where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawMove where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions {
       sumEncoding = ObjectWithSingleField
     , constructorTagModifier = map toLower . drop 3
     }


toRawMove :: Move -> RawMove
toRawMove (Claim {..}) = RawClaim {
    rcPunter = claimPunterId
  , rcSource = fst claimEdge
  , rcTarget = snd claimEdge}

toRawMove (Pass {..}) = RawPass { rpPunter = passPunterId }



newtype RawSGameStateStandalone = RawSGameStateStandalone { rssState :: RawSGameState } deriving (Show, Eq, Generic)

instance FromJSON RawSGameStateStandalone where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawSGameStateStandalone where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions



newtype RawMoveState = RawMoveState (RawMove, RawSGameStateStandalone) deriving (Show, Eq, Generic)

instance FromJSON RawMoveState where
  parseJSON o@(Object v) = do
    rogMove  <- parseJSON o
    rogState <- RawSGameStateStandalone <$> (v .: "state")
    return $ RawMoveState (rogMove, rogState)

  parseJSON _ = error "type mismatch"

-- may cause problems
instance ToJSON RawMoveState where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions {sumEncoding = ObjectWithSingleField}

{-
-- BoardMap

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


data RawBoardMap = RawBoardMap {
    mapSites  :: [Site]
  , mapRivers :: [River]
  , mapMines :: Set SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON BoardMap where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON BoardMap where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

---- Moves

data Move
  = Claim {claimPunter :: PunterId, claimSource :: SiteId, claimTarget :: SiteId}
  | Pass  {passPunter :: PunterId}
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


data MovesScores = MovesScores { msMoves :: [Move], msScores :: [Score] }
    deriving (Show, Eq, Generic)

instance FromJSON Moves where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Moves where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

----
-}
-}

instance FromJSON RawIncomingMessage where
  parseJSON = genericParseJSON jsonOptions

instance FromJSON RawOutgoingMessage where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawIncomingMessage where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions

instance ToJSON RawOutgoingMessage where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


-- debug = encode $ RIScore {
--       risStop = RawMovesScores {
--           rmsMoves = RawMoves [
--             RawMoveClaim RawClaim {rcPunter = 1, rcSource = 4, rcTarget = 2 }
--           , RawMoveClaim RawClaim {rcPunter = 2, rcSource = 3, rcTarget = 4 }
--           , RawMovePass  RawPass  {rpPunter = 3 }]
--            , rmsScores = [
--                  RawScore { rscPunter = 1, rscScore = 3 }
--                , RawScore { rscPunter = 2, rscScore = 2 }
--                , RawScore { rscPunter = 3, rscScore = 1 }]}
--       , risState = ""}
{-
Move =
  {"claim" : {"punter" : PunterId, "source" : SiteId, "target" : SiteId}}
| {"pass" : {"punter" : PunterId}}


-- BoardMap
-- Moves
-- Moves+Scores
-- SGameState



0. P → S{"me" : name}
1. P → S{"ready" : p, "state" : state}
2. P → S move + {"state" : state0}

0. S → P{"you" : name}
1. S → P{"punter" : p, "punters" : n, "map" : map}
2. S → P{"move" : {"moves" : moves}, "state" : state}
3. S → P{"stop" : {"moves" : moves, "scores" : scores}, "state" : state}
4. S → P{"timeout" : t}

-}
