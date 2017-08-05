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


type Nat = Int
type RawSGameState = Text


data SGameState = SGameState -- TODO: here goes custom structure: gamedata + gamestate
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
     | ISetup { isPunterId :: PunterId, isPunters :: Nat, isGameBoard :: StartingBoard }
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
fromRaw (RISetup    {..}) = ISetup { isPunterId = risPunter, isPunters = risPunters, isGameBoard = boardFromMap risMap }
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
     = RIHandshake { rihYou     :: Text                                                                }
     | RISetup     { risPunter  :: PunterId      , risPunters :: Nat           , risMap :: RawGameMap  }
     | RIGameplay  { rigMove    :: RawMoves      , rigState   :: RawSGameState                         }
     | RIScore     { risStop    :: RawMovesScores, risState   :: RawSGameState                         }
     | RITimeout   { ritTimeout :: RawSGameState                                                       }
  deriving (Show, Eq, Generic)

data RawOutgoingMessage
     = ROHandshake      { rohMe    :: Text                            }
     | ROSetup          { rosReady :: PunterId, rosState :: RawSGameState }
     | ROGameplayClaim  { rogcState :: RawSGameState, rogcClaim :: RawClaim }
     | ROGameplayPass   { rogpState :: RawSGameState, rogpPass :: RawPass }
  deriving (Show, Eq, Generic)


-- Curses on you!

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


data RawGameMap = RawGameMap {
    rgmSites  :: [Site]
  , rgmRivers :: [River]
  , rgmMines  :: Set SiteId
  } deriving (Show, Eq, Generic)

instance FromJSON RawGameMap where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON RawGameMap where
  toJSON = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions


boardFromMap :: RawGameMap -> StartingBoard
boardFromMap (RawGameMap {..}) = StartingBoard {
    sbBoard = mkGraph nodes edges
  , sbMines = rgmMines } where
  nodes = map (\(Site {..}) -> (siteId, ())) rgmSites
  edges = map (\(River {..}) -> (riverSource, riverTarget, ())) rgmRivers


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
