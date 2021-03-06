module DNIWE.Punt.Interface.Protocol where
 -- export all

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Graph.Inductive.Graph
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, Result(..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Parser as JSONP
import Data.Conduit (ConduitM, await, yield)

import DNIWE.Punt.Interface.Types
import DNIWE.Punt.Interface.Utility
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game


boardFromMap :: BoardMap -> StartingBoard
boardFromMap (BoardMap {..}) = StartingBoard { sbBoard = mkGraph sbNodes sbEdges
                                             , sbMines = mapMines
                                             }
  where sbNodes = map (\(Site {..}) -> (siteId, ())) mapSites
        sbEdges = map (\(River {..}) -> (riverSource, riverTarget, ())) mapRivers

messageParser :: P.Parser JSON.Value
messageParser = do
  msize <- P.decimal
  _ <- P.char ':'
  msg <- P.take msize
  case P.parseOnly JSONP.json msg of
    Left e -> fail e
    Right r -> return r

encodeMessage :: JSON.Value -> B.ByteString
encodeMessage val = B.pack (show $ B.length encoded) <> ":" <> encoded
  where encoded = BL.toStrict $ JSON.encode val

awaitJSON :: (Monad m, FromJSON a) => ConduitM JSON.Value b m a
awaitJSON = await >>= \case
  Nothing -> fail "Session has been finished prematurely"
  Just v  -> do
    case fromJSON v of
      Error e   -> fail e
      Success x -> return x

yieldJSON :: (Monad m, ToJSON a) => a -> ConduitM i JSON.Value m ()
yieldJSON = yield . toJSON

makeMove :: PunterId -> Maybe GameMove -> Move
makeMove myId (Just (MoveClaim (a, b))) = Claim { claimPunter = myId, claimSource = a, claimTarget = b }
makeMove myId (Just (MoveOption (a, b))) = Option { optionPunter = myId, optionSource = a, optionTarget = b }
makeMove myId (Just (MoveSplurge es)) = Splurge { splurgePunter = myId, splurgeRoute = edgesToRoute es }
makeMove myId (Just MovePass) = Pass { passPunter = myId }
makeMove myId Nothing = Pass { passPunter = myId }

applyMove :: GameData -> Move -> GameState -> GameState
applyMove _ (Pass _) state = state
applyMove game (Claim {..}) state = performClaim game claimPunter (sanitizeEdge (sbBoard $ gameStarting game) (claimSource, claimTarget)) state
applyMove game (Splurge {..}) state = performSplurge game splurgePunter (map (sanitizeEdge (sbBoard $ gameStarting game)) $ zip (init splurgeRoute) $ drop 1 splurgeRoute) state
applyMove game (Option {..}) state = performOption game optionPunter (sanitizeEdge (sbBoard $ gameStarting game) (optionSource, optionTarget)) state
