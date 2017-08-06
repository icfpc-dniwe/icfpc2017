module DNIWE.Punt.Interface.Protocol where
 -- export all

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Graph.Inductive.Graph (mkGraph)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, Result(..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Parser as JSONP
import Data.Conduit (ConduitM, await, yield)

import DNIWE.Punt.Interface.Types
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game


boardFromMap :: BoardMap -> StartingBoard
boardFromMap (BoardMap {..}) = StartingBoard { sbBoard = mkGraph nodes edges
                                             , sbMines = mapMines
                                             }
  where nodes = map (\(Site {..}) -> (siteId, ())) mapSites
        edges = map (\(River {..}) -> (riverSource, riverTarget, ())) mapRivers

messageParser :: P.Parser JSON.Value
messageParser = do
  size <- P.decimal
  _ <- P.char ':'
  msg <- P.take size
  case P.parseOnly JSONP.json msg of
    Left e -> fail e
    Right r -> return r

encodeMessage :: JSON.Value -> B.ByteString
encodeMessage val = B.pack (show $ B.length encoded) <> ":" <> encoded
  where encoded = BL.toStrict $ JSON.encode val

awaitJSON :: (Monad m, FromJSON a) => ConduitM JSON.Value b m a
awaitJSON = await >>= \case
  Nothing -> fail "Session has been finished prematurely"
  Just v  -> case fromJSON v of
    Error e   -> fail e
    Success x -> return x

yieldJSON :: (Monad m, ToJSON a) => a -> ConduitM i JSON.Value m ()
yieldJSON = yield . toJSON

makeMove :: PunterId -> Maybe GameMove -> Move
makeMove myId (Just (MoveClaim (a, b))) = Claim { claimPunter = myId, claimSource = a, claimTarget = b }
makeMove myId (Just (MoveSplurge es)) = Splurge { splurgePunter = myId, splurgeRoute = edgesToRoute es }
makeMove myId (Just MovePass) = Pass { passPunter = myId }
makeMove myId Nothing = Pass { passPunter = myId }

applyMove :: Move -> GameState -> GameState
applyMove (Pass _) state = state
applyMove (Claim {..}) state = performClaim claimPunter (claimSource, claimTarget) state
applyMove (Splurge {..}) state = performSplurge splurgePunter (zip (init splurgeRoute) $ drop 1 splurgeRoute) state
