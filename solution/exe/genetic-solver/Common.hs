{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Common where

import Data.Proxy (Proxy(..))
import qualified Data.Foldable as Foldable (toList)
import Data.Sequence (Seq(..), ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Data.Default (Default, def)
import Data.Graph.Inductive.Graph (LEdge, Edge, Node, mkGraph, gmap, emap, toEdge, toLEdge, labEdges, edgeLabel)
import Data.Graph.Inductive.PatriciaTree (Gr)

import DNIWE.Punt.Solver.Types (StartingBoard(..), Future(..), PunterId, MineScores)
import DNIWE.Punt.Solver.Game ()
import DNIWE.Punt.Solver.Score (boardScores)
import GHC.Generics (Generic)

import Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as Vector

import Data.Set (Set)
import qualified Data.Set as Set

import DNIWE.Punt.Solver.Game (relabelEdge)

emap' :: ((Node, Node) -> e1 -> e2) -> Gr v e1 -> Gr v e2
emap' f = gmap f' where
  f' (is, n1, nc, os) = (map fi is, n1, nc, map fo os) where
    fi (b, n0) = (f (n0, n1) b, n0)
    fo (b, n2) = (f (n1, n2) b, n2)

-----

data Board = Board {
    boardMap   :: Gr () Bool {- Bool: is free? -}
  , boardMines :: Set Node}
  deriving (Show, Eq, Generic)


mkInitialBoard :: [(Int, Bool)] -> [(Int, Int)] -> Board
mkInitialBoard sites rivers = Board {
    boardMap = mkGraph
      (map (\(a, _) -> (a, ())) sites)
      (map (\(a, b) -> (a, b, True)) rivers)
  , boardMines = Set.fromList (map fst . filter snd $ sites)}

-----


-- data GameState = GameState {
--      gsBoard      :: Board
--    , gsPlayersNum :: Int}
--   deriving (Show, Eq, Generic)

{-
{-
mkGameData :: StartingBoard -> [Future] -> PunterId -> Int -> GameData
mkGameData startingBoard futures playerId playersNum = GameData {
    gdStartingBoard = startingBoard
  , gdScoring       = boardScores startingBoard
  , gdFutures       = futures
  , gdPlayerId      = playerId
  , gdPlayersNum    = playersNum}
-}




data PlayerState = PlayerState {
    psScoring  :: MineScores
  , psFutures  :: [Future]
  , psPlayerId :: PunterId}
  deriving (Show, Eq, Generic)



data EdgeContext = EdgeContext {
    ecFree     :: Bool
  , ecFeatures :: Vector Double
  , ecResult   :: Double}
  deriving (Show, Eq, Ord, Generic)

instance Default EdgeContext where
  def = EdgeContext {
      ecFree = True
    , ecFeatures = Vector.replicate featuresNum 0
    , ecResult = 0.0}


updateFeatures
  :: (Int, Int)
  -> EdgeContext
  -> EdgeContext
updateFeatures (n1, n2) e = e { ecFeatures = ecFeatures' } where
  ecFeatures' = (ecFeatures e) // [
      (0, if ecFree e then 1.0 else 0.0)
    , (1, 0.5 * (fromIntegral $ n1 + n2))]


featuresNum :: Int
featuresNum = 16


-- data GameState = GameState {gsBoard  :: Gr () EdgeContext}
--   deriving (Show, Eq, Generic)
--
-- mkInitialState :: GameData -> GameState
-- mkInitialState (GameData {..}) = GameState {
--   gsBoard  = emap' updateFeatures . emap (const def) . sbBoard $ gdStartingBoard } where


applyMove :: PunterId -> LEdge EdgeContext -> GameState -> GameState
applyMove p (n1, n2, e) state = state {
  gsBoard = relabelEdge (n1, n2, e {ecFree = False}) $ gsBoard state }



-- playerScore :: GameData -> GameState -> Int
-- playerScore (GameData {..}) (GameState {..}) = totalDefault + futureDefault where
--   curReachable = mineReachable statePlayer stateBoard
--         totalDefault = sum $ concatMap (\m -> map (defaultScore m) $ curReachable m) $ S.toList $ sbMines gameStarting
--         futureDefault = sum $ map (\ftr -> futureScore ftr) $ M.findWithDefault [] statePlayer gameFutures
--
--         defaultScore mine n = defaultScoringFunction $ gameScoring M.! mine M.! n
--         futureScore (Future mine target) = if target `elem` curReachable mine then ftrScore else -ftrScore
--           where ftrScore = futuresScoringFunction $ gameScoring M.! mine M.! target
--
-- mineReachable :: PunterId -> Board -> Node -> [Node]
-- mineReachable player graph start = xdfsWith (map snd . filterTaken . lneighbors') node' [start] graph
--   where filterTaken = filter ((== Just player) . edgeTaken . fst)


-}

data Move
  = Claim (Node, Node)
  | Pass
  deriving (Show, Eq)

isPass :: Move -> Bool
isPass Pass = True
isPass _    = False

fromClaim :: Move -> (Node, Node)
fromClaim (Claim x) = x
fromClaim _         = error "not supported"


relabelEdge' :: (Node, Node) -> (e -> e) -> Gr v e -> Gr v e
relabelEdge' (n1, n2) f = emap' f' where
  f' (n1', n2') e = if (n1 == n1' && n2 == n2') then (f e) else e


class Player p where
  initialState :: PunterId -> Int -> Board -> Proxy p -> IO p
  updateState  :: [(PunterId, Move)] -> p -> IO p
  playerId     :: p -> PunterId
  makeMove     :: p -> IO (p, Move)
