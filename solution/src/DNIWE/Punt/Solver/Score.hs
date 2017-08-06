module DNIWE.Punt.Solver.Score
  ( boardScores
  , playerScore
  ) where

import qualified Data.Set as S
import qualified Data.IntMap.Strict as M
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Graph.Inductive.Query.BFS as BFS
import Data.Graph.Inductive.Query.SP

import DNIWE.Punt.Solver.Types

defaultScoringFunction :: Int -> Score
defaultScoringFunction = (^ (2 :: Int))

futuresScoringFunction :: Int -> Score
futuresScoringFunction = (^ (3 :: Int))

boardScores :: StartingBoard -> MineScores
boardScores (StartingBoard {..}) = M.unionsWith M.union $ map scoreOne $ S.toList sbMines
  where scoreOne mine = M.singleton mine $ M.fromList $ BFS.level mine $ undir sbBoard

playerScore :: GameData -> GameState -> Score
playerScore (GameData {..}) (GameState {..}) = totalDefault + futureDefault
  where curReachable = mineReachable statePlayer stateBoard

        totalDefault = sum $ concatMap (\m -> map (defaultScore m) $ curReachable m) $ S.toList $ sbMines gameStarting
        futureDefault = sum $ map (\ftr -> futureScore ftr) $ M.findWithDefault [] statePlayer gameFutures

        defaultScore mine n = defaultScoringFunction $ gameScoring M.! mine M.! n
        futureScore (Future mine target) = if target `elem` curReachable mine then ftrScore else -ftrScore
          where ftrScore = futuresScoringFunction $ gameScoring M.! mine M.! target

mineReachable :: PunterId -> Board -> Node -> [Node]
mineReachable player graph start = DFS.xdfsWith (map snd . filterTaken . lneighbors') node' [start] graph
  where filterTaken = filter ((== Just player) . edgeTaken . fst)
<<<<<<< HEAD

determineEdgesNearMines :: Int -> StartingBoard -> NearestEdges
determineEdgesNearMines depth (StartingBoard {..}) = map (\m -> ) $ S.toArray sbMines
=======
>>>>>>> parent of 62a0a5b... unchulhu
