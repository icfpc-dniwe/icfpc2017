module DNIWE.Punt.Solver.Score
  ( boardScores
  , playerScore
  ) where

import qualified Data.Set as S
import qualified Data.IntMap.Strict as M
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.SP

import DNIWE.Punt.Solver.Types

defaultScoringFunction :: Int -> Int
defaultScoringFunction = (^ (2 :: Int))

futuresScoringFunction :: Int -> Int
futuresScoringFunction = (^ (3 :: Int))

boardScores :: StartingBoard -> MineScores
boardScores (StartingBoard {..}) = M.unionsWith M.union $ map scoreOne $ S.toList sbMines
  where scoreOne mine = M.singleton mine $ M.fromList $ map (head . unLPath) $ spTree mine weightedBoard
        weightedBoard = undir $ emap (const 1) sbBoard

playerScore :: GameData -> GameState -> Int
playerScore (GameData {..}) (GameState {..}) = totalDefault + futureDefault
  where curReachable = mineReachable statePlayer stateBoard

        totalDefault = sum $ concatMap (\m -> map (defaultScore m) $ curReachable m) $ S.toList $ sbMines gameStarting
        futureDefault = sum $ map (\ftr -> futureScore ftr) $ M.findWithDefault [] statePlayer gameFutures

        defaultScore mine n = defaultScoringFunction $ gameScoring M.! mine M.! n
        futureScore (Future mine target) = if target `elem` curReachable mine then ftrScore else -ftrScore
          where ftrScore = futuresScoringFunction $ gameScoring M.! mine M.! target

mineReachable :: PunterId -> Board -> Node -> [Node]
mineReachable player graph start = xdfsWith (map snd . filterTaken . lneighbors') node' [start] graph
  where filterTaken = filter ((== Just player) . edgeTaken . fst)
