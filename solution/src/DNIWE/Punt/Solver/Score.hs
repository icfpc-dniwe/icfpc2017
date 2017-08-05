module DNIWE.Punt.Solver.Score
  ( boardScores
  , playerScore
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
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

playerScore ::  Game -> Int
playerScore (Game {..}) = sum (concatMap (\m -> map (defaultScore m) $ curReachable m) $ S.toList gameMines) + (sum $ map (\ftr -> futureScore ftr) $ gameFutures M.! gamePlayer)
  where curReachable = mineReachable gamePlayer gameBoard

        defaultScore mine n = defaultScoringFunction $ gameScoring M.! mine M.! n
        futureScore (Future mine target) = if target `elem` curReachable mine then ftrScore else -ftrScore
          where ftrScore = futuresScoringFunction $ gameScoring M.! mine M.! target

mineReachable :: Player -> Board -> Node -> [Node]
mineReachable player graph start = xdfsWith marked (\(_, n, _, _) -> n) [start] graph
  where marked (from, _, _, to) = map snd $ filterTaken from ++ filterTaken to
        filterTaken = filter ((== Just player) . edgeTaken . fst)
