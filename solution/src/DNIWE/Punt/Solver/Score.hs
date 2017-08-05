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
defaultScoringFunction = (^ 2)

futuresScoringFunction :: Int -> Int
futuresScoringFunction = (^ 3)

boardScores :: StartingBoard -> MineScores
boardScores (StartingBoard {..}) = M.unionsWith M.union $ map scoreOne $ S.toList sbMines
  where scoreOne mine = M.singleton mine $ M.fromList $ map (head . unLPath) $ spTree mine weightedBoard
        weightedBoard = undir $ emap (const 1) sbBoard

playerScore :: Player -> Game -> Int
playerScore player (Game {..}) = sum (concatMap (\m -> map (defaultScore m) $ reachedOne m) $ S.toList gameMines) + (sum $ map (\ftr -> futureScore ftr) $ gameFutures M.! player)
  where reachedOne mine = mineReachable player gameBoard mine
  
        defaultScore mine n = defaultScoringFunction $ gameScoring M.! mine M.! n
        futureScore (Future {..}) = if target `elem` (mineReachable player gameBoard mine) then ftrScore else -ftrScore
          where ftrScore = futuresScoringFunction $ gameScoring M.! mine M.! target

mineReachable :: Player -> Board -> Node -> [Node]
mineReachable player graph start = xdfsWith marked (\(_, n, _, _) -> n) [start] graph
  where marked (from, _, _, to) = map snd $ filterTaken from ++ filterTaken to
        filterTaken = filter ((== Just player) . taken . fst)
