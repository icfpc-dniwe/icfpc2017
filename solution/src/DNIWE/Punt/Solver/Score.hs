module DNIWE.Punt.Solver.Score
  ( boardGame
  , playerScore
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.SP

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game

boardGame :: IndexedBoard -> MineScores
boardGame (IndexedBoard {..}) fScore = M.unionsWith M.union $ map scoreOne $ S.toList ibMines
  where scoreOne mine = M.singleton mine $ M.fromList $ map (\(LP ((dest, score):_)) -> (dest, score)) $ spTree mine weightedBoard
        weightedBoard = undir $ emap (const 1) ibBoard

playerScore :: Player -> Game -> (Int -> Int) -> (Int -> Int) -> Int
playerScore ib player (Game {..}) = sum (concatMap (\m -> map (\n -> defaultScoringFunction $ gameScoring M.! m M.! n) $ reachedOne m) $ S.toList gameMines) +
									sum $ map (\ftr -> futureScore ftr) $ S.toList $ gameFutures M.! player
  where reachedOne mine = mineReachable player gameBoard mine
  		futureScore ftr@{mine, target} = if target `elem` (mineReachable player gameBoard mine) then ftrScore else -ftrScore
  		  where ftrScore = futuresScoringFunction $ mineScores M.! mine M.! target

mineReachable :: Player -> Board -> Node -> [Node]
mineReachable player graph start = xdfsWith marked (\(_, n, _, _) -> n) [start] graph
  where marked (from, _, _, to) = map snd $ filterTaken from ++ filterTaken to
        filterTaken = filter ((== Just player) . taken . fst)
