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
boardGame (IndexedBoard {..}) = M.unionsWith M.union $ map scoreOne $ S.toList ibMines
  where scoreOne mine = M.singleton mine $ M.fromList $ map (\(LP ((dest, score):_)) -> (dest, score * score)) $ spTree mine weightedBoard
        weightedBoard = undir $ emap (const 1) ibBoard

playerScore :: Player -> Game -> Int
playerScore player (Game {..}) = sum $ concatMap (\m -> map (\n -> gameScoring M.! m M.! n) $ reachedOne m) $ S.toList gameMines + 
  where reachedOne mine = mineReachable player gameBoard mine
  		futureScore ftr@{mine, target} = if nodeReachable mine target then 

mineReachable :: Player -> Board -> Node -> [Node]
mineReachable player graph start = xdfsWith marked (\(_, n, _, _) -> n) [start] graph
  where marked (from, _, _, to) = map snd $ filterTaken from ++ filterTaken to
        filterTaken = filter ((== Just player) . taken . fst)
