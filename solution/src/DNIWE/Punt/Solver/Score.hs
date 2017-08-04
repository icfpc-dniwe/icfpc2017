module DNIWE.Punt.Solver.Score
  ( boardGame
  , playerScore
  ) where

import Control.Arrow
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.SP

import DNIWE.Punt.Solver.Types

boardGame :: Board -> Game
boardGame gameBoard = Game {..}
  where gameMines = S.fromList $ map fst $ filter (isMine . snd) $ labNodes gameBoard
        gameScoring = M.unionsWith M.union $ map scoreOne $ S.toList gameMines
        scoreOne mine = M.singleton mine $ M.fromList $ map (\(LP ((dest, score):_)) -> (dest, score * score)) $ spTree mine weightedBoard
        weightedBoard = emap (const 1) gameBoard

playerScore :: Game -> Player -> Int
playerScore (Game {..}) player = sum $ concatMap (\m -> map (\n -> gameScoring M.! m M.! n) $ reachedOne m) $ S.toList gameMines
  where reachedOne mine = mineReachable gameBoard player mine

mineReachable :: Board -> Player -> Node -> [Node]
mineReachable graph player start = xdfsWith marked (\(_, n, _, _) -> n) [start] graph
  where marked (from, _, _, to) = map snd $ filterTaken from ++ filterTaken to
        filterTaken = filter ((== Just player) . taken . fst)
