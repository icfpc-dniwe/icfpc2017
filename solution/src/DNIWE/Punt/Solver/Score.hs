module DNIWE.Punt.Solver.Score
  (
  ) where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.MST

import DNIWE.Punt.Solver.Types

score :: Board -> Player -> Int
score board player = sum $ map (\n -> mineScore n playerBoard) mines
  where playerBoard = emap (maybe False (== player) . taken) board
        mines = map fst $ filter (isMine . snd) $ labNodes board

mineReachable :: Node -> Gr a Bool -> Set Node
mineReachable start graph = S.fromList $ xdfsWith marked (\(_, n, _, _) -> n) [start] graph
  where marked (from, n, _, to) = map snd $ filter fst from ++ filter fst to

mineTree :: Node -> Gr Bool b -> Int
mineTree = undefined

mineScore :: Node -> Gr a Bool -> Int
mineScore start graph = undefined
  where reachable = mineReachable start graph
        newGraph = gmap (\(l, n, c, r) -> (l, n, n `S.member` reachable, r)) graph
