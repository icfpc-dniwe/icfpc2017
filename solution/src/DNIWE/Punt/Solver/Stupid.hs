module DNIWE.Punt.Solver.Stupid where

import Data.Maybe
import Data.List
import Data.Ord
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types

stupidSolver :: GameTree -> Maybe (Node, Node)
stupidSolver = listToMaybe . stupidSolver'

stupidSolver' :: GameTree -> [(Node, Node)]
stupidSolver' gtree = map actionEdge $ sortBy (comparing (Down . actionScore)) $ map fst $ treeActions gtree
