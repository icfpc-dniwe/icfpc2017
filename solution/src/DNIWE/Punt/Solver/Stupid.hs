module DNIWE.Punt.Solver.Stupid where

import Data.Maybe
import Data.List
import Data.Ord
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score

relabelEdge :: DynGraph gr => LEdge b -> gr a b -> gr a b
relabelEdge e@(a, b, _) = insEdge e . delEdge (a, b)

stupidSolver :: Game -> Maybe (Node, Node)
stupidSolver g@(Game {..}) = fmap fst $ listToMaybe $ sortBy (comparing snd) $ map (\(a, b, _) -> ((a, b), predictScore (a, b))) $ filter (\(_, _, ctx) -> isNothing $ taken ctx) $ labEdges gameBoard
  where predictScore (a, b) = playerScore Us $ g { gameBoard = relabelEdge (a, b, EdgeContext { taken = Just Us }) gameBoard }
