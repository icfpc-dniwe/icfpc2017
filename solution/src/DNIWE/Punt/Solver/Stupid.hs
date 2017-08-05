module DNIWE.Punt.Solver.Stupid where

import Data.Maybe
import Data.List
import Data.Ord
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score
import DNIWE.Punt.Solver.Game

stupidSolver :: Game -> Maybe (Node, Node)
stupidSolver g@(Game {..}) =
  fmap fst $ listToMaybe $ sortBy (comparing (Down . snd))
  $ map (\(a, b, _) -> ((a, b), predictScore (a, b)))
  $ freeEdges g
  where predictScore (a, b) = playerScore $ g { gameBoard = relabelEdge (a, b, EdgeContext { edgeTaken = Just gamePlayer }) gameBoard }

treeBuilding ::  Game -> GameTree
treeBuilding game@(Game {..}) =
  GameTree { treeState = game
           , treeActions = map (\e -> (estimateAction game e, undefined)) $ freeEdges game
           }

getScore = undefined

estimateAction :: Game -> LEdge EdgeContext -> Action
estimateAction game (node1, node2, ctx) =
  Action { actionScore = getScore node1 + getScore node2
         , actionEdge = (node1, node2)
         , actionFeatures = [0]
         }
