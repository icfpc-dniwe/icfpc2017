module DNIWE.Punt.Solver.Game where

import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types

relabelEdge :: DynGraph gr => LEdge b -> gr a b -> gr a b
relabelEdge e@(a, b, _) = insEdge e . delEdge (a, b)

applyMove :: Player -> Edge -> Game -> Game
applyMove p (a, b) game = game { gameBoard = relabelEdge (a, b, EdgeContext { taken = Just p }) $ gameBoard game }

nodeReachable :: Board -> Node -> Node -> Bool
nodeReachable graph src dst = dst `elem` (udfs [src])