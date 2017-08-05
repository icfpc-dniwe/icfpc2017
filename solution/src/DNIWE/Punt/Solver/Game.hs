module DNIWE.Punt.Solver.Game where

import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score

startingGame :: StartingBoard -> Futures -> Game
startingGame board futures = Game { gameBoard = emap (const notTaken) $ sbBoard board
                          , gameMines = sbMines board
                          , gameScoring = boardScores board
                          , gameFutures = futures
                          }
  where notTaken = EdgeContext { taken = Nothing }

relabelEdge :: DynGraph gr => LEdge b -> gr a b -> gr a b
relabelEdge e@(a, b, _) = insEdge e . delEdge (a, b)

applyMove :: Player -> Edge -> Game -> Game
applyMove p (a, b) game = game { gameBoard = relabelEdge (a, b, EdgeContext { taken = Just p }) $ gameBoard game }
