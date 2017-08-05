module DNIWE.Punt.Solver.Game where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score

startingGame :: StartingBoard -> [Future] -> Game
startingGame board futures = Game { gameBoard = emap (const notTaken) $ sbBoard board
                                  , gameMines = sbMines board
                                  , gameScoring = boardScores board
                                  , gameFutures = M.singleton Us futures
                                  , gamePlayer = Us
                                  }
  where notTaken = EdgeContext { edgeTaken = Nothing }

relabelEdge :: DynGraph gr => LEdge b -> gr a b -> gr a b
relabelEdge e@(a, b, _) = insEdge e . delEdge (a, b)

applyMove :: Player -> Edge -> Game -> Game
applyMove p (a, b) game = game { gameBoard = relabelEdge (a, b, EdgeContext { edgeTaken = Just p }) $ gameBoard game }

freeEdges :: Game -> [LEdge EdgeContext]
freeEdges = filter (\(_, _, ctx) -> isNothing $ edgeTaken ctx) . labEdges . gameBoard

maybeFuture :: Game -> Edge -> Maybe Edge
maybeFuture game (a, b)
  | aMine && not bMine = Just (a, b)
  | bMine && not aMine = Just (b, a)
  | otherwise = Nothing

  where aMine = a `S.member` gameMines game
        bMine = b `S.member` gameMines game
