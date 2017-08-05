module DNIWE.Punt.Solver.Game where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score


relabelEdge :: DynGraph gr => LEdge b -> gr a b -> gr a b
relabelEdge e@(a, b, _) = insEdge e . delEdge (a, b)


gameData :: StartingBoard -> [Future] -> PunterId -> Int -> GameData
gameData board futures playerId totalPlayers =
  GameData { gameStarting = board
           , gameScoring = boardScores board
           , gameFutures = M.singleton Us futures
           , gameBeforeN = playerId
           , gameAfterN = totalPlayers - (playerId + 1)
           }

initialState :: GameData -> GameState
initialState (GameData {..}) = GameState { stateBoard = emap (const initialEdge) $ sbBoard gameStarting
                                         , statePlayer = Us
                                         }
  where initialEdge = EdgeContext { edgeTaken = Nothing }


applyMove :: Player -> Edge -> GameState -> GameState
applyMove p edge state = state { stateBoard = relabelEdge (toLEdge edge $ EdgeContext { edgeTaken = Just p }) $ stateBoard state }

freeEdges :: GameState -> [LEdge EdgeContext]
freeEdges = filter (isNothing . edgeTaken . edgeLabel) . labEdges . stateBoard

maybeFuture :: GameData -> Edge -> Maybe Edge
maybeFuture game (a, b)
  | aMine && not bMine = Just (a, b)
  | bMine && not aMine = Just (b, a)
  | otherwise = Nothing

  where aMine = mineCheck a 
        bMine = mineCheck b
        mineCheck n = n `S.member` sbMines (gameStarting game)

nextPlayer :: GameData -> Player -> Player
nextPlayer game (Before n)
  | n + 1 == gameBeforeN game = Us
  | otherwise = Before (n + 1)
nextPlayer game Us
  | gameAfterN game == 0 =
    if gameBeforeN game == 0
    then Us
    else Before 0
  | otherwise = After 0
nextPlayer game (After n)
  | n + 1 == gameAfterN game =
    if gameBeforeN game == 0
    then Us
    else Before 0
  | otherwise = After (n + 1)
