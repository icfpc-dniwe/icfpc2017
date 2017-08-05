module DNIWE.Punt.Solver.Game where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score


relabelEdge :: LEdge b -> Gr a b -> Gr a b
relabelEdge e = insEdge e . delEdge (toEdge e)


gameData :: StartingBoard -> [Future] -> PunterId -> Int -> GameData
gameData board futures playerId totalPlayers =
  GameData { gameStarting = board
           , gameScoring = boardScores board
           , gameFutures = M.singleton playerId futures
           , gameMyId = playerId
           , gamePlayersN = totalPlayers
           }

initialState :: GameData -> GameState
initialState (GameData {..}) = GameState { stateBoard = emap (const initialEdge) $ sbBoard gameStarting
                                         , statePlayer = gameMyId
                                         }
  where initialEdge = EdgeContext { edgeTaken = Nothing }


applyMove :: PunterId -> Edge -> GameState -> GameState
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

nextPlayer :: GameData -> PunterId -> PunterId
nextPlayer game n = (n + 1) `mod` gamePlayersN game
