module DNIWE.Punt.Solver.Game where

import Data.Maybe
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score

-- game moves

performClaim :: PunterId -> Edge -> GameState -> GameState
performClaim p edge state = state { stateBoard = relabelEdge (toLEdge edge $ EdgeContext { edgeTaken = Just p }) $ stateBoard state }

performSplurge :: PunterId -> [Edge] -> GameState -> GameState
performSplurge _ [] state = state
performSplurge p (h:t) state = performSplurge p t $ performClaim p h state

-- TODO: remove
preGameMoveToFutureEdge :: GameMove -> Maybe Edge
preGameMoveToFutureEdge (MoveClaim e) = Just e
preGameMoveToFutureEdge (MoveSplurge es) = Just $ head es
preGameMoveToFutureEdge (MovePass) = Nothing

-- other

relabelEdge :: LEdge b -> Gr a b -> Gr a b
relabelEdge e@(a, b, _) g = insEdge e $ delEdge (a, b) g

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

edgesToRoute' :: [Edge] -> [Node] -> [Node]
edgesToRoute' ((src,dst):t) [] = edgesToRoute' t [src,dst]
edgesToRoute' [] res = res
edgesToRoute' ((src,dst):t) res = edgesToRoute' t $ if src == last res then res ++ [dst] else res ++ [src]

edgesToRoute :: [Edge] -> [Node]
edgesToRoute es = edgesToRoute' es []