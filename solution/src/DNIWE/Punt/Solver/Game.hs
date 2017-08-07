module DNIWE.Punt.Solver.Game where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score

-- heuristics
defaultLookupDepth :: Int
defaultLookupDepth = 3

-- game moves

performClaim :: PunterId -> Edge -> GameState -> GameState
performClaim p edge state = state { stateTaken = M.insert edge p $ stateTaken state }

performOption :: PunterId -> Edge -> GameState -> GameState
performOption p edge state@(GameState {..})
  | stateRemainingOptions IM.! p >= 0 = state { stateTaken = M.insert edge p stateTaken
                                             , stateRemainingOptions = IM.adjust (subtract 1) p stateRemainingOptions
                                             }
  | otherwise = error "no more options available!"

performSplurge :: PunterId -> [Edge] -> GameState -> GameState
performSplurge _ [] state = state
performSplurge p (h:t) state = performSplurge p t $ performClaim p h state

-- TODO: remove
preGameMoveToFutureEdge :: GameMove -> Maybe Edge
preGameMoveToFutureEdge (MoveClaim e) = Just e
preGameMoveToFutureEdge (MoveOption e) = Just e
preGameMoveToFutureEdge (MoveSplurge es) = Just $ head es
preGameMoveToFutureEdge (MovePass) = Nothing

-- other

gameData :: StartingBoard -> [Future] -> PunterId -> Int -> GameSettings -> GameData
gameData board futures playerId totalPlayers settings =
  GameData { gameStarting = board
           , gameScoring = boardScores board
           , gameFutures = IM.singleton playerId futures
           , gameMyId = playerId
           , gamePlayersN = totalPlayers
           , gameEdgesNearMines = edgesNearMines defaultLookupDepth board
           , gameSettings = settings
           }

gamePlayers :: GameData -> [PunterId]
gamePlayers game = [0..gamePlayersN game - 1]

initialState :: GameData -> GameState
initialState game = GameState { stateTaken = M.empty
                              , statePlayer = gameMyId game
                              , stateRemainingOptions = if settingsOptions (gameSettings game)
                                                        then IM.fromList $ zip (gamePlayers game) (repeat $ IS.size $ sbMines $ gameStarting game)
                                                        else IM.empty
                              }


freeEdges :: GameData -> GameState -> [Edge]
freeEdges game state = filter (not . (`M.member` stateTaken state)) $ map toEdge $ labEdges $ sbBoard $ gameStarting game

maybeFuture :: GameData -> Edge -> Maybe Edge
maybeFuture game (a, b)
  | aMine && not bMine = Just (a, b)
  | bMine && not aMine = Just (b, a)
  | otherwise = Nothing

  where aMine = mineCheck a 
        bMine = mineCheck b
        mineCheck n = n `IS.member` sbMines (gameStarting game)

nextPlayer :: GameData -> PunterId -> PunterId
nextPlayer game n = (n + 1) `mod` gamePlayersN game

edgesToRoute' :: [Edge] -> [Node] -> [Node]
edgesToRoute' ((src,dst):t) [] = edgesToRoute' t [src,dst]
edgesToRoute' [] res = res
edgesToRoute' ((src,dst):t) res = edgesToRoute' t $ if src == last res then res ++ [dst] else res ++ [src]

edgesToRoute :: [Edge] -> [Node]
edgesToRoute es = edgesToRoute' es []
