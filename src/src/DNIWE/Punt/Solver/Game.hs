module DNIWE.Punt.Solver.Game where

import Data.List
import qualified Data.IntMap.Strict as IM
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Graph.Inductive.Graph as G
import Control.Monad.Random.Class
import Data.Hashable

import DNIWE.Punt.Solver.Utility
import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score

-- heuristics
defaultLookupDepth :: Int
-- defaultLookupDepth = 3
defaultLookupDepth = 1

defaultExperimentsNum :: Int -> Int
defaultExperimentsNum = (* 2)

-- game moves

performClaim :: GameData -> PunterId -> Edge -> GameState -> GameState
performClaim game p edge state =
  state { stateTaken = M.insert edge p $ stateTaken state
        , stateClusters = newClusters
        , stateScores = IM.insertWith (+) p newScore $ stateScores state
        , stateMarginEdges = newMarginEdges
        }
  where (newScore, newClusters) = updateClusters game (state { statePlayer = p }) edge
        newMarginEdges = S.delete edge
                       . foldr (S.insert) (stateMarginEdges state)
                       . filter (flip M.notMember (stateTaken state))
                       . concatMap (\n -> edgesNearNode (stateMarginWidth state) n (sbBoard . gameStarting $ game))
                       . (\(n1, n2) -> [n1, n2])
                       $ edge

performOption :: GameData -> PunterId -> Edge -> GameState -> GameState
performOption game p edge state@(GameState {..})
  | stateRemainingOptions IM.! p >= 0 = (performClaim game p edge state) { stateRemainingOptions = IM.adjust (subtract 1) p stateRemainingOptions }
  | otherwise = error "no more options available!"

performSplurge :: GameData -> PunterId -> [Edge] -> GameState -> GameState
performSplurge game p path initState = foldl' (\state edge -> performClaim game p edge state) initState path

-- TODO: remove
preGameMoveToFutureEdge :: GameMove -> Maybe Edge
preGameMoveToFutureEdge (MoveClaim e) = Just e
preGameMoveToFutureEdge (MoveOption e) = Just e
preGameMoveToFutureEdge (MoveSplurge es) = Just $ head es
preGameMoveToFutureEdge (MovePass) = Nothing

-- other

gameData :: MonadRandom m => StartingBoard -> [Future] -> PunterId -> Int -> GameSettings -> m GameData
gameData board futures playerId totalPlayers settings = do
  let scores = boardScores board
      experimentsNum = defaultExperimentsNum $ G.size $ sbBoard board
  maxScore <- probableTotalMaxScore board scores totalPlayers experimentsNum

  return $ GameData { gameStarting = board
                    , gameScoring = scores
                    , gameFutures = IM.singleton playerId futures
                    , gameMyId = playerId
                    , gamePlayersN = totalPlayers
                    , gameEdgesNearMines = edgesNearMines defaultLookupDepth board
                    , gameSettings = settings
                    , gameMaxScore = maxScore
                    }

gamePlayers :: GameData -> [PunterId]
gamePlayers game = [0..gamePlayersN game - 1]

initialState :: GameData -> GameState
initialState game = GameState { stateTaken = M.empty
                              , statePlayer = gameMyId game
                              , stateRemainingOptions = if settingsOptions (gameSettings game)
                                                        then IM.fromList $ zip (gamePlayers game) (repeat $ IS.size $ sbMines $ gameStarting game)
                                                        else IM.empty
                              , stateClusters = IM.empty
                              , stateScores = IM.fromList $ zip (gamePlayers game) (repeat 0)
                              , stateMarginEdges = gameEdgesNearMines game
                              , stateMarginWidth = defaultLookupDepth
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

edgesNearMines :: Int -> StartingBoard -> Set Edge
edgesNearMines depth board = S.fromList $ concatMap (\n -> edgesNearNode depth n $ sbBoard board) $ IS.toList $ sbMines board

gameStateHash :: GameState -> Int
gameStateHash (GameState {..}) = hash stateTaken `hashWithSalt` statePlayer `hashWithSalt` stateRemainingOptions
