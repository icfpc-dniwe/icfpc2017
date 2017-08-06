module DNIWE.Punt.Solver.Game where

import Control.Monad
import Data.Maybe
import Data.Tree (Tree)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Control.Monad.Random.Class
import qualified Data.Graph.Inductive.Query.DFS as DFS

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score

-- game moves

performClaim :: PunterId -> Edge -> GameState -> GameState
performClaim p edge state = state { stateTaken = M.insert edge p $ stateTaken state }

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
           , gameFutures = IM.singleton playerId futures
           , gameMyId = playerId
           , gamePlayersN = totalPlayers
           }

initialState :: GameData -> GameState
initialState (GameData {..}) = GameState { stateTaken = M.empty
                                         , statePlayer = gameMyId
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
        mineCheck n = n `S.member` sbMines (gameStarting game)

nextPlayer :: GameData -> PunterId -> PunterId
nextPlayer game n = (n + 1) `mod` gamePlayersN game

edgesToRoute' :: [Edge] -> [Node] -> [Node]
edgesToRoute' ((src,dst):t) [] = edgesToRoute' t [src,dst]
edgesToRoute' [] res = res
edgesToRoute' ((src,dst):t) res = edgesToRoute' t $ if src == last res then res ++ [dst] else res ++ [src]

edgesToRoute :: [Edge] -> [Node]
edgesToRoute es = edgesToRoute' es []

randomVersatile :: MonadRandom m => Float -> [Node] -> Gr a b -> m [Tree Node]
randomVersatile percentage starts graph = do
  let maybeDrop x = do
        chance <- getRandom
        if chance <= percentage
          then return $ Just x
          else return Nothing
  dropped <- liftM (S.fromList . catMaybes) $ mapM (maybeDrop . toEdge) $ labEdges graph

  let filterDropped = filter (not . (`S.member` dropped))
      filterContext ctx = map snd (filterDropped $ map (node' ctx, ) $ pre' ctx) ++ map fst (filterDropped $ map (, node' ctx) $ suc' ctx)

  return $ DFS.xdffWith filterContext node' starts graph
