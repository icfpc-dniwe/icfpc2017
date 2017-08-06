module DNIWE.Punt.Solver.Stupid where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.Ord
import Data.Maybe
import Data.Hashable
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Score

newtype StupidScore = StupidScore { stupidScores :: Map PunterId Int }
                    deriving (Show)

stupidMetric :: PunterId -> StupidScore -> Int
stupidMetric player (StupidScore {..}) = myScore - othersMax
  where myScore = stupidScores M.! player
        othersMax = foldr max 0 $ map snd $ M.toList $ M.delete player stupidScores

gameStateHash :: GameState -> Int
gameStateHash (GameState {..}) = hash (mapMaybe filterTaken $ labEdges stateBoard) `hashWithSalt` statePlayer
  where filterTaken e
          | isJust $ edgeTaken $ edgeLabel e = Just $ toEdge e
          | otherwise = Nothing

stupidGameTree :: Int -> GameData -> GameState -> [Edge]
stupidGameTree n game state = resEdges
  where (_, _, resEdges) = stupidGameTree' n IS.empty game state

stupidGameTree' ::  Int -> IntSet -> GameData -> GameState -> (IntSet, StupidScore, [Edge])
stupidGameTree' 0 visitedStates game gstate = (visitedStates, finalScore, [])
  where finalScore = StupidScore { stupidScores = M.singleton (statePlayer gstate) (playerScore game gstate) }
stupidGameTree' n visitedStates game gstate = (newVisitedStates, curScore, curEdges)
  where player = statePlayer gstate

        (newVisitedStates, results) = nextAction visitedStates $ freeEdges gstate
        curActions = sortBy (comparing (Down . stupidMetric player . fst)) results
        curEdges = map snd curActions
        curScore = case curActions of
                     (score, _):_ -> score
                     _ -> StupidScore { stupidScores = M.singleton player (playerScore game gstate) }

        nextAction oldVisited [] = (oldVisited, [])
        nextAction oldVisited ((toEdge -> edge):others)
          | newHash `IS.member` oldVisited = nextAction oldVisited others
          | otherwise = (newVisited, (newScore, edge):nextEdges)

          where newState' = applyMove player edge gstate
                newState = newState' { statePlayer = nextPlayer game $ statePlayer newState' }
                newHash = gameStateHash newState

                (curVisited, score, _) = stupidGameTree' (n - 1) oldVisited game newState
                curVisited' = IS.insert newHash curVisited

                (newVisited, nextEdges) = nextAction curVisited' others

                newScore
                  | player `M.member` stupidScores score = score
                  | otherwise = score { stupidScores = M.insert player (playerScore game $ newState' { statePlayer = player }) $ stupidScores score }
