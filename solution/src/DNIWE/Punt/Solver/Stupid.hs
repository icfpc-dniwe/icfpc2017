module DNIWE.Punt.Solver.Stupid where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Data.Ord
import Data.Graph.Inductive.Graph
import Data.Hashable
import Control.DeepSeq
import GHC.Generics (Generic)

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Score

newtype StupidScore = StupidScore { stupidScores :: Map PunterId Int }
                    deriving (Show, Generic)

instance NFData StupidScore where

stupidMetric :: PunterId -> StupidScore -> Score
stupidMetric player (StupidScore {..}) = myScore - othersMax
  where myScore = stupidScores M.! player
        othersMax = foldl' max 0 $ map snd $ M.toList $ M.delete player stupidScores

stupidGameTree :: Int -> GameData -> GameState -> [GameMove]
stupidGameTree n game state = resMoves
  where (_, _, resEdges) = stupidGameTree' n IS.empty game state
        resMoves = map (\e -> MoveClaim e) resEdges

stupidGameTree' ::  Int -> IntSet -> GameData -> GameState -> (IntSet, StupidScore, [Edge])
stupidGameTree' 0 visitedStates game gstate = (visitedStates, finalScore, [])
  where finalScore = StupidScore { stupidScores = M.singleton (statePlayer gstate) (playerScore game gstate) }
stupidGameTree' n visitedStates game gstate = (newVisitedStates, curScore, curEdges)
  where player = statePlayer gstate

        (newVisitedStates, results) = force $ stupidNextAction n game gstate visitedStates $ freeEdges game gstate
        curActions = sortBy (comparing (Down . stupidMetric player . fst)) results
        curEdges = map snd curActions
        curScore = case curActions of
                     (score, _):_ -> score
                     _ -> StupidScore { stupidScores = M.singleton player (playerScore game gstate) }
 
stupidNextAction :: Int -> GameData -> GameState -> IntSet -> [Edge] -> (IntSet, [(StupidScore, Edge)])
stupidNextAction n game gstate startVisited = foldl' process (startVisited, [])
  where process (oldVisited, oldEdges) edge
          | newHash `IS.member` oldVisited = (oldVisited, oldEdges)
          | otherwise = (newVisited, (newScore, edge):oldEdges)

          where player = statePlayer gstate

                newState' = performClaim player edge gstate
                newState = newState' { statePlayer = nextPlayer game $ statePlayer newState' }
                newHash = hash newState

                (curVisited, score, _) = stupidGameTree' (n - 1) oldVisited game newState
                newVisited = IS.insert newHash curVisited
  
                newScore
                  | player `M.member` stupidScores score = score
                  | otherwise = score { stupidScores = M.insert player (playerScore game newState') $ stupidScores score }

{-# INLINE stupidNextAction #-}
