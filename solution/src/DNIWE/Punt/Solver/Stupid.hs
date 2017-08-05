module DNIWE.Punt.Solver.Stupid where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List
import Data.Ord
import Data.Graph.Inductive.Graph

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Score

newtype StupidScore = StupidScore { stupidScores :: Map PunterId Int }
                    deriving (Show)

stupidMetric :: PunterId -> StupidScore -> Int
stupidMetric player (StupidScore {..}) = myScore - othersMax
  where myScore = stupidScores M.! player
        othersMax = maximum $ map snd $ M.toList $ M.delete player stupidScores

stupidGameTree :: Int -> GameData -> GameState -> (StupidScore, GameTree StupidScore)
stupidGameTree 0 game gstate = (finalScore, finalNode)
  where finalNode = GameTree { treeState = gstate, treeActions = [] }
        finalScore = StupidScore { stupidScores = M.singleton (statePlayer gstate) (playerScore game gstate) }
stupidGameTree n game gstate = (curScore, curNode)
  where player = statePlayer gstate

        curActions = sortBy (comparing (Down . stupidMetric player . actionScore . fst)) $ map nextAction $ freeEdges gstate
        curScore = case curActions of
                     ((action, _):_) -> actionScore action
                     _ -> StupidScore { stupidScores = M.singleton player (playerScore game gstate) }

        curNode = GameTree { treeState = gstate, treeActions = curActions }

        nextAction (toEdge -> edge) = (action, newTree { treeActions = [] })
          where (score, newTree) = stupidGameTree (n - 1) game $ newState { statePlayer = nextPlayer game $ statePlayer newState }
                newState = applyMove player edge gstate
                action = Action { actionEdge = edge, actionScore = newScore }
                newScore
                  | player `M.member` stupidScores score = score
                  | otherwise = score { stupidScores = M.insert player (playerScore game $ (treeState newTree) { statePlayer = player }) $ stupidScores score }
