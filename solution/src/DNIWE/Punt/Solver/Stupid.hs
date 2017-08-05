module DNIWE.Punt.Solver.Stupid where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List
import Data.Ord

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Score

newtype StupidScore = StupidScore { stupidScores :: Map Player Int }
                    deriving (Show)

stupidMetric :: Player -> StupidScore -> Int
stupidMetric player (StupidScore {..}) = myScore - othersMax
  where myScore = stupidScores M.! player
        othersMax = maximum $ map snd $ M.toList $ M.delete player stupidScores

stupidGameTree :: Int -> GameData -> GameTree () -> (StupidScore, GameTree StupidScore)
stupidGameTree 0 game gtree@(GameTree {..}) = (finalScore, finalNode)
  where finalNode = gtree { treeActions = [] }
        finalScore = StupidScore { stupidScores = M.singleton (statePlayer treeState) (playerScore game treeState) }
stupidGameTree n game gtree = (curScore, curNode)
  where player = statePlayer (treeState gtree)

        curActions = sortBy (comparing (Down . stupidMetric player . actionScore . fst)) $ map nextAction $ treeActions gtree
        curScore = case curActions of
                     ((action, _):_) -> actionScore action
                     _ -> StupidScore { stupidScores = M.singleton player (playerScore game (treeState gtree)) }

        curNode = gtree { treeActions = curActions }

        nextAction (action, nextTree) = (action { actionScore = newScore }, newTree)
          where (score, newTree) = stupidGameTree (n - 1) game nextTree
                newScore
                  | player `M.member` stupidScores score = score
                  | otherwise = score { stupidScores = M.insert player (playerScore game $ (treeState newTree) { statePlayer = player }) $ stupidScores score }
