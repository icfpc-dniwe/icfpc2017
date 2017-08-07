module DNIWE.Punt.Solver.Stupid where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Sequence (Seq, (<|), ViewL(..))
import qualified Data.Sequence as SQ
import Data.Foldable
import Data.Ord
import Data.Graph.Inductive.Graph
import Data.Hashable
import Control.DeepSeq
import GHC.Generics (Generic)

import DNIWE.Punt.Solver.Types
import DNIWE.Punt.Solver.Game
import DNIWE.Punt.Solver.Score

newtype StupidScore = StupidScore { stupidScores :: IntMap Score }
                    deriving (Show, Generic)

instance NFData StupidScore where

initialScore :: GameData -> GameState -> StupidScore
initialScore game gstate = StupidScore { stupidScores = IM.singleton (statePlayer gstate) (playerScore game gstate) }

data StupidState = StupidState { minimumScores :: !(IntMap Score)
                               , canTrimScores :: !Bool
                               , visitedStates :: !IntSet
                               }
                 deriving (Show, Generic)

instance NFData StupidState where

type Metric = Int
  
stupidMetric :: PunterId -> StupidScore -> Metric
stupidMetric player (StupidScore {..}) = myScore - othersMax
  where myScore = stupidScores IM.! player
        othersMax = maximum $ map snd $ filter ((/= player) . fst) $ IM.toList stupidScores

stupidGameTree :: Int -> GameData -> GameState -> [GameMove]
stupidGameTree n game state = resMoves
  where (_, _, resEdges) = stupidGameTree' n startStupid game state
        resMoves = map (\e -> MoveClaim e) resEdges
        startStupid = StupidState { minimumScores = IM.empty
                                  , canTrimScores = False
                                  , visitedStates = IS.empty
                                  }

stupidGameTree' ::  Int -> StupidState -> GameData -> GameState -> (StupidState, StupidScore, [Edge])
stupidGameTree' 0 curStupid game gstate = (curStupid, finalScore, [])
  where finalScore = initialScore game gstate
stupidGameTree' n curStupid game gstate = (newStupid, curScore, curEdges)
  where (newStupid, results) = force $ stupidNextAction n game gstate curStupid $ freeEdges game gstate
        curActions = SQ.unstableSortBy (comparing (Down . (\(_, scoreVal, _) -> scoreVal)))  results
        curEdges = map (\(_, _, edge) -> edge) $ toList curActions
        curScore = case SQ.viewl curActions of
          (score, _, _) :< _ -> score
          _ -> initialScore game gstate

stupidNextAction :: Int -> GameData -> GameState -> StupidState -> [Edge] -> (StupidState, Seq (StupidScore, Metric, Edge))
stupidNextAction n game gstate startStupid = foldl' process (startStupid, SQ.empty)
  where process (oldStupid, oldEdges) edge
          | newHash `IS.member` visitedStates oldStupid = (oldStupid, oldEdges)
          | otherwise = (newStupid, (newScore, newScoreVal, edge) <| oldEdges)

          where player = statePlayer gstate

                newState' = performClaim player edge gstate
                newState = newState' { statePlayer = nextPlayer game player }
                newHash = hash newState

                (curStupid, score, _) = stupidGameTree' (n - 1) oldStupid game newState

                newScore
                  | player `IM.member` stupidScores score = score
                  | otherwise = score { stupidScores = IM.insert player (playerScore game newState') $ stupidScores score }

                newScoreVal = stupidMetric player newScore
                newVisited = IS.insert newHash $ visitedStates curStupid
                newMinimum = IM.insertWith max player newScoreVal $ minimumScores curStupid
                newCanTrim = canTrimScores curStupid || all (`IM.member` newMinimum) (gamePlayers game)
                newStupid = StupidState { minimumScores = newMinimum
                                        , canTrimScores = newCanTrim
                                        , visitedStates = newVisited
                                        }
  
{-# INLINE stupidNextAction #-}
