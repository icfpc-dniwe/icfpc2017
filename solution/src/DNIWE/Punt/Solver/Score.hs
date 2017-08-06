module DNIWE.Punt.Solver.Score
  ( boardScores
  , playerScore
  ) where

import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Graph.Inductive.Query.BFS as BFS

import DNIWE.Punt.Solver.Types

defaultScoringFunction :: Int -> Score
defaultScoringFunction = (^ (2 :: Int))

futuresScoringFunction :: Int -> Score
futuresScoringFunction = (^ (3 :: Int))

boardScores :: StartingBoard -> MineScores
boardScores (StartingBoard {..}) = IM.unionsWith IM.union $ map scoreOne $ S.toList sbMines
  where scoreOne mine = IM.singleton mine $ IM.fromList $ BFS.level mine $ undir sbBoard

playerScore :: GameData -> GameState -> Score
playerScore (GameData {..}) state@(GameState {..}) = totalDefault + futureDefault
  where curReachable = mineReachable (sbBoard gameStarting) state

        totalDefault = sum $ concatMap (\m -> map (defaultScore m) $ curReachable m) $ S.toList $ sbMines gameStarting
        futureDefault = sum $ map (\ftr -> futureScore ftr) $ IM.findWithDefault [] statePlayer gameFutures

        defaultScore mine n = defaultScoringFunction $ gameScoring IM.! mine IM.! n
        futureScore (Future mine target) = if target `elem` curReachable mine then ftrScore else -ftrScore
          where ftrScore = futuresScoringFunction $ gameScoring IM.! mine IM.! target

mineReachable :: Board -> GameState -> Node -> [Node]
mineReachable board (GameState {..}) start = DFS.xdfsWith filterContext node' [start] board
  where filterTaken = filter (\edge -> maybe False (== statePlayer) $ M.lookup edge stateTaken)
        filterContext ctx = map snd (filterTaken $ map (node' ctx, ) $ pre' ctx) ++ map fst (filterTaken $ map (, node' ctx) $ suc' ctx)
