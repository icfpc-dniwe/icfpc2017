module DNIWE.Punt.Solver.Score
  ( boardScores
  , playerScore
  , edgesNearMines
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

edgesNearMines :: Int -> StartingBoard -> NearestEdges
edgesNearMines depth (StartingBoard {..}) = foldr1 (IM.union) $ map (\m -> IM.singleton m $ S.fromList $ edgesNearMine m) $ S.toList sbMines
  where nodesNearMine m = map (\(n, _) -> n) $ filter (\(_, d) -> d <= depth) $ BFS.level m sbBoard
        edgesNearMine m = foldr1 (++) $ map (\n -> filter (\(s,t) -> s == n || t == n) $ edges sbBoard) $ nodesNearMine m

--edgesNearPunters :: Int -> GameData -> GameState -> NearestEdges
--depth (GameData {..}) (GameState {..}) = foldr1 (M.union) $ map (\p -> M.singleton p $ S.fromList $ edgesNearPunter p) [0..gamePlayersN]
--  where claimedEdges p = filter (\(EdgeContext {..}) -> case edgeTaken of
--  	                                                      Just pId -> pId == p
--                                                          Nothing -> False
--                                ) edges stateBoard
----        claimedNodes = S.fromList $ map (\(s, t) -> S.fromList [s, t])
--  	    nodesNearNode node = map (\(n, _) -> n) $ filter (\((EdgeContext {..}), d) -> d <= depth && isNothing edgeTaken) $ level node stateBoard
--        edgesNearNode node = foldr1 (++) $ map (\n -> filter (\(s,t) -> s == n || t == n) $ edges stateBoard) $ nodesNearNode node
--        edgesNearPunter p = map (\e -> ) map (\n -> edgesNearNode n) 