module DNIWE.Punt.Solver.Score
  ( boardScores
  , playerScore
  , edgesNearMines
  ) where

import qualified Data.Set as S
import qualified Data.IntMap.Strict as M
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.SP

import DNIWE.Punt.Solver.Types

defaultScoringFunction :: Int -> Int
defaultScoringFunction = (^ (2 :: Int))

futuresScoringFunction :: Int -> Int
futuresScoringFunction = (^ (3 :: Int))

boardScores :: StartingBoard -> MineScores
boardScores (StartingBoard {..}) = M.unionsWith M.union $ map scoreOne $ S.toList sbMines
  where scoreOne mine = M.singleton mine $ M.fromList $ map (head . unLPath) $ spTree mine weightedBoard
        weightedBoard = undir $ emap (const 1) sbBoard

playerScore :: GameData -> GameState -> Int
playerScore (GameData {..}) (GameState {..}) = totalDefault + futureDefault
  where curReachable = mineReachable statePlayer stateBoard

        totalDefault = sum $ concatMap (\m -> map (defaultScore m) $ curReachable m) $ S.toList $ sbMines gameStarting
        futureDefault = sum $ map (\ftr -> futureScore ftr) $ M.findWithDefault [] statePlayer gameFutures

        defaultScore mine n = defaultScoringFunction $ gameScoring M.! mine M.! n
        futureScore (Future mine target) = if target `elem` curReachable mine then ftrScore else -ftrScore
          where ftrScore = futuresScoringFunction $ gameScoring M.! mine M.! target

--

mineReachable :: PunterId -> Board -> Node -> [Node]
mineReachable player graph start = xdfsWith (map snd . filterTaken . lneighbors') node' [start] graph
  where filterTaken = filter ((== Just player) . edgeTaken . fst)

edgesNearMines :: Int -> StartingBoard -> NearestEdges
edgesNearMines depth (StartingBoard {..}) = foldr1 (M.union) $ map (\m -> M.singleton m $ S.fromList $ edgesNearMine m) $ S.toList sbMines
  where nodesNearMine m = map (\(n, _) -> n) $ filter (\(_, d) -> d <= depth) $ level m sbBoard
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