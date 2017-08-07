module DNIWE.Punt.Solver.Score where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Basic
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Graph.Inductive.Query.BFS as BFS
import Control.Monad.Random.Class
import qualified Data.Vector as V

import DNIWE.Punt.Solver.Utility
import DNIWE.Punt.Solver.Types

defaultScoringFunction :: Int -> Score
defaultScoringFunction = (^ (2 :: Int))

futuresScoringFunction :: Int -> Score
futuresScoringFunction = (^ (3 :: Int))

boardScores :: StartingBoard -> MineScores
boardScores (StartingBoard {..}) = IM.unionsWith IM.union $ map scoreOne $ IS.toList sbMines
  where scoreOne mine = IM.singleton mine $ IM.fromList $ BFS.level mine $ undir sbBoard

playerScore :: GameData -> GameState -> Score
playerScore (GameData {..}) (GameState {..}) =
  playerScore' gameStarting gameScoring stateTaken (IM.findWithDefault [] statePlayer gameFutures) statePlayer

playerScore' :: StartingBoard -> MineScores -> TakenMap -> [Future] -> PunterId -> Score
playerScore' (StartingBoard {..}) scoring taken futures playerId = totalDefault + futureDefault
  where curReachable = mineReachable sbBoard taken playerId

        totalDefault = sum $ concatMap (\m -> map (defaultScore m) $ curReachable m) $ IS.toList sbMines
        futureDefault = sum $ map (\ftr -> futureScore ftr) futures

        defaultScore mine n = defaultScoringFunction $ scoring IM.! mine IM.! n
        futureScore (Future mine target) = if target `elem` curReachable mine then ftrScore else -ftrScore
          where ftrScore = futuresScoringFunction $ scoring IM.! mine IM.! target

mineReachable :: Board -> TakenMap -> PunterId -> Node -> [Node]
mineReachable board taken playerId start = DFS.xdfsWith (filterContext isTaken) node' [start] board
  where isTaken edge = maybe False (== playerId) $ M.lookup edge taken

probableTotalMaxScore :: MonadRandom m => StartingBoard -> MineScores -> Int -> Int -> m Score
probableTotalMaxScore board scores playersNum experimentsNum = do
  let allEdges = V.fromList $ map toEdge $ labEdges $ sbBoard board
      takeNumber = V.length allEdges `div` playersNum

      performExperiment = do
        taken <- M.fromList <$> map (, 0) <$> V.toList <$> randomSample takeNumber allEdges
        return $ playerScore' board scores taken [] 0

  experiments <- mapM (const performExperiment) [1..experimentsNum]
  return $ maximum experiments * playersNum

updateClusters :: GameData -> GameState -> Edge -> (Score, ClusterMap)
updateClusters (GameData {..}) (GameState {..}) (a, b) = (newScore, foldr insertNew stateClusters $ IS.toList $ clusterNodes newCluster)
  where insertNew n = IM.insertWith IM.union n (IM.singleton statePlayer newCluster)
        lookupCluster n = IM.lookup n stateClusters >>= IM.lookup statePlayer
        checkMine n = n `IS.member` sbMines gameStarting
        newMineScore n
          | checkMine n = 1
          | otherwise = 0
        newMine n
          | checkMine n = IS.singleton n
          | otherwise = IS.empty
        oneMineScore n m = defaultScoringFunction (gameScoring IM.! m IM.! n)
        manyMineScore addedNodes m = sum $ map (\n -> oneMineScore n m) $ IS.toList addedNodes
        addMineScore addedNodes n
          | checkMine n = manyMineScore addedNodes n
          | otherwise = 0
        addOneNode (PlayerCluster {..}) n = addMineScore clusterNodes n + sum (map (oneMineScore n) $ IS.toList clusterMines)
        mergeMines addedNodes mines = sum $ map (manyMineScore addedNodes) $ IS.toList mines

        (newScore, newCluster) = case (lookupCluster a, lookupCluster b) of
          (Just aCluster, Just bCluster) ->
            let cluster = PlayerCluster { clusterMines = clusterMines aCluster `IS.union` clusterMines bCluster
                                        , clusterNodes = clusterNodes aCluster `IS.union` clusterNodes bCluster
                                        }
                score = mergeMines (clusterNodes aCluster) (clusterMines bCluster) + mergeMines (clusterNodes bCluster) (clusterMines aCluster)
            in (score, cluster)
          (Nothing, Just bCluster) ->
            let cluster = PlayerCluster { clusterMines = clusterMines bCluster `IS.union` newMine a
                                        , clusterNodes = IS.insert a $ clusterNodes bCluster
                                        }
                score = addOneNode bCluster a
            in (score, cluster)
          (Just aCluster, Nothing) ->
            let cluster = PlayerCluster { clusterMines = clusterMines aCluster `IS.union` newMine b
                                        , clusterNodes = IS.insert b $ clusterNodes aCluster
                                        }
                score = addOneNode aCluster b
            in (score, cluster)
          (Nothing, Nothing) ->
            let cluster = PlayerCluster { clusterMines = newMine a `IS.union` newMine b
                                        , clusterNodes = IS.fromList [a, b]
                                        }
                score = newMineScore a + newMineScore b
            in (score, cluster)

--depth (GameData {..}) (GameState {..}) = foldr1 (M.union) $ map (\p -> M.singleton p $ S.fromList $ edgesNearPunter p) [0..gamePlayersN]
--  where claimedEdges p = filter (\(EdgeContext {..}) -> case edgeTaken of
--  	                                                      Just pId -> pId == p
--                                                          Nothing -> False
--                                ) edges stateBoard
----        claimedNodes = S.fromList $ map (\(s, t) -> S.fromList [s, t])
--  	    nodesNearNode node = map (\(n, _) -> n) $ filter (\((EdgeContext {..}), d) -> d <= depth && isNothing edgeTaken) $ level node stateBoard
--        edgesNearNode node = foldr1 (++) $ map (\n -> filter (\(s,t) -> s == n || t == n) $ edges stateBoard) $ nodesNearNode node
--        edgesNearPunter p = map (\e -> ) map (\n -> edgesNearNode n) 
