{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Player.Watcher where

import Control.Arrow (second)
import Data.Graph.Inductive.Graph (emap)
import Data.Graph.Inductive.PatriciaTree (Gr)
import GHC.Generics (Generic)

import DNIWE.Punt.Solver.Types (PunterId)

import Common (Board(..), Player(..), isPass, fromClaim, relabelEdge')


import DNIWE.Punt.Solver.Score (defaultScoringFunction, futuresScoringFunction)
import Data.Graph.Inductive.Graph (Node, unLPath, lneighbors', node')
import Data.Graph.Inductive.Basic (undir)
import Data.Graph.Inductive.Query.DFS (xdfsWith)
import Data.Graph.Inductive.Query.SP (spTree)
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as M
import DNIWE.Punt.Solver.Types (MineScores, Mines, Future(..))


type GameMap = Gr () (Maybe PunterId)

-- | Player that only monitors game state.
data Watcher = Watcher {
    watcherMap        :: GameMap
  , watcherMines      :: Mines
  , watcherPlayersNum :: Int}
  deriving (Show, Eq, Generic)

instance Player Watcher where

  initialState _ playersNum _ Board {..} _ = do
    return $ Watcher {
        watcherMap        = emap (const Nothing) boardMap
      , watcherMines      = boardMines
      , watcherPlayersNum = playersNum}

  updateState moves state = do
    let watcherMap'
          = foldl (\board (pid, edge) -> relabelEdge' edge (updateEdge pid) board) (watcherMap $ state)
          . map (second fromClaim)
          . filter (not . isPass . snd)
          $ moves where
          updateEdge pid = maybe
            (Just pid)
            (\pid' -> error $ "Already claimed by " ++ (show pid'))

    return state { watcherMap = watcherMap' }

  playerId      _ = error "Watcher doesn't not participate in a game"
  playerFutures _ = error "Watcher doesn't not participate in a game"
  makeMove      _ = error "Watcher doesn't not participate in a game"



mineScores :: GameMap -> Mines -> MineScores
mineScores gameMap mines = M.unionsWith M.union $ map scoreOne $ Set.toList mines where
  scoreOne mine = M.singleton mine $ M.fromList $ map (head . unLPath) $ spTree mine weightedBoard
  weightedBoard = undir $ emap (const 1) gameMap

mineReachable :: GameMap -> PunterId -> Node -> [Node]
mineReachable gameMap player start = xdfsWith (map snd . filterTaken . lneighbors') node' [start] gameMap
  where filterTaken = filter ((== Just player) . fst)

playerScore :: GameMap -> Mines -> MineScores -> PunterId -> [Future] -> Int
playerScore gameMap mines scoring pid futures = totalDefault + futureDefault where
  curReachable = mineReachable gameMap pid

  totalDefault = sum $ concatMap (\m -> map (defaultScore m) $ curReachable m) (Set.toList mines)
  futureDefault = sum $ map (\ftr -> futureScore ftr) $ futures

  defaultScore mine n = defaultScoringFunction $ scoring M.! mine M.! n
  futureScore (Future mine target) = if target `elem` curReachable mine then ftrScore else -ftrScore
    where ftrScore = futuresScoringFunction $ scoring M.! mine M.! target

getScores :: (Player p) => Watcher -> [p] -> [(PunterId, Int, String)]
getScores Watcher {..} ps = map mkPlayerInfo ps where
  scoring = mineScores watcherMap watcherMines

  mkPlayerInfo p = (
      playerId p
    , playerScore watcherMap watcherMines scoring (playerId p) (playerFutures p)
    , feedback p)
