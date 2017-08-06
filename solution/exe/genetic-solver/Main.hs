{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Default (Default, def)
import Data.Graph.Inductive.Graph (LEdge, Edge, Node, mkGraph, gmap, emap, toEdge, toLEdge, labEdges, edgeLabel)
import Data.Graph.Inductive.PatriciaTree (Gr)

import DNIWE.Punt.Solver.Types (StartingBoard(..), Future(..), PunterId, MineScores)
import DNIWE.Punt.Solver.Game ()
import DNIWE.Punt.Solver.Score (boardScores)
import GHC.Generics (Generic)

import Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as Vector

import qualified Data.Set as Set

import DNIWE.Punt.Solver.Game (relabelEdge)

-- roadmap
-- TODO: get scores
-- TODO: typeclass for players
-- TODO: launch different players against each other


emap' :: ((Node, Node) -> e1 -> e2) -> Gr v e1 -> Gr v e2 
emap' f = gmap f' where
  f' (is, n1, nc, os) = (map fi is, n1, nc, map fo os) where
    fi (b, n0) = (f (n0, n1) b, n0)
    fo (b, n2) = (f (n1, n2) b, n2)


mkStartingBoard :: [(Int, Bool)] -> [(Int, Int)] -> StartingBoard  
mkStartingBoard sites rivers = StartingBoard {
    sbBoard = mkGraph
      (map (\(a, _) -> (a, ())) sites)
      (map (\(a, b) -> (a, b, ())) rivers)
  , sbMines = Set.fromList (map fst . filter snd $ sites)}


data GameData = GameData {
     gdStartingBoard :: StartingBoard
   , gdScoring       :: MineScores
   , gdFutures       :: [Future]
   , gdPlayerId      :: PunterId
   , gdPlayersNum    :: PunterId}
  deriving (Show, Eq, Generic)


mkGameData :: StartingBoard -> [Future] -> PunterId -> Int -> GameData
mkGameData startingBoard futures playerId playersNum = GameData {
    gdStartingBoard = startingBoard
  , gdScoring       = boardScores startingBoard
  , gdFutures       = futures
  , gdPlayerId      = playerId
  , gdPlayersNum    = playersNum}

                       

data EdgeContext = EdgeContext {
    ecFree     :: Bool
  , ecFeatures :: Vector Double
  , ecResult   :: Double}
  deriving (Show, Eq, Ord, Generic)

instance Default EdgeContext where
  def = EdgeContext {
      ecFree = True
    , ecFeatures = Vector.replicate featuresNum 0
    , ecResult = 0.0}


updateFeatures
  :: (Int, Int)
  -> EdgeContext
  -> EdgeContext
updateFeatures (n1, n2) e = e { ecFeatures = ecFeatures' } where
  ecFeatures' = (ecFeatures e) // [
      (0, if ecFree e then 1.0 else 0.0)
    , (1, 0.5 * (fromIntegral $ n1 + n2))]

  
featuresNum :: Int
featuresNum = 16

           
data GameState = GameState {gsBoard  :: Gr () EdgeContext}
  deriving (Show, Eq, Generic)
  
mkInitialState :: GameData -> GameState
mkInitialState (GameData {..}) = GameState {
  gsBoard  = emap' updateFeatures . emap (const def) . sbBoard $ gdStartingBoard } where

      
applyMove :: PunterId -> LEdge EdgeContext -> GameState -> GameState
applyMove p (n1, n2, e) state = state {
  gsBoard = relabelEdge (n1, n2, e {ecFree = False}) $ gsBoard state }


  

main :: IO ()
main = do
  let startingBoard = mkStartingBoard
        [(1, True), (2, False), (3, False), (4, False)]
        [(1,2), (1,3), (1,4)]


  let playersNum = 3

  let gameData  = (\i -> mkGameData startingBoard [] i playersNum) $ 1
  let gameState = mkInitialState gameData
  
  putStrLn . show $ gameData 
  putStrLn . show $ gameState

  let edge = head . filter (ecFree . edgeLabel) . labEdges . gsBoard $ gameState
  let gameState' = applyMove 1 edge gameState
  putStrLn . show $ gameState'


  return ()
