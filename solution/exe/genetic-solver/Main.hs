{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- roadmap
-- TODO: remove bool from boardMap
-- TODO: watcher should know who took an edge
-- TODO: get scores from watcher
-- TODO: player with features
--

module Main where

import Data.Proxy (Proxy(..))
import qualified Data.Foldable as Foldable (toList)
import Data.Sequence (Seq(..), ViewL(..), viewl, (|>), dropWhileL)
import qualified Data.Sequence as Seq
import Data.Default (Default, def)
import Data.Graph.Inductive.Graph (LEdge, Edge, Node, mkGraph, gmap, emap, toEdge, toLEdge, labEdges, edgeLabel)
import Data.Graph.Inductive.PatriciaTree (Gr)

import DNIWE.Punt.Solver.Types (StartingBoard(..), Future(..), PunterId, MineScores)
import DNIWE.Punt.Solver.Game ()
import DNIWE.Punt.Solver.Score (boardScores)
import GHC.Generics (Generic)

import Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as Vector

import Data.Set (Set)
import qualified Data.Set as Set

import DNIWE.Punt.Solver.Game (relabelEdge)

import Common (
  Board(..), mkInitialBoard,
  Move(..), isPass, fromClaim, relabelEdge',
  Player(..),
  emap')



-- | Player that only monitors game state.
data Watcher = Watcher {watcherBoard :: Board}
  deriving (Show, Eq, Generic)

instance Player Watcher where

  initialState playerId playersNum board@(Board {..}) _ = do
    return $ Watcher { watcherBoard = board }

  updateState moves state = do
    let boardMap'
          = foldl (\board edge -> relabelEdge' edge (const False) board) (boardMap . watcherBoard $ state)
          . map (fromClaim . snd)
          . filter (not . isPass . snd)
          $ moves
    return state { watcherBoard = (watcherBoard state) { boardMap = boardMap' }}

  playerId _ = error "Watcher doesn't not participate in a game"
  makeMove _ = error "Watcher doesn't not participate in a game"



-- | Player that takes first free edge every turn.
data DummyPlayer = DummyPlayer {
    dpBoard    :: Board
  , dpPlayerId :: PunterId}
  deriving (Show, Eq, Generic)


instance Player DummyPlayer where

  initialState playerId playersNum board@(Board {..}) _ = do
    return $ DummyPlayer { dpBoard = board, dpPlayerId = playerId }

  playerId DummyPlayer {..} = dpPlayerId

  updateState moves state = do
    let boardMap'
          = foldl (\board edge -> relabelEdge' edge (const False) board) (boardMap . dpBoard $ state)
          . map (fromClaim . snd)
          . filter (not . isPass . snd)
          $ moves
    return state { dpBoard = (dpBoard state) { boardMap = boardMap' }}


  makeMove state = do
    let edges = filter (edgeLabel) . labEdges . boardMap . dpBoard $ state
    if null edges
       then return (state, Pass)
       else do
         let (n1, n2, _) = head edges
         let boardMap' = relabelEdge' (n1, n2) (const False) . boardMap . dpBoard $ state
         return (state { dpBoard = (dpBoard state) { boardMap = boardMap' }}, Claim (n1, n2))



simulate :: (Player p) => Int -> Watcher -> [p] -> IO (Watcher, [p])
simulate r w ps = simulate' r (w, Seq.fromList ps, Seq.empty)
  >>= \(w, ps, _) -> return (w, Foldable.toList ps) where
    simulate'
      :: (Player p)
      => Int
      ->    (Watcher, Seq p, Seq (PunterId, Move))
      -> IO (Watcher, Seq p, Seq (PunterId, Move))
    simulate' 0 (w, ps_, ms)  = return (w, ps_, ms)
    simulate' n (w, ps_, ms) = do

      let (p, ps) = seqHeadTail ps_
      let pid = playerId p
      let ms' = dropWhileL ((== pid) . fst) ms

      (p', m) <- updateState (Foldable.toList ms') p >>= makeMove
      putStrLn . concat $ ["player ", show pid, ": ", show m]

      w' <- updateState [(pid, m)] w

      simulate' (n-1) (w', (ps |> p'), (ms' |> (pid, m)))

    seqHeadTail :: Seq a -> (a, Seq a)
    seqHeadTail s = case viewl s of
      EmptyL    -> error "seqHeadTail called on empty sequence"
      (x :< xs) -> (x, xs)





main :: IO ()
main = do

  let board = mkInitialBoard
        [(1, True), (2, False), (3, False), (4, False)]
        [(1,2), (1,3), (1,4)]

  let r = 4
  let playersNum = 3

  ps <- mapM (\i -> initialState i playersNum board (Proxy :: Proxy DummyPlayer)) [1..playersNum]
  w  <- initialState 0 playersNum board (Proxy :: Proxy Watcher)

  putStrLn . show $ w

  (w', ps') <- simulate r w ps

  putStrLn . show $ w'
