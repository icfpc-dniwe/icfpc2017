{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- roadmap
-- TODO: get scores from watcher
-- TODO: player with features
--

module Main where

import Data.Proxy (Proxy(..))
import qualified Data.Foldable as Foldable (toList)
import Data.Sequence (Seq, ViewL(..), viewl, (|>), dropWhileL)
import qualified Data.Sequence as Seq

import DNIWE.Punt.Solver.Types (PunterId)

import Common (PlayerWrapper(..), Move(..), Player(..), mkInitialBoard)
import Player.Watcher (Watcher, getScores)
import Player.Dummy   (Dummy)



simulate :: (Player p) => Int -> Watcher -> [p] -> IO (Watcher, [p])
simulate r watcher players = simulate' r (watcher, Seq.fromList players, Seq.empty)
  >>= \(watcher', players', _) -> return (watcher', Foldable.toList players') where
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

  ps <- mapM (\i -> PlayerWrapper <$> initialState i playersNum [] board (Proxy :: Proxy Dummy)) [1..playersNum]
  w  <- initialState 0 playersNum [] board (Proxy :: Proxy Watcher)

  putStrLn . show $ w
  (w', ps') <- simulate r w ps

  putStrLn . show $ w'
  putStrLn . unlines . map show $ getScores w' ps'
