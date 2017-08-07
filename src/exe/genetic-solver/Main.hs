{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module Main where
{-
import Control.Monad (liftM2)
import Data.Proxy (Proxy(..))
import qualified Data.Foldable as Foldable (toList)
import Data.Sequence (Seq, ViewL(..), viewl, (|>), dropWhileL)
import qualified Data.Sequence as Seq
import Data.Graph.Inductive.Graph (size)

import DNIWE.Punt.Solver.Types (PunterId)

import Common (Board(..), PlayerWrapper(..), Move(..), Player(..), mkInitialBoard)
import Player.Watcher      (Watcher, getScores)
import Player.Dummy        (Dummy)
import Player.FeatureBased (FeatureBased)



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


runGame :: (Player p) => Board -> Proxy p -> Int -> IO [(PunterId, Int, String)]
runGame board p1 playersNum = do

  let r = size $ boardMap board
  w  <- initialState 0 playersNum [] board (Proxy :: Proxy Watcher)
  ps <- liftM2 (:)
          (PlayerWrapper <$> initialState 1 playersNum [] board p1)
          (mapM (\i -> PlayerWrapper <$> initialState i playersNum [] board (Proxy :: Proxy Dummy)) [2..playersNum])

  (w', ps') <- simulate r w ps
  return $ getScores w' ps'


main :: IO ()
main = do

  let board = mkInitialBoard
        [(1, True), (2, False), (3, False), (4, False)]
        [(1,2), (1,3), (1,4)]

  runGame board (Proxy :: Proxy FeatureBased) 3 >>= putStrLn . unlines . map show
-}

main :: IO ()
main = return ()
