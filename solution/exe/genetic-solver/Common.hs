{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Common where

import Control.Arrow (first)
import Data.Proxy (Proxy(..))
import Data.Graph.Inductive.Graph (Node, mkGraph, gmap)
import Data.Graph.Inductive.PatriciaTree (Gr)

import DNIWE.Punt.Solver.Types (Future(..), PunterId, Mines)
import GHC.Generics (Generic)

import qualified Data.Set as Set


emap' :: ((Node, Node) -> e1 -> e2) -> Gr v e1 -> Gr v e2
emap' f = gmap f' where
  f' (is, n1, nc, os) = (map fi is, n1, nc, map fo os) where
    fi (b, n0) = (f (n0, n1) b, n0)
    fo (b, n2) = (f (n1, n2) b, n2)


data Board = Board {
    boardMap   :: Gr () ()
  , boardMines :: Mines}
  deriving (Show, Eq, Generic)


mkInitialBoard :: [(Int, Bool)] -> [(Int, Int)] -> Board
mkInitialBoard sites rivers = Board {
    boardMap = mkGraph
      (map (\(a, _) -> (a, ())) sites)
      (map (\(a, b) -> (a, b, ())) rivers)
  , boardMines = Set.fromList (map fst . filter snd $ sites)}


data Move
  = Claim (Node, Node)
  | Pass
  deriving (Show, Eq)

isPass :: Move -> Bool
isPass Pass = True
isPass _    = False

fromClaim :: Move -> (Node, Node)
fromClaim (Claim x) = x
fromClaim _         = error "not supported"


relabelEdge' :: (Node, Node) -> (e -> e) -> Gr v e -> Gr v e
relabelEdge' (n1, n2) f = emap' f' where
  f' (n1', n2') e = if (n1 == n1' && n2 == n2') then (f e) else e


class Player p where
  initialState  :: PunterId -> Int -> [Future] -> Board -> Proxy p -> IO p
  updateState   :: [(PunterId, Move)] -> p -> IO p
  playerId      :: p -> PunterId
  playerFutures :: p -> [Future]
  makeMove      :: p -> IO (p, Move)
  feedback      :: p -> String
  feedback _   = ""

data PlayerWrapper = forall p. (Player p) => PlayerWrapper p

instance Player PlayerWrapper where
  initialState                     = error "Must be concrete player type"
  updateState ms (PlayerWrapper p) = PlayerWrapper <$> (updateState ms p)
  playerId       (PlayerWrapper p) = playerId p
  playerFutures  (PlayerWrapper p) = playerFutures p
  makeMove       (PlayerWrapper p) = (first PlayerWrapper) <$> (makeMove p)
  feedback       (PlayerWrapper p) = feedback p
