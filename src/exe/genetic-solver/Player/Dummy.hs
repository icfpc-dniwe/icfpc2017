{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Player.Dummy where

import Data.Graph.Inductive.Graph (emap, labEdges, edgeLabel)
import Data.Graph.Inductive.PatriciaTree (Gr)
import GHC.Generics (Generic)

import DNIWE.Punt.Solver.Types (PunterId)

import Common (Board(..), Player(..), Move(..), isPass, fromClaim, relabelEdge')


-- | Player that takes first free edge every turn.
data Dummy = Dummy {
    dpMap      :: Gr () Bool
  , dpPlayerId :: PunterId}
  deriving (Show, Eq, Generic)


instance Player Dummy where

  initialState pid _ _ Board {..} _ = do
    return $ Dummy { dpMap = emap (const True) boardMap, dpPlayerId = pid }

  playerId Dummy {..} = dpPlayerId
  playerFutures _ = []

  updateState moves state = do
    let dpMap'
          = foldl (\board edge -> relabelEdge' edge (const False) board) (dpMap state)
          . map (fromClaim . snd)
          . filter (not . isPass . snd)
          $ moves
    return state { dpMap = dpMap' }


  makeMove state = do
    let edges = filter (edgeLabel) . labEdges . dpMap $ state
    if null edges
       then return (state, Pass)
       else do
         let (n1, n2, _) = head edges
         let dpMap' = relabelEdge' (n1, n2) (const False) . dpMap $ state
         return (state { dpMap = dpMap' }, Claim (n1, n2))
