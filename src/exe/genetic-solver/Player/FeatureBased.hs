{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Player.FeatureBased where

import Data.Maybe (isNothing, catMaybes)
import Control.Arrow ((***))
import Data.List (maximumBy)
import Data.Default (Default, def)
import Data.Function (on)
import Data.Graph.Inductive.Graph (Node, nmap, emap, labEdges, edgeLabel, toEdge, labNodes, gmap, mkGraph, nodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import GHC.Generics (Generic)

import DNIWE.Punt.Solver.Types (PunterId, Mines, Future)

import Common (Board(..), Player(..), Move(..), isPass, fromClaim, relabelEdge', relabelNode', emap')

import Control.Arrow (second)

import Data.Vector.Unboxed (Vector, (//))
import qualified Data.Vector.Unboxed as Vector

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Set (Set)
import qualified Data.Set as Set


data StaticFeatureContext = StaticFeatureContext {
    sfcState  :: FeatureBased
  , sfcEdge   :: (Node, Node)}
  deriving (Show, Eq)

data DynamicFeatureContext = DynamicFeatureContext {
    dfcEdgeContext :: EdgeContext}
  deriving (Show, Eq)

data Feature
  = StaticFeature  { sfCompute :: StaticFeatureContext  -> Double }
  | DynamicFeature { dfCompute :: (StaticFeatureContext, DynamicFeatureContext) -> Double }


featureSet :: [Feature]
featureSet = [
    DynamicFeature $ maybe 1 (const 0) . ecOwner . dfcEdgeContext . snd
  , StaticFeature  $ (0.5 *) . fromIntegral . uncurry (+) . sfcEdge
  ]


initFeatures :: StaticFeatureContext -> Vector Double
initFeatures ctx = Vector.fromList $ map compute featureSet where
  compute (StaticFeature f) = f ctx
  compute _                 = 0.0


updateFeatures :: (StaticFeatureContext, DynamicFeatureContext) -> Vector Double -> Vector Double
updateFeatures ctx v = v // updates where
  updates = catMaybes . map compute $ zip [0..] featureSet

  compute (n, DynamicFeature f) = Just $ (n, f ctx)
  compute _                     = Nothing


data EdgeContext = EdgeContext {
    ecOwner    :: Maybe PunterId
  , ecResult   :: Double
  , ecFeatures :: Vector Double}
  deriving (Show, Eq, Ord)

instance Default EdgeContext where
  def = EdgeContext {
    ecOwner    = Nothing
  , ecFeatures = Vector.empty
  , ecResult   = 0.0}


type GameMap = Gr Bool EdgeContext

data FeatureBased = FeatureBased {
    fbMap        :: GameMap
  , fbMines      :: Mines
  , fbPlayerId   :: PunterId
  , fbPlayersNum :: PunterId
  , fbFutures    :: [Future]}
  deriving (Show, Eq, Generic)



applyMove :: (PunterId, (Node, Node)) -> GameMap -> GameMap
applyMove (pid, edge@(n1, n2))
  = relabelNode' n1 (const True)
  . relabelNode' n2 (const True)
  . relabelEdge' edge (\e -> e { ecOwner = Just pid })


updateMap :: FeatureBased -> GameMap -> GameMap
updateMap state g = mkGraph (map (,False) ns) (map updateEdge es) where
  ns :: [Node]
  ns = nodes g

  updated :: Set Node
  updated = Set.fromList $ map fst . filter snd . labNodes $ g

  es :: [(Node, Node, EdgeContext)]
  es =  labEdges $ g

  reorder :: (Node, Node, EdgeContext) -> ((Node, Node), EdgeContext)
  reorder (n1, n2, ec) = ((n1, n2), ec)

  es' :: HashMap (Int,Int) EdgeContext
  es' = HashMap.fromList . map reorder $ es

  updateEdge :: (Node, Node, EdgeContext) -> (Node, Node, EdgeContext)
  updateEdge e@(n1, n2, ec) = if Set.member n1 updated || Set.member n2 updated
    then let
      staticCtx = StaticFeatureContext { sfcState  = state, sfcEdge = (n1, n2) }
      dynamicCtx = DynamicFeatureContext { dfcEdgeContext = ec }
      in (n1, n2, ec { ecFeatures = updateFeatures (staticCtx, dynamicCtx) (ecFeatures ec) })
    else e



instance Player FeatureBased where

  initialState fbPlayerId fbPlayersNum fbFutures Board {..} _ = do
    let fbMap = nmap (const False) . emap def $ boardMap
    let fbMines = boardMines
    let sfcState = FeatureBased {..}

    return $ sfcState {
        fbMap = emap' (\sfcEdge ec -> ec { ecFeatures = initFeatures StaticFeatureContext {..} }) fbMap
      }

  playerId FeatureBased {..} = fbPlayerId

  playerFutures FeatureBased {..} = fbFutures

  updateState moves state = return $ state {
    fbMap = updateMap state
          . foldr applyMove (fbMap state)
          . map (second fromClaim)
          . filter (not . isPass . snd)
          $ moves}

  makeMove state = do
    let edges = filter (isNothing . ecOwner . edgeLabel) . labEdges $ fbMap state
    if null edges
      then return (state, Pass)
      else do
        let (n1, n2, _) = maximumBy (compare `on` (ecResult . edgeLabel)) edges
        return (
            state { fbMap = updateMap state . applyMove (fbPlayerId state, (n1, n2)) $ fbMap state }
          , Claim (n1, n2))
