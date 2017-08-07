module DNIWE.Punt.Solver.Test where

import Data.List
import qualified Data.IntSet as IS
import Control.Arrow (second, (***))
import Control.Monad (when)
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import DNIWE.Punt.Solver.Utility hiding (edgesNearNode)
import Debug.Trace


-- edgesNearNode :: Graph gr => Int -> Node -> gr a b -> [Edge]
-- edgesNearNode d m gr = kokoko where
--   nearNodes = IS.fromList . map fst . takeWhile ((<= d) . snd) $ ulevel m gr
--
--   getEdges n = maybe ([], []) (\(ei, _, _, eo) -> (map snd ei, map snd eo)) . fst $ (G.match n gr)


edgesNearNode :: Graph gr => Int -> Node -> gr a b -> [Edge]
edgesNearNode depth m gr = concat [
    concatMap internalEdges (IS.toList internalNodes)
  , concatMap marginalEdges (IS.toList marginalNodes)] where

  internalEdges :: Node -> [Edge]
  internalEdges n = map (,n) . nearInNodes $ n

  marginalEdges :: Node -> [Edge]
  marginalEdges n = map (,n) . filter (flip IS.member internalNodes) . nearInNodes $ n

  nearInNodes :: Node -> [Node]
  nearInNodes n = maybe [] (\(ni, _, _, _) -> map snd ni) (fst $ G.match n gr)

  internalNodes, marginalNodes :: IS.IntSet
  (internalNodes, marginalNodes)
    = traceShowId $ ((IS.fromList . map fst) *** (IS.fromList . map fst))
    . second (takeWhile ((== depth) . snd))
    . break ((>= depth) . snd)
    $ ulevel m gr




{-
  filterNodes isMarginal
    = filter (\n -> (n `IS.member` internalNodes) || (not isMarginal && n `IS.member` marginNodes))

  getEdges n = map (,n) toNodes ++ map (n,) fromNodes where
    isMarginal = n `IS.member` marginNodes
    filterNodes' = filterNodes isMarginal . map snd

    (toNodes, fromNodes) = maybe
      ([], [])
      (\(ei, _, _, eo) -> (filterNodes' ei, filterNodes' eo))
      $ (fst $ G.match n gr)

--     (Just (filterNodes' -> toNodes, _, _, filterNodes' -> fromNodes), _) = G.match n gr

  getAllEdges = concatMap getEdges . IS.toList
-}


mkGraph' :: [Node] -> [Edge] -> Gr () ()
mkGraph' ns es = G.mkGraph (map (\x -> (x, ())) ns) (map (\(x, y) -> (x, y, ())) es)

norm :: [Edge] -> [Edge]
norm = map (\(a, b) -> (min a b, max a b))

gr1 :: Gr () ()
gr1 = mkGraph' [0..7] (norm [(0,1), (1,2), (2,3), (3,4), (4,5), (5,6), (6,7), (7,0), (1,7), (1,3), (3,5), (5,7)])

gr2 :: Gr () ()
gr2 = mkGraph' [0..20] (norm [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10),(10,11),(11,12),(12,13),(13,14),(14,15),(15,16),(16,17),(17,18),(18,19),(19,20),(20,0),(14,17),(17,2),(2,18),(18,6),(6,19),(19,10),(10,20),(20,14)])


mkTest :: (Show a, Eq a) => String -> a -> a -> IO ()
mkTest name x x' = when (x /= x') $ putStrLn . unlines $ ["Failed " ++ name ++ ":", show x, show x']


run :: IO ()
run = sequence_ [
    mkTest "" (0::Int) (0::Int)

  , mkTest "ulevel" (ulevel 4 gr1) []

  , mkTest "gr1 0 n0" (sort $ edgesNearNode 0 0 gr1) (norm . sort $ [])
  , mkTest "gr1 0 n1" (sort $ edgesNearNode 1 0 gr1) (norm . sort $ [(0,1), (0,7)])
  , mkTest "gr1 0 n2" (sort $ edgesNearNode 2 0 gr1) (norm . sort $ [(0,1), (1,2), (1,3), (1,7), (0,7), (7,6), (7,5)])

  , mkTest "gr1 4 n0" (sort $ edgesNearNode 0 4 gr1) (norm . sort $ [])
  , mkTest "gr1 4 n1" (sort $ edgesNearNode 1 4 gr1) (norm . sort $ [(4,3), (4,5)])
  , mkTest "gr1 4 n2" (sort $ edgesNearNode 2 4 gr1) (norm . sort $ [(4,3), (4,5), (5,6), (5,7), (5,3), (3,2), (3,1)])
  ]
