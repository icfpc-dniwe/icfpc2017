module DNIWE.Punt.Solver.Test where

import Data.List
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Control.Arrow (second, (***))
import Data.Function (on)
import Control.Monad (when)
import Data.List (sortBy)
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree (Gr)
import DNIWE.Punt.Solver.Utility ()
import Debug.Trace


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


ulevel :: Graph gr => Node -> gr a b -> [(Node, Int)]
ulevel m g = sortBy (compare `on` snd) $ IM.toList (step 1 [m] $ IM.singleton m 0) where
  step _ [] r = r
  step d ms r = step (d+1) ns r' where
    ns = IS.toList . IS.fromList $ filter (flip IM.notMember r) (concatMap nearNodes ms)
    r' = foldr (\n -> IM.insert n d) r ns

  nearNodes :: Node -> [Node]
  nearNodes n = maybe [] (\(ni, _, _, no) -> map snd $ ni ++ no) (fst $ G.match n g)


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
