module DNIWE.Punt.Solver.Utility where

import Data.List
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Control.Arrow (second, (***))
import Data.Function (on)
import qualified Data.Vector as V
import Control.Monad.Random.Class
import Data.Graph.Inductive.Graph as G

relabelEdge :: DynGraph gr => LEdge b -> gr a b -> gr a b
relabelEdge e@(a, b, _) g = insEdge e $ delEdge (a, b) g

filterContext :: (Edge -> Bool) -> Context a b -> [Node]
filterContext func (map snd -> toNodes, n, _, map snd -> fromNodes) =
  map fst (filter func $ map (, n) toNodes) ++ map snd (filter func $ map (n, ) fromNodes)

randomSample :: MonadRandom m => Int -> V.Vector a -> m (V.Vector a)
randomSample n vec = do
  let len = V.length vec
  sampledIds <- mapM (getRandomR . (0, )) [len - 1, len - 2 .. len - n - 1]

  let fixupId newId curId
        | newId < curId = newId
        | otherwise = newId + 1

      goFixupIds (oldIds, nonfixedIds) curId = (newId : oldIds, curId : nonfixedIds)
        where newId = foldl' fixupId curId nonfixedIds
      fixupIds ids = fst $ foldl' goFixupIds ([], []) ids
      
      fixedIds = IS.fromList $ fixupIds sampledIds

  return $ V.ifilter (\i _ -> i `IS.member` fixedIds) vec

-- ulevel :: Graph gr => Node -> gr a b -> [(Node, Int)]
-- ulevel v = uleveln [(v,0)]
-- 
-- usuci :: Context a b -> Int -> [(Node, Int)]
-- usuci c i = zip (neighbors' c) (repeat i)
-- 
-- uleveln :: Graph gr => [(Node, Int)] -> gr a b -> [(Node, Int)]
-- uleveln []         _             = []
-- uleveln _          g | isEmpty g = []
-- uleveln ((v,j):vs) g = case match v g of
--                          (Just c,g')  -> (v,j):uleveln (vs++usuci c (j+1)) g'
--                          (Nothing,g') -> uleveln vs g'
-- 
-- edgesNearNode :: Graph gr => Int -> Node -> gr a b -> [Edge]
-- edgesNearNode depth m gr = concatMap getEdges $ IS.toList nearNodes
--   where nearNodes = IS.fromList $ map (\(n, _) -> n) $ takeWhile (\(_, d) -> d <= depth) $ ulevel m gr
--         filterNodes = filter (`IS.member` nearNodes) . map snd
--         getEdges n = map (, n) toNodes ++ map (n, ) fromNodes
--           where (Just (filterNodes -> toNodes, _, _, filterNodes -> fromNodes), _) = G.match n gr

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
    = ((IS.fromList . map fst) *** (IS.fromList . map fst))
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
