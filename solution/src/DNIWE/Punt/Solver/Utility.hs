module DNIWE.Punt.Solver.Utility where

import Data.Maybe
import Control.Monad
import qualified Data.Set as S
import Data.Tree (Tree)
import Control.Monad.Random.Class
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Query.DFS as DFS

relabelEdge :: DynGraph gr => LEdge b -> gr a b -> gr a b
relabelEdge e@(a, b, _) g = insEdge e $ delEdge (a, b) g

filterContext :: (Edge -> Bool) -> Context a b -> [Node]
filterContext func (map snd -> toNodes, n, _, map snd -> fromNodes) =
  map fst (filter func $ map (, n) toNodes) ++ map snd (filter func $ map (n, ) fromNodes)

randomVersatile :: Graph gr => MonadRandom m => Float -> [Node] -> gr a b -> m [Tree Node]
randomVersatile percentage starts graph = do
  let maybeDrop x = do
        chance <- getRandom
        if chance <= percentage
          then return $ Just x
          else return Nothing
  dropped <- liftM (S.fromList . catMaybes) $ mapM (maybeDrop . toEdge) $ labEdges graph

  let isDropped = (not . (`S.member` dropped))

  return $ DFS.xdffWith (filterContext isDropped) node' starts graph
