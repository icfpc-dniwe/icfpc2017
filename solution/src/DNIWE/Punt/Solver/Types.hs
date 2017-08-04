module DNIWE.Punt.Solver.Types where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type SiteId = Int
type PunterId = Int

data Player = Us | Other !PunterId
            deriving (Show, Eq)

newtype NodeContext = NodeContext { isMine :: Bool }
                    deriving (Show, Eq)
newtype EdgeContext = EdgeContext { taken :: Maybe Player }
                    deriving (Show, Eq)

type Board = Gr NodeContext EdgeContext
