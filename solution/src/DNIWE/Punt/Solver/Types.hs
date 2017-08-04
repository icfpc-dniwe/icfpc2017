module DNIWE.Punt.Solver.Types where

import Data.Map.Strict (Map)
import Data.Set (Set)
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

type NodeScores = Map Node Int
type MineScores = Map Node NodeScores
type Mines = Set Node

data IndexedBoard = IndexedBoard { ibBoard :: Board
                                 , ibMines :: Mines
                                 }
                  deriving (Show)

data Game = Game { gameBoard :: Board
                 , gameMines :: Mines
                 , gameScoring :: MineScores
                 }
          deriving (Show)
