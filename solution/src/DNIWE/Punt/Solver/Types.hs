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

type Mine = Node
type MineScores = Map Mine NodeScores
type Mines = Set Node

data Future = Future { mine :: Mine
                     , target :: Node
                     }
            deriving (Show, Eq)
type Futures = [Future]

data IndexedBoard = IndexedBoard { ibBoard :: Board
                                 , ibMines :: Mines
                                 }
                  deriving (Show)

data Game = Game { gameBoard :: Board
                 , gameMines :: Mines
                 , gameFutures :: Futures
                 , gameScoring :: MineScores
                 }
          deriving (Show)

data Action = Action { estimatedScore :: Double
                     , features :: [Double]
                     }
            deriving (Show)

data GameState = GameState { stupid :: Double
                           }
               deriving (Show)

data GameTree = GameTree { gameState :: GameState
                         , actions :: [(Action, GameTree)]
                         }
              deriving (Show)
