module DNIWE.Punt.Interface.Utility where

import Data.Char
import Data.Graph.Inductive.Graph

dropPrefix :: String -> String
dropPrefix "" = ""
dropPrefix (c:t)
  | isUpper c = toLower c : t
  | otherwise = dropPrefix t

dropSuffix :: String -> String
dropSuffix = reverse . drop 1 . dropPrefix . reverse

snakeCase :: String -> String
snakeCase "" = ""
snakeCase (startC:startStr) = toLower startC : snakeize startStr
  where snakeize "" = ""
        snakeize (c:str)
          | isUpper c = '_' : toLower c : snakeize str
          | otherwise = c : snakeize str

sanitizeEdge :: Graph gr => gr a b -> Edge -> Edge
sanitizeEdge gr e@(a, b)
  | hasEdge gr e = e
  | hasEdge gr re = re
  | otherwise = error "sanitizeEdge: edge does not exist"
  where re = (b, a)
