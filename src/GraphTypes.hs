{-# LANGUAGE MultiParamTypeClasses #-}

module GraphTypes (
    Graph(..)
  ) where

import Data.List (minimumBy)
import Data.Ord (comparing)

import Util (fst')

class Eq v => Graph v e g where
  edgesFrom :: Integral n => g -> v -> [(v, n, e)]

  edgesBetween :: Integral n => g -> v -> v -> [(n,e)]
  edgesBetween g v v' =
    fmap (\(_,n,e) -> (n,e)) .
    filter ((==v') . fst') $
    edgesFrom g v

  minCostEdge :: Integral n => g -> v -> v -> Maybe (n,e)
  minCostEdge g v v' =
    case edges of
      [] -> Nothing
      _  -> Just $ minimumBy (comparing fst) edges
    where edges = edgesBetween g v v'
