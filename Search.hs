{-# LANGUAGE ScopedTypeVariables #-}

module Search (
    graphBFS
  ) where

import qualified Set

import Util (fst')
import GraphTypes

graphBFS :: Graph v e g => g -> v -> (v -> Bool) -> Maybe [(v, n, e)]
graphBFS g s0 goal = graphBFS' (Seq.singleton (s0,[])) Set.empty
  where graphBFS' :: [(v,[(v, n, e)])] -> Set v -> Maybe [(v, n, e)]
        graphBFS' [] = Nothing
        graphBFS' ((node,path):fringe) closed
          | goal node = Just path
          | node `member` closed = graphBFS' fringe closed
          | otherwise = graphBFS' (fringe ++ neighbours) (node `insert` closed)
            where neighbours = fmap (\t -> (fst' t, t:path)) $ edgesFrom g node
