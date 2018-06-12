{-# LANGUAGE ScopedTypeVariables #-}

module Search (
    graphDFS
  , graphBFS
  ) where

import Data.Foldable (msum)

import qualified Data.Set as Set
import Data.Set (Set, member)

import Util (fst')
import GraphTypes

graphDFS ::
  forall v e g n. (Ord v, Integral n, Graph v e g) =>
  g -> v -> (v -> Bool) -> Maybe [(v, n, e)]
graphDFS g s0 goal = graphDFS' s0 Set.empty
  where graphDFS' :: v -> Set v -> Maybe [(v, n, e)]
        graphDFS' node closed
          | goal node = Just []
          | node `member` closed = Nothing
          | otherwise = msum $ fmap recurse neighbours
            where neighbours = fmap (\t -> (fst' t, t)) $ edgesFrom g node
                  recurse (v,p) = fmap (p:) $ graphDFS' v (v `Set.insert` closed)
                  
graphBFS ::
  forall v e g n. (Ord v, Integral n, Graph v e g) =>
  g -> v -> (v -> Bool) -> Maybe [(v, n, e)]
graphBFS g s0 goal = graphBFS' [(s0,[])] Set.empty
  where graphBFS' :: [(v,[(v, n, e)])] -> Set v -> Maybe [(v, n, e)]
        graphBFS' [] _ = Nothing
        graphBFS' ((node,path):fringe) closed
          | goal node = Just path
          | node `member` closed = graphBFS' fringe closed
          | otherwise = graphBFS'
                          (fringe ++ neighbours)
                          (node `Set.insert` closed)
            where neighbours = fmap (\t -> (fst' t, t:path)) $ edgesFrom g node
