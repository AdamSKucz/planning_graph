module Util (
    fst'
  , snd'
  , third'
  , cartesianProduct
  , powerSet
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

third' :: (a,b,c) -> c
third' (_,_,c) = c

cartesianProduct :: Set a -> Set b -> Set (a,b)
cartesianProduct a b  = Set.fromDistinctAscList [(i,j) | i <- as, j <- bs]
    where as = Set.toAscList a
          bs = Set.toAscList b

powerSet :: Set a -> Set (Set a)
powerSet =
  Set.fromDistinctDescList .
  fmap Set.fromDistinctDescList .
  powerList .
  Set.toDescList
  where powerList [] = [[]]
        powerList (x:xs) = let prev = powerList xs in fmap (x:) prev ++ prev
