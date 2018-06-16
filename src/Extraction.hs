{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Extraction (
    attemptPlanExtraction
  ) where

import qualified Data.Set as Set
import Data.Set (Set, member, notMember, unions)

import PlanTypes
import GraphTypes
import Util (third', powerSet, cartesianProduct)
import Search

instance Graph (Int, Set Proposition) (Set Action) PlanGraph where
  edgesFrom g (n, props) =
    if n == 0
      then []
      else
        fmap (\as -> ((n-1, setPreconds as), 1, as)) .
        filter setAdmissible .
        Set.toList $
        powerSet actions
    where alvl = getActLevel g n
          actions = getALvlActions alvl
          actMutexes = getALvlMutexes alvl
          preMutexes = getFLvlMutexes $ getFactLevel g (n-1)
          setPreconds = unions . fmap preconds . Set.toList
          setEffects  = unions . fmap effects  . Set.toList
          setAdmissible as = all ($ as) [noMutex, noMutexPreconds, achieve props]
          noMutex as =
            all ((`notMember` actMutexes) . uncurry mutex) $
            cartesianProduct actions actions
          noMutexPreconds as = all ok actions
            where ok a =
                    let ps = preconds a
                    in all ((`notMember` preMutexes) . uncurry mutex) $
                       cartesianProduct ps ps
          achieve props as = all (`member` setEffects as) props

{-
result represents whether a decision was reached
Left () means no decision
Right Nothing means a decision that no plan exists
Right (Just plan) means a plan was found
-}
attemptPlanExtraction ::
  PlanGraph ->
  Set Proposition ->
  Either () (Maybe Plan)
attemptPlanExtraction g goals =
  let maxN = numFactLvls g - 1
  in let soln = graphDFS g (maxN, goals) ((==0) . fst)
  in case soln of
    Just s  -> Right . Just $ fmap third' s
    Nothing ->
      if maxN > 0 && getFactLevel g maxN == getFactLevel g (maxN - 1)
      then Right Nothing
      else Left ()
