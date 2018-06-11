module Extraction (
    attemptPlanExtraction
  ) where

import PlanTypes
import GraphTypes

instance Graph (Int, Set Proposition) (Set Action) PlanGraph where
  edgesFrom g (n, props) =
    if n == 0
      then []
      else
        fmap (\as -> ((n-1, setPreconds as), 1, as)) .
        Set.filter setAdmissible .
        powerSet actions
    where nMax = numFactLvls g
          alvl = getActionLevel g n
          actions = getALvlActions alvl
          actMutexes = getALvlMutexes alvl
          preMutexes = let f = getFLvlMutexes $ getFactLevel g n
          setPreconds = unions . fmap preconds . Set.toList
          setEffects  = unions . fmap effects  . Set.toList
          setAdmissible as = all ($ as) [noMutex, noMutexPreconds, achieve props]
          noMutex as =
            all ((`notMember` actMutexes) . mutex) $
            cartesianProduct actions actions
          noMutexPreconds as = all ok actions
            where ok a =
                    let ps = preconds a
                    in all ((`notMember` preMutexes) . mutex) $
                       cartesianProduct ps ps
          achieve props as = all (`member` setEffects as) props

attemptPlanExtraction :: MonadState Graph m => m (Either (Maybe Plan) ())

