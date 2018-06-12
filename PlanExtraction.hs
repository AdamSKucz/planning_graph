module Extraction (
    attemptPlanExtraction
  ) where

import PlanTypes
import GraphTypes
import Util (third')
import Search

instance Graph (Int, Set Proposition) (Set Action) PlanGraph where
  edgesFrom g (n, props) =
    if n == 0
      then []
      else
        fmap (\as -> ((n-1, setPreconds as), 1, as)) .
        Set.filter setAdmissible .
        powerSet actions
    where alvl = getActionLevel g n
          actions = getALvlActions alvl
          actMutexes = getALvlMutexes alvl
          preMutexes = getFLvlMutexes $ getFactLevel g (n-1)
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

attemptPlanExtraction ::
  MonadState PlanGraph m =>
  Set Proposition ->
  m (Either (Maybe Plan) ())
attemptPlanExtraction goals = do
  g <- get
  let maxN = numFactLvls g - 1
  let soln = graphBFS g (maxN, goals) ((==0) . fst)
  case soln of
    Just s  -> return . Left $ fmap third' s
    Nothing ->
      if maxN > 0 && getFactLevel g maxN == getFactLevel g (maxN - 1)
      then return $ Left Nothing
      else return $ Right ()
