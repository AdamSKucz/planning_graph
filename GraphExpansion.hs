module Expansion (
    expandGraph
  ) where

import Types

actionMutexes :: Set (Mutex Proposition) -> Set Action -> Set (Mutex Action)

propMutexes :: Set (Mutex Action) -> Set Proposition -> Set (Mutex Proposition)

{-
two-pass approach
in first pass get all actions and facts for the next level
in the second pass find all mutexes for those actions and facts
-}
expandGraph :: MonadState Graph m => Set Action -> m ()
expandGraph allActions = do
  lastLevel <- gets lastFactLevel
  let propositions = getFLvlProps lastLevel
  let mutexes = getFLvlMutexes lastLevel
  let nextActions = filter precondSatisfiable allActions
  let nextProps = propositions `union` unions (fmap effects nextActions)
  let actionMutexes = getActionMutexes mutexes nextActions
  let propMutexes = getPropMutexes actionMutexes nextProps
  modify $ addLevel (aLvl nextActions actionMutexes) (fLvl nextProps propMutexes)
  where precondSatisfiable = all (`elem` propositions) . preconds
