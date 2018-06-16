{-# LANGUAGE FlexibleContexts #-}

module Expansion (
    expandGraph
  ) where

import Control.Monad.State.Class (MonadState, gets, modify)

import qualified Data.Set as Set
import Data.Set (Set, member, union, unions)

import Util (cartesianProduct)
import PlanTypes

{-
generate and test approach
first create a set of all action mutexes
then filter by mutex links (slide 264-266)
-}
actionMutexes :: Set (Mutex Proposition) -> Set Action -> Set (Mutex Action)
actionMutexes mutexes actions =
  Set.filter (\m -> any ($ m) mutexRules) allActionMutexes
  where allActionMutexes =
          Set.map (uncurry mutex) $
          cartesianProduct actions actions
        mutexRules = fmap (. mutexToPair)
                       [effectsConflict, effectPrecondConflict, precondConflict]
        conflict :: Set Proposition -> Set Proposition -> Bool
        conflict pas = any (\pb -> any (==pb) $ Set.map neg pas)
        effectsConflict (a, b) = conflict (effects a) (effects b)
        effectPrecondConflict (a, b) =
          conflict (effects a) (preconds b) || conflict (effects b) (preconds a)
        precondConflict :: (Action, Action) -> Bool
        precondConflict (a, b) = 
          any ((`member` mutexes) . uncurry mutex) $
          cartesianProduct (preconds a) (preconds b)

{-
generate and test approach
first create a set of all proposition mutexes
then filter by mutex porssibilities (slide 267-268)
-}
propMutexes ::
  Set Action ->
  Set (Mutex Action) ->
  Set Proposition -> Set (Mutex Proposition)
propMutexes actions actionMutexes props =
  Set.filter (\m -> any ($ m) mutexRules) allPropMutexes
  where allPropMutexes = Set.map (uncurry mutex) $ cartesianProduct props props
        mutexRules = fmap (. mutexToPair) [negation, mutexActions]
        negation (p,p') = p == neg p'
        mutexActions (p,p') =
          let sources k = Set.filter (member k . effects) actions
          in all ((`member` actionMutexes) . uncurry mutex) $
             cartesianProduct (sources p) (sources p')

{-
two-pass approach
in first pass get all actions and facts for the next level
in the second pass find all mutexes for those actions and facts
-}
expandGraph :: MonadState PlanGraph m => Set Action -> m ()
expandGraph allActions = do
  lastLevel <- gets lastFactLevel
  let propositions = getFLvlProps lastLevel
  let mutexes = getFLvlMutexes lastLevel
  let precondSatisfiable = all (`elem` propositions) . preconds
  let nextActions = Set.filter precondSatisfiable allActions
  let nextProps =
        propositions `union` unions (fmap effects $ Set.toList  nextActions)
  let aMutexes = actionMutexes mutexes nextActions
  let pMutexes = propMutexes nextActions aMutexes nextProps
  modify $ addLevel (aLvl nextActions aMutexes) (fLvl nextProps pMutexes)
