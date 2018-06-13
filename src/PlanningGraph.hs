{-# LANGUAGE FlexibleContexts #-}

module PlanningGraph (
    graphPlan
  ) where

import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.State.Class (MonadState, get)

import qualified Data.Set as Set
import Data.Set (Set, member, cartesianProduct)

import Extraction
import Expansion
import PlanTypes

noMutex :: Set (Mutex Proposition) -> Set Proposition -> Bool
noMutex mutexes props =
  not .
  any ((`member` mutexes) . uncurry mutex) $
  cartesianProduct props props

graphPlan ::
  (MonadReader (Set Proposition, Set Action) m, MonadState PlanGraph m) =>
  m (Maybe Plan) -- described on slide 273
graphPlan = do
  graph <- get
  let currentLevel = lastFactLevel graph
  (goalPropositions, actions) <- ask
  if all (`member` getFLvlProps currentLevel) goalPropositions &&
     noMutex (getFLvlMutexes currentLevel) goalPropositions
    then case attemptPlanExtraction graph goalPropositions of
           Right result -> return result
           Left _       -> expandGraph actions >> graphPlan
    else expandGraph actions >> graphPlan
