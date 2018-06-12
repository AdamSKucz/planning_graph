{-# LANGUAGE FlexibleInstances #-}
module Interface (
                 ) where

import qualified Data.Foldable as Foldable

import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set, union)
import qualified Data.Maybe as Maybe
import Data.List (intersperse)

import PlanTypes

type Var = String
type PredTag = String
data Predicate = Pos PredTag [Var] | Neg PredTag [Var]
  deriving (Eq, Ord, Read, Show)
type PrecondPred = Predicate
type EffectPred = Predicate
type ActTag = String
data ParamAction = PA ActTag (Set PrecondPred) (Set EffectPred)
  deriving (Eq, Ord, Read, Show)

type VarValue = String

class Parametrised a where
  freeVars :: a -> Set Var
  ground :: Map Var VarValue -> a -> a
  
  allInstances :: Set VarValue -> a -> [a]
  allInstances vals a = do
    let vars = Set.toList $ freeVars a
    mapping <- mappings vars
    return $ ground mapping a
    where vList = Set.toList vals
          mappings [] = return Map.empty
          mappings (v:vs) = do
            m <- mappings vs
            val <- vList
            return $ Map.insert v val m

instance Parametrised Predicate where
  freeVars (Pos _ vs) = Set.fromList vs
  freeVars (Neg _ vs) = Set.fromList vs
  ground g (Pos t vs) = Pos t' []
    where t' = t ++ "(" ++ (concat . intersperse "," $ fmap (g!) vs) ++ ")"
  ground g (Neg t vs) = let (Pos t' vs') = ground g (Pos t vs) in Neg t' vs'

instance Parametrised ParamAction where
  freeVars (PA _ pre eff) = freeVars pre `union` freeVars eff
  ground g a@(PA t pre eff) = PA t' (sGround pre) (sGround eff)
    where vars = Set.toList $ freeVars a
          t' = t ++ "(" ++ (concat . intersperse "," $ fmap (g!) vars) ++ ")"
          sGround = Set.map $ ground g

-- instance (Parametrised a, Foldable f, Functor f) => Parametrised (f a) where
--   freeVars = Set.unions . fmap freeVars . Foldable.toList
--   ground = fmap . ground

instance Parametrised a => Parametrised [a] where
  freeVars = Set.unions . fmap freeVars
  ground = fmap . ground

instance (Parametrised a, Ord a) => Parametrised (Set a) where
  freeVars = Set.unions . fmap freeVars . Set.toList
  ground = Set.map . ground

castToProp :: Predicate -> Maybe Proposition
castToProp (Pos t []) = return $ prop t
castToProp (Neg t []) = return . neg $ prop t
castToProp _ = Nothing

castToAction :: ParamAction -> Maybe Action
castToAction (PA t pre eff) = do
  pre' <- castAll pre
  eff' <- castAll eff
  return $ action t pre' eff'
  where castAll = fmap Set.fromList . sequence . fmap castToProp . Set.toList

{-
given a set of possible variable values and predicates
instantiates all variables to ground instances
and returns a set of all possible resulting propositions
-}
createFactSet :: Set VarValue -> Set Predicate -> Set Proposition
createFactSet values predicates =
  Set.fromList .
  Maybe.mapMaybe castToProp $
  Set.toList predicates >>= allInstances values

{-
given a set of propositions, possible variable values and actions
instantiates all variables in actions to ground instances,
adds null actions for all propositions
and returns a set of all possible resulting actions
-}
createActionSet ::
  Set Proposition ->
  Set VarValue ->
  Set ParamAction -> Set Action
createActionSet props values pActions =
  union nullActions $
  Set.fromList .
  Maybe.mapMaybe castToAction $
  Set.toList pActions >>= allInstances values
  where nullActions = Set.map makeNullAction props

makeNullAction :: Proposition -> Action
makeNullAction p =
  action ("[" ++ show p ++ "]") (Set.singleton p) (Set.singleton p)
