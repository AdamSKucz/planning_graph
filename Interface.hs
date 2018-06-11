module Interface (
                 ) where

import qualified Set
import Set (Set, union)

import Types

type Var = String
type PredTag = String
data Predicate = Pos PredTag [Var] | Neg PredTag [Var]
type PrecondPred = Predicate
type EffectPred = Predicate
type ActTag = String
data ParamAction = PA ActTag (Set PrecondPred) (Set EffectPred)

class Parametrised a where
  freeVars :: a -> Set Var

instance Parametrised Predicate where
  freeVars (Pos _ vs) = Set.fromList vs
  freeVars (Neg _ vs) = Set.fromList vs

instance Paramterised ParamAction where
  freeVars (PA _ pre eff) = freeVars pre `union` freeVars eff

