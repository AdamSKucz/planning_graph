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

type VarValue = String

class Parametrised a where
  freeVars :: a -> Set Var
  ground :: Map Var VarValue -> a -> a
  
  allInstances :: Set VarValue -> a -> [a]
  allInstances vals a = do
    let vars = Set.toList $ freeVars a
    mapping <- mapppings vars
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
    where t' = t ++ "(" ++ (intersperse ',' $ fmap (g!) vs) ++ ")"
  ground g (Neg t vs) = let (Pos t' vs') = ground g (Pos t vs) in Neg t' vs'

instance Paramterised ParamAction where
  freeVars (PA _ pre eff) = freeVars pre `union` freeVars eff
  ground g a@(PA t pre eff) = PA t' (sGround pre) (sGround eff)
    where vars = Set.toList $ freeVras a
          t' = t ++ "(" ++ (intersperse ',' fmap (g!) vars) ++ ")"
          sGround = Set.map $ ground g

coerceToProp :: Predicate -> Maybe Proposition
coerceToProp (Pos t []) = return $ Pos t
coerceToProp (Neg t []) = return $ Neg t
coerceToProp _ = Nothing

coerceToAction :: ParamAction -> Maybe Action
coerceToAction (PA t pre eff) = do
  pre' <- coerceAll pre
  eff' <- coerceAll eff
  return $ A t pre' eff'
  where coerceAll = fmap Set.fromList . sequence . fmap coerceToProp . Set.toList

{-
given a set of possible variable values and predicates
instantiates all variables to ground instances
and returns a set of all possible resulting propositions
-}
createFactSet :: Set VarValue -> Set Predicate -> Set Proposition
createFactSet values predicates =
  Set.fromList .
  mapMaybe coerceToProp $
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
  mapMaybe coerceToAction $
  Set.toList pActions >>= allInstances values
  where nullActions = Set.map makeNullAction props

makeNullAction :: Proposition -> Action
makeNullAction p = A ("[" ++ show p ++ "]") (Set.singleton p) (Set.singleton p)
