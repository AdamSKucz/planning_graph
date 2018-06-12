module PlanTypes (
  -- types
    Proposition
  , Action
  , Mutex
  , PlanGraph
  , Plan
  -- propositions interface
  , prop
  , neg
  -- action interface
  , action
  , preconds
  , effects
  -- mutex interface
  , mutex
  , mutexToPair
  -- fact level interface
  , fLvl
  , getFLvlProps
  , getFLvlMutexes
  -- action level interface
  , aLvl
  -- graph interface
  , initPlanGraph
  , lastFactLevel
  , addLevel
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
-- import GraphTypes

import Data.Function (on)
import Data.Ord (comparing)

type PropTag = String
data Proposition = Pos PropTag | Neg PropTag
  deriving (Eq, Ord, Show)
type Precondition = Proposition
type Effect = Proposition
type ActTag = String
data Action = A ActTag (Set Precondition) (Set Effect)
  deriving (Eq, Ord, Show)
-- type ParamAction = (Var -> GroundedVar) -> Action
type Mutex a = (a,a)
data FactLevel = FLevel (Set Proposition) (Set (Mutex Proposition))
  deriving (Eq, Ord, Show)
data ActionLevel = ALevel (Set Action) (Set (Mutex Action))
  deriving (Eq, Ord, Show)
data PlanGraph = PG [(FactLevel, ActionLevel)] FactLevel
type Plan = [Set Action]

prop :: PropTag -> Proposition
prop = Pos

neg :: Proposition -> Proposition
neg (Pos p) = Neg p
neg (Neg p) = Pos p

-- instance Eq Action where
--   (==) = (==) `on` actTag

-- instance Ord Action where
--   compare = comparing actTag

action :: ActTag -> Set Precondition -> Set Effect -> Action
action = A

actTag :: Action -> ActTag
actTag (A t _ _) = t

preconds :: Action -> Set Precondition
preconds (A _ ps _) = ps

effects :: Action -> Set Effect
effects (A _ _ es) = es

mutex :: Ord a => a -> a -> Mutex a
mutex a b = if a <= b then (a,b) else (b,a)

mutexToPair :: Mutex a -> (a,a)
mutexToPair = id

fLvl :: Set Proposition -> Set (Mutex Proposition) -> FactLevel
fLvl = FLevel

getFLvlProps :: FactLevel -> Set Proposition
getFLvlProps (FLevel ps _) = ps

getFLvlMutexes :: FactLevel -> Set (Mutex Proposition)
getFLvlMutexes (FLevel _ ms) = ms

aLvl :: Set Action -> Set (Mutex Action) -> ActionLevel
aLvl = ALevel

initPlanGraph :: Set Proposition -> PlanGraph
initPlanGraph ps = PG [] $ fLvl ps Set.empty

numFactLvls :: PlanGraph -> Int
numFactLvls (PG ls _) = 1 + length ls

lastFactLevel :: PlanGraph -> FactLevel
lastFactLevel (PG [] f) = f
lastFactLevel (PG ((f,_):_) _) = f

getFactLevel :: PlanGraph -> Int -> FactLevel
getFactLevel (PG _ f) 0 = f
getFactLevel g@(PG ls _) n = fst $ ls !! (numFactLvls g - 1 - n)

addLevel :: ActionLevel -> FactLevel -> PlanGraph -> PlanGraph
addLevel al fl (PG ls f) = PG ((fl,al) : ls) f
