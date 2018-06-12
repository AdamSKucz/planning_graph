module PlanTypes (
  -- types
    Proposition
  , Action
  , Mutex
  , PlanGraph
  , Plan
  -- propositions interface
  , neg
  -- action interface
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

import qualified Set
import Set (Set)
import GraphTypes

type PropTag = Integer
data Proposition = Pos PropTag | Neg PropTag
  deriving (Eq, Ord)
type Precondition = Proposition
type Effect = Proposition
type ActTag = String
data Action = A ActTag (Set Precondition) (Set Effect)
-- type ParamAction = (Var -> GroundedVar) -> Action
type Mutex a = (a,a)
data FactLevel = FLevel (Set Proposition) (Set (Mutex Proposition))
  deriving (Eq)
data ActionLevel = ALevel (Set Action) (Set (Mutex Action))
  deriving (Eq)
data PlanGraph = PG [(FactLevel, ActionLevel)] FactLevel
type Plan = [Set Action]

neg :: Proposition -> Proposition
neg (Pos p) = Neg p
neg (Neg p) = Pos p

instance Eq Action where
  (==) = (==) `on` actTag

instance Ord Action where
  compare = comparing actTag

actTag :: Action -> ActTag
actTag (A t _ _) -> t

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

initGraph :: Set Proposition -> PlanGraph
initGraph ps = PG [] $ fLvl ps Set.empty

numFactLvls :: Integral n => PlanGraph -> n
numFactLvls (PG ls _) = 1 + length ls

lastFactLevel :: PlanGraph -> FactLevel
lastFactLevel (PG [] f) = f
lastFactLevel (PG ((f,_):_) _) = f

getFactLevel :: Integral n => PlanGraph -> n -> FactLevel
getFactLevel (PG _ f) 0 = f
getFactLevel g@(PG ls _) n = fst $ ls !! (numFactLvls g - 1 - n)

addLevel :: ActionLevel -> FactLevel -> PlanGraph -> PlanGraph
addLevel al fl (PG f ls) -> PG f $ (fl,al) : ls
