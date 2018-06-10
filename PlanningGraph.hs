data CondTag = C1
data Condition = Pos CondTag | Neg CondTag
type Precondition = Condition
type Effect = Condition
data ActTag = String
data Action = Action ActTag (Set Precondition) (Set Effect)
type ParamAction = (Var -> GroundedVar) -> Action
type CondMutex = (Condition, Condition)
data FactLevel = FLevel (Set Condition) (Set CondMutex)
type ActMutex = (Action, Action)
data ActionLevel = ALevel (Set Action) (Set ActMutex)
data Graph = G FactLevel [(ActionLevel, FactLevel)]

groundAction :: (Var -> GroundedVar) -> ParamAction -> [Action]
groundAction = flip ($)

initialGraph :: Graph
initialGraph = G 

expandGraph :: MonadState Graph m => m ()
expandGraph = do
  graph <- get

checkIfPlanPresent :: MonadStateGraph m => m (Either (Maybe Plan) ())
checkIfPlanPresent 

extractPlan :: MonadState Graph m => m (Maybe Plan) -- described on slide 273
extractPlan = do
  currentLevel <- gets getCurrentLevel
  if allGoalPrepositions currentLevel && noMutex goalPrepositions currentLevel
    then do extractable <- checkIfPlanPresent
            case extractable of
              Left result -> return result
              Right _     -> expandGraph >> extractPlan
    else expandGraph >> extractPlan
