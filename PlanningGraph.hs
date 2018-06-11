graphPlan :: MonadState Graph m => m (Maybe Plan) -- described on slide 273
graphPlan = do
  currentLevel <- gets getCurrentLevel
  if allAppear goalPropositions currentLevel &&
     noMutex goalPropositions currentLevel
    then do extractable <- attemptPlanExtraction
            case extractable of
              Left result -> return result
              Right _     -> expandGraph >> graphPlan
    else expandGraph >> graphPlan
