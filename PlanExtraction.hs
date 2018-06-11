module Extraction (
    attemptPlanExtraction
  ) where

attemptPlanExtraction :: MonadStateGraph m => m (Either (Maybe Plan) ())
