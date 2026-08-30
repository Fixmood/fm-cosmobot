{-|
Module      : Bot.Agent.Failure
Description : Structured agent failure categories
Stability   : experimental
-}

module Bot.Agent.Failure
  ( FailureCategory (..)
  , Failure (..)
  , failureFromException
  , failureStatus
  , transientFailure
  , permanentArgumentFailure
  , permissionDeniedFailure
  , budgetExhaustedFailure
  , externalServiceFailure
  , uncertainSideEffectFailure
  )
where

import Bot.Prelude
import qualified Bot.Effect.LLM as LLM
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP

data FailureCategory
  = TransientFailure
  | PermanentArgumentError
  | PermissionDenied
  | BudgetExhausted
  | ExternalServiceUnavailable
  | UncertainSideEffectState
  deriving (Eq, Show)

data Failure = Failure
  { category :: !FailureCategory
  , userMessage :: !Text
  , detail :: !Text
  }
  deriving (Eq, Show)

instance Exception Failure

transientFailure :: Text -> Text -> Failure
transientFailure =
  makeFailure TransientFailure

permanentArgumentFailure :: Text -> Text -> Failure
permanentArgumentFailure =
  makeFailure PermanentArgumentError

permissionDeniedFailure :: Text -> Text -> Failure
permissionDeniedFailure =
  makeFailure PermissionDenied

budgetExhaustedFailure :: Text -> Text -> Failure
budgetExhaustedFailure =
  makeFailure BudgetExhausted

externalServiceFailure :: Text -> Text -> Failure
externalServiceFailure =
  makeFailure ExternalServiceUnavailable

uncertainSideEffectFailure :: Text -> Text -> Failure
uncertainSideEffectFailure =
  makeFailure UncertainSideEffectState

makeFailure :: FailureCategory -> Text -> Text -> Failure
makeFailure category userMessage detail =
  Failure{category, userMessage, detail}

failureFromException :: SomeException -> Failure
failureFromException err =
  case fromException err of
    Just failure ->
      failure
    Nothing ->
      classifyException err

classifyException :: SomeException -> Failure
classifyException err =
  case fromException err of
    Just httpErr ->
      httpFailure httpErr
    Nothing ->
      case fromException err of
        Just (LLM.LLMException message) ->
          llmFailure message
        Nothing ->
          Failure
            { category = ExternalServiceUnavailable
            , userMessage = LLM.llmExceptionSummary err
            , detail = Text.pack (show err)
            }

httpFailure :: HTTP.HttpException -> Failure
httpFailure httpErr =
  case httpErr of
    HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout ->
      transient (LLM.llmExceptionSummary (toException httpErr))
    HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout ->
      transient (LLM.llmExceptionSummary (toException httpErr))
    _ ->
      Failure
        { category = ExternalServiceUnavailable
        , userMessage = LLM.llmExceptionSummary (toException httpErr)
        , detail = Text.pack (show httpErr)
        }
  where
    transient message =
      Failure
        { category = TransientFailure
        , userMessage = message
        , detail = Text.pack (show httpErr)
        }

llmFailure :: Text -> Failure
llmFailure message =
  Failure
    { category =
        if "empty" `Text.isInfixOf` Text.toLower message
          then TransientFailure
          else ExternalServiceUnavailable
    , userMessage = "LLM error: " <> message
    , detail = message
    }

failureStatus :: Failure -> Text
failureStatus failure =
  case failure.category of
    TransientFailure ->
      "transient"
    PermanentArgumentError ->
      "permanent_argument_error"
    PermissionDenied ->
      "permission_denied"
    BudgetExhausted ->
      "budget_exhausted"
    ExternalServiceUnavailable ->
      "external_service_unavailable"
    UncertainSideEffectState ->
      "uncertain_side_effect_state"
