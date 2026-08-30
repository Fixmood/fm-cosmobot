{-# LANGUAGE DataKinds #-}
{-|
Module      : Bot.Agent.Middleware.Steering
Description : Inject user steering between agent model turns
Stability   : experimental
-}
module Bot.Agent.Middleware.Steering
  ( SteeringControl (..)
  , withSteering
  )
where

import Bot.Agent.Core
import Bot.Core.Message (MessageInput)
import Bot.Core.Transcript (Transcript, appendUserInput)
import Bot.Prelude
import qualified Streaming.Prelude as S

data SteeringControl es = SteeringControl
  { drain :: !(Eff es [MessageInput])
  , complete :: !(Eff es (Maybe [MessageInput]))
  }

withSteering
  :: SteeringControl es
  -> Runtime context (Eff es)
  -> Runtime context (Eff es)
withSteering steering program =
  program
    { aroundProgram = \finalRuntime ->
        program.aroundProgram finalRuntime . steerProgram steering
    , aroundModelTurn = \context agentState action -> do
        pending <- lift steering.drain
        let steeredState = injectSteers pending agentState
        program.aroundModelTurn context steeredState action
    }

steerProgram
  :: SteeringControl es
  -> Program (Eff es) Result
  -> Program (Eff es) Result
steerProgram steering (Program action) =
  Program do
    action >>= \case
      Finished result ->
        pure (Finished result)
      Continues next ->
        pure (Continues (steerProgram steering next))
      Visible event@(RunModel _) continue ->
        pure (Visible event (completeModel continue))
      Visible event continue ->
        pure (Visible event (steerProgram steering . continue))
  where
    completeModel continue response@(agentState, _) =
      Program do
        (continue response).observe >>= \case
          Finished completion ->
            lift steering.complete >>= \case
              Nothing ->
                pure (Finished completion)
              Just steers -> do
                S.yield ReplyBoundary
                pure . Continues . steerProgram steering $
                  runModel (injectCompletedSteers steers agentState completion) >>= continue
          Continues next ->
            pure (Continues (steerProgram steering next))
          Visible event next ->
            pure (Visible event (steerProgram steering . next))

injectSteers
  :: [MessageInput]
  -> TurnState
  -> TurnState
injectSteers [] agentState =
  agentState
injectSteers steers agentState =
  agentState
    { transcript = appendSteers steers agentState.transcript
    , nextModelTranscript = appendSteers steers <$> agentState.nextModelTranscript
    }

injectCompletedSteers
  :: [MessageInput]
  -> TurnState
  -> Result
  -> TurnState
injectCompletedSteers steers agentState completion =
  agentState
    { transcript = appendSteers steers completion.transcript
    , nextModelTranscript = Nothing
    , modelTokenUsage = completion.tokenUsage
    }

appendSteers :: [MessageInput] -> Transcript -> Transcript
appendSteers steers transcript =
  foldl' (flip appendUserInput) transcript steers
