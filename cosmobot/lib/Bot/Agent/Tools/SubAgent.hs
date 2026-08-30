{-# LANGUAGE TypeApplications #-}
{-|
Module      : Bot.Agent.Tools.SubAgent
Description : Agent tool for chat-scoped background agents
Stability   : experimental
-}
module Bot.Agent.Tools.SubAgent
  ( subagentTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Agent as Agent
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Bot.Resource.SubAgent as SubAgent
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.List as List
import qualified Data.Text as Text

subagentTool
  :: ( Agent.Agent :> es
     , AgentAudit.AgentAudit :> es
     , Chat.Chat :> es
     , Concurrency.Concurrency :> es
     , LLM.LLM :> es
     , Media.Media :> es
     , Resource.Resource :> es
     , KatipE :> es
     , Prim :> es
     , Concurrent :> es
     , IOE :> es
     )
  => [Tool (Eff es)]
  -> Tool (Eff es)
subagentTool availableTools =
  tagged [workTag]
  . allowWhen (isRight . Resource.accessFromMessage . (.message))
  . withDescription "Manage background agents scoped to the current chat. steer adds input to an active run. list returns accessible subagent resource names. wait_any waits for one current run; wait_all waits for all current runs. Ready resources return immediately, and waiting never cancels unfinished subagents. Use these instead of polling query."
  $ tool "subagent"
      (parsedArguments
        (objectSchema
          [ fieldText "op" "One of: create, list, send, steer, query, wait_any, wait_all, rename, delete."
          , fieldText "name" "Optional globally unique resource name for create; required as the new name for rename."
          , fieldText "system_prompt" "System prompt for create; empty inherits the current system prompt."
          , fieldTextArray "tools" "Tool names exposed to the subagent for create; empty exposes none."
          , fieldText "resource" "Subagent resource name; required for send, steer, query, rename, and delete."
          , fieldTextArray "resources" "Non-empty subagent resource names; required for wait_any and wait_all."
          , fieldText "prompt" "Prompt to send; required for send and steer."
          , fieldInteger "ttl_minutes" "Resource inactivity lifetime in minutes; required for create, minimum 5."
          ]
          ["op"])
        parseCall)
      \call -> do
        context <- askToolContext
        metadata <- askToolCallMetadata
        raise (runCall context metadata call)
  where
    runCall context metadata call =
      case Resource.accessFromMessage context.message of
        Left err -> pure (resourceToolFailure err)
        Right access ->
          case call of
            Create requestedName systemPrompt toolNames ttlMinutes ->
              createSubAgent context metadata requestedName systemPrompt toolNames ttlMinutes
            ListResources ->
              listResourceNames (Proxy @SubAgent.SubAgent) access
            Send resourceId prompt ->
              use access resourceId \subagent ->
                SubAgent.sendPrompt metadata resourceId availableTools context subagent prompt
                  <&> fmap (const "Prompt sent.")
            Steer resourceId prompt ->
              use access resourceId \subagent ->
                SubAgent.steer subagent prompt
                  <&> fmap (const "Steer queued.")
            Query resourceId ->
              use access resourceId (fmap Right . SubAgent.queryOutput)
            WaitAny resourceIds ->
              useMany access resourceIds (fmap (jsonText . outputValue) . SubAgent.waitAnyOutput)
            WaitAll resourceIds ->
              useMany access resourceIds (fmap (jsonText . map outputValue . toList) . SubAgent.waitAllOutputs)
            Destroy resourceId ->
              Resource.destroy access resourceId
                <&> either resourceToolFailure (const (toolText "Subagent destroyed."))
            Rename resourceId newName ->
              Resource.rename access resourceId newName
                <&> either resourceToolFailure (toolText . ("Subagent renamed: " <>))
      where
        use access resourceId action =
          Resource.withResource @SubAgent.SubAgent access resourceId metadata.resourceOwner action
            <&> join . first renderResourceError
            <&> either clientFailure toolText

        useMany access resourceIds action =
          withSubagents access resourceIds action
            <&> either resourceToolFailure (toolText)
          where
            withSubagents access' (resourceId :| remaining) action' =
              acquire resourceId \subagent ->
                gather (resourceId, subagent) [] remaining
              where
                gather firstSubagent rest [] =
                  Right <$> action' (firstSubagent :| reverse rest)
                gather firstSubagent rest (nextId : nextIds) =
                  acquire nextId \subagent ->
                    gather firstSubagent ((nextId, subagent) : rest) nextIds

                acquire resourceId' callback =
                  Resource.withResource @SubAgent.SubAgent access' resourceId' metadata.resourceOwner callback
                    <&> join

        outputValue (resourceId, output) =
          Aeson.object
            [ "resource" Aeson..= resourceId
            , "output" Aeson..= output
            ]

    createSubAgent context metadata requestedName systemPrompt toolNames ttlMinutes =
      case validateTools context toolNames of
        Left err ->
          pure (clientFailure err)
        Right names ->
          createResource requestedName
            Resource.Init
              { message = context.message
              , arguments = SubAgent.SubAgentArgs{systemContext, toolNames = names, ttlMinutes}
              }
            <&> either resourceToolFailure (toolText . ("Subagent created: " <>))
      where
        createResource = \case
          Nothing -> Resource.createForRun @SubAgent.SubAgent metadata.originRunId metadata.resourceOwner
          Just name -> Resource.createNamedForRun @SubAgent.SubAgent metadata.originRunId metadata.resourceOwner name

        systemContext
          | Text.null (Text.strip systemPrompt) = context.systemContext
          | otherwise = systemPrompt

    validateTools context = traverse validate . List.nub
      where
        validate name =
          case find ((== name) . toolName) availableTools of
            Nothing -> Left ("Unknown tool: " <> name)
            Just definition
              | toolAllowed definition context -> Right name
              | otherwise -> Left ("Tool is not allowed: " <> name)

data Call
  = Create !(Maybe Text) !Text ![Text] !Int
  | ListResources
  | Send !Text !Text
  | Steer !Text !Text
  | Query !Text
  | WaitAny !(NonEmpty Text)
  | WaitAll !(NonEmpty Text)
  | Destroy !Text
  | Rename !Text !Text

parseCall :: Aeson.Value -> AesonTypes.Parser Call
parseCall = Aeson.withObject "subagent arguments" \o -> do
  op <- o Aeson..: Key.fromText "op"
  case op :: Text of
    "create" ->
      Create
        <$> o Aeson..:? Key.fromText "name"
        <*> (fromMaybe "" <$> o Aeson..:? Key.fromText "system_prompt")
        <*> (fromMaybe [] <$> o Aeson..:? Key.fromText "tools")
        <*> parseTTLMinutes o
    "list" -> pure ListResources
    "send" -> Send <$> resource o <*> requiredTextValue o "prompt"
    "steer" -> Steer <$> resource o <*> requiredTextValue o "prompt"
    "query" -> Query <$> resource o
    "wait_any" -> WaitAny <$> resources o
    "wait_all" -> WaitAll <$> resources o
    "delete" -> Destroy <$> resource o
    "destroy" -> Destroy <$> resource o
    "rename" -> Rename <$> resource o <*> requiredTextValue o "name"
    _ -> fail "op must be one of: create, list, send, steer, query, wait_any, wait_all, rename, delete."
  where
    resource o = requiredTextValue o "resource"
    resources o = do
      values <- List.nub <$> o Aeson..: Key.fromText "resources"
      valid <- traverse (validateText "resources entries") values
      case valid of
        firstValue : rest -> pure (firstValue :| rest)
        [] -> fail "resources must not be empty."
    requiredTextValue o key =
      o Aeson..: Key.fromText key >>= validateText (Text.unpack key)
    validateText label value =
      value <$ when (Text.null (Text.strip value)) (fail (label <> " must not be empty."))

clientFailure :: Text -> ToolResult
clientFailure err = toolFailure (permanentArgumentFailure err err)
