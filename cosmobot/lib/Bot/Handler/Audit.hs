{-|
Module      : Bot.Handler.Audit
Description : Superuser audit commands for agent tool use
Stability   : experimental
-}

module Bot.Handler.Audit
  ( auditHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import Bot.Core.Thread
import qualified Bot.Effect.AgentAudit as AgentAudit
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Resource as Resource
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Thread
import qualified Data.Text as Text
import Data.Time (FormatTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)

auditHandlers
  :: (AgentAudit.AgentAudit :> es, Chat.Chat :> es, Resource.Resource :> es, Storage.Storage :> es, Prim :> es, Concurrent :> es, IOE :> es)
  => ThreadStore
  -> [RouteHandler es]
auditHandlers threads =
  [ withHelp (RouteHelp "!stats" "Show cumulative agent statistics for a replied thread message (superuser only).") $
    stopOn (command "!stats") (handleStats threads)
  , withHelp (RouteHelp "!audit [all|log|<id>]" "Inspect agent tool-use audit records (superuser only).") $
    requireAuth isSuperuser (\message -> void $ Chat.replyTo message "只有 superuser 可以查看 audit。") $
      stopOn (command "!audit") (handleAudit threads)
  ]

handleStats
  :: (AgentAudit.AgentAudit :> es, Chat.Chat :> es, Resource.Resource :> es, Storage.Storage :> es, Prim :> es, Concurrent :> es, IOE :> es)
  => ThreadStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleStats threads message args
  | not (Text.null (Text.strip args)) =
      void $ Chat.replyTo message "用法：回复一条 agent thread 消息并发送 !stats"
  | otherwise =
      case message.replyToMessageId of
        Nothing ->
          void $ Chat.replyTo message "用法：回复一条 agent thread 消息并发送 !stats"
        Just parentId -> do
          let parentKey = threadMessageKey message parentId
          messageIds <- lookupThreadMessageIds threads parentKey
          completed <- AgentAudit.queryThreadMessagesAudit (map (threadMessageKey message) messageIds)
          activeRunId <- lookupActiveThreadRunId threads parentKey
          pendingSteers <- lookupActiveThreadPendingSteers threads parentKey
          active <- maybe (pure []) AgentAudit.queryRunAudit activeRunId
          let records = ordNubOn (.id) (linkedMessageAudit messageIds completed <> active)
              runIds = threadRunIds activeRunId records
          subAgentRuns <- querySubAgentRuns runIds records
          resources <- case Resource.accessFromMessage message of
            Left _ -> pure []
            Right access -> Resource.listCreatedByRuns access runIds
          now <- liftIO getCurrentTime
          void $ Chat.replyTo message (renderThreadStats now parentId (length messageIds) activeRunId pendingSteers records subAgentRuns resources)

handleAudit
  :: (AgentAudit.AgentAudit :> es, Chat.Chat :> es, Storage.Storage :> es, Prim :> es)
  => ThreadStore
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleAudit threads message args =
  case parseAuditId args of
    Nothing
      | Text.toLower (Text.strip args) == "log" ->
          case message.replyToMessageId of
            Just parentId -> do
              records <- AgentAudit.queryThreadAudit (threadMessageKey message parentId)
              void $ Chat.replyTo message (renderThreadAuditLog parentId records)
            Nothing ->
              void $ Chat.replyTo message "用法：回复一条 agent thread 消息并发送 !audit log"
      | Text.toLower (Text.strip args) == "all" ->
          case message.replyToMessageId of
            Just parentId -> do
              messageIds <- lookupThreadMessageIds threads (threadMessageKey message parentId)
              records <- AgentAudit.queryThreadMessagesAudit (map (threadMessageKey message) messageIds)
              void $ Chat.replyTo message (renderThreadToolUses parentId records)
            Nothing ->
              void $ Chat.replyTo message "用法：回复一条 agent thread 消息并发送 !audit all"
      | Text.null (Text.strip args) ->
          case message.replyToMessageId of
            Just parentId -> do
              records <- AgentAudit.queryThreadAudit (threadMessageKey message parentId)
              void $ Chat.replyTo message (renderThreadToolUses parentId records)
            Nothing -> do
              toolUses <- AgentAudit.queryRecentToolUses recentAuditLimit
              void $ Chat.replyTo message (renderAuditList toolUses)
      | otherwise ->
          void $ Chat.replyTo message "用法：!audit、!audit all、!audit log 或 !audit <id>"
    Just auditId -> do
      detail <- AgentAudit.queryToolUse auditId
      void $ Chat.replyTo message (maybe [i|没有找到 audit id #{auditId}。|] renderAuditDetail detail)

parseAuditId :: Text -> Maybe Integer
parseAuditId =
  readMaybe . toString . Text.strip

recentAuditLimit :: Int
recentAuditLimit =
  20

renderAuditList :: [AgentAudit.ToolUseDetail] -> Text
renderAuditList [] =
  "最近没有 agent tool use。"
renderAuditList toolUses =
  Text.unlines ("*Recent agent tool uses*" : map renderToolUseLine toolUses)

data SubAgentRunAudit = SubAgentRunAudit
  { parentRunId :: !Text
  , subagentId :: !Text
  , runId :: !Text
  , records :: ![AgentAudit.AgentAuditRecord]
  }

querySubAgentRuns
  :: AgentAudit.AgentAudit :> es
  => [Text]
  -> [AgentAudit.AgentAuditRecord]
  -> Eff es [SubAgentRunAudit]
querySubAgentRuns rootRunIds rootRecords =
  go rootRunIds (subAgentLinks rootRecords)
  where
    go _ [] =
      pure []
    go seen ((parentRunId, subagentId, runId) : pending)
      | runId `elem` seen =
          go seen pending
      | otherwise = do
          records <- AgentAudit.queryRunAudit runId
          rest <- go (runId : seen) (pending <> subAgentLinks records)
          pure (SubAgentRunAudit{parentRunId, subagentId, runId, records} : rest)

    subAgentLinks records =
      [ (runId, subagentId, childRunId)
      | AgentAudit.AgentAuditRecord
          { event = AgentAudit.SubAgentRunStarted{runId, subagentId, childRunId}
          } <- records
      ]

renderThreadStats :: UTCTime -> MessageId -> Int -> Maybe Text -> Maybe Int -> [AgentAudit.AgentAuditRecord] -> [SubAgentRunAudit] -> [Resource.SomeResourceObject] -> Text
renderThreadStats now parentId branchMessages activeRunId pendingSteers records subAgentRuns resources
  | null records, isNothing activeRunId =
      [i|没有找到消息 #{messageIdText parentId} 对应的 agent stats。|]
  | otherwise =
      Text.unlines $
        [ "*Thread stats*"
        , [i|- status: #{if isJust activeRunId then "active" else "complete" :: Text}|]
        , [i|- branch: #{branchMessages} messages, #{length records} audit events|]
        ]
          <> [ [i|- current run: `#{runId}` (phase: #{currentPhase}, #{fromMaybe 0 pendingSteers} pending steers)|]
             | runId <- maybeToList activeRunId
        ]
          <> [ [i|- runs: #{length runIds} (#{length finishedRunIds} finished, #{length interruptedRunIds} interrupted#{unreportedRunsSuffix})|]
        , [i|- model turns: #{length modelTurns}|]
        , renderRunUsage "tokens" allTurnUsage
        , renderRunUsage "current run" currentRunTurns
        , renderContextMessages contextMessageCounts
        , renderCompactionUsage compactionUsages
        , renderEnabledTools enabledToolGroups
        , [i|- tool calls: #{length toolUses} (#{okTools} ok, #{failedTools} failed, #{interruptedTools} interrupted, #{length runningTools} running#{unreportedToolsSuffix})|]
        , renderToolTime toolUses
        , renderModelTime modelDurations
        , renderRunWallTime runIds runDurations currentRunDuration
        ]
          <> maybeToList renderRunConfig
          <> [ "- running tools: " <> Text.intercalate ", " (map renderRunningTool runningTools)
             | not (null runningTools)
             ]
          <> renderSubAgentStats now runIds subAgentRuns
          <> renderThreadResources resources
  where
    runIds =
      threadRunIds activeRunId records
    modelTurns =
      [ (runId, tokenUsage)
      | AgentAudit.AgentAuditRecord{event = AgentAudit.ModelTurnFinished{runId, tokenUsage}} <- records
      ]
    allTurnUsage =
      map snd modelTurns
    contextMessageCounts =
      [ messageCount
      | AgentAudit.AgentAuditRecord{event = AgentAudit.ModelTurnStarted{messageCount}} <- records
      ]
    currentRunId =
      viaNonEmpty last runIds
    currentRunTurns =
      case currentRunId of
        Nothing ->
          []
        Just current ->
          [ tokenUsage
          | (runId, tokenUsage) <- modelTurns
          , runId == current
          ]
    compactionUsages =
      [ tokenUsage
      | AgentAudit.AgentAuditRecord{event = AgentAudit.ContextCompacted{tokenUsage}} <- records
      ]
    enabledToolGroups = do
      current <- currentRunId
      join . viaNonEmpty last $
        [ toolGroups
        | AgentAudit.AgentAuditRecord
            { event = AgentAudit.ModelTurnStarted{runId, toolGroups}
            } <- records
        , runId == current
        ]
    toolUses =
      AgentAudit.toolUsesFromAuditRecords records
    okTools =
      length
        [ ()
        | AgentAudit.ToolUseDetail{status = AgentAudit.ToolUseFinished{status = "ok"}} <- toolUses
        ]
    failedTools =
      length
        [ ()
        | AgentAudit.ToolUseDetail{status = AgentAudit.ToolUseFinished{status}} <- toolUses
        , status /= "ok"
        ]
    interruptedTools =
      length
        [ ()
        | AgentAudit.ToolUseDetail{status = AgentAudit.ToolUseInterrupted{}} <- toolUses
        ]
    runningTools =
      [ toolUse
      | toolUse@AgentAudit.ToolUseDetail{status = AgentAudit.ToolUseInProgress} <- toolUses
      , Just toolUse.runId == activeRunId
      ]
    unreportedTools =
      length toolUses - okTools - failedTools - interruptedTools - length runningTools
    unreportedToolsSuffix :: Text
    unreportedToolsSuffix
      | unreportedTools <= 0 = ""
      | otherwise = [i|, #{unreportedTools} stale/unreported|]
    finishedRunIds =
      ordNub
        [ runId
        | AgentAudit.AgentAuditRecord{event = AgentAudit.AgentRunFinished{runId}} <- records
        ]
    interruptedRunIds =
      ordNub
        [ runId
        | AgentAudit.AgentAuditRecord{event = AgentAudit.AgentRunInterrupted{runId}} <- records
        ]
    reportedRunIds =
      ordNub (finishedRunIds <> interruptedRunIds <> maybeToList activeRunId)
    unreportedRuns =
      length (filter (`notElem` reportedRunIds) runIds)
    unreportedRunsSuffix :: Text
    unreportedRunsSuffix
      | unreportedRuns == 0 = ""
      | otherwise = [i|, #{unreportedRuns} legacy/unreported|]
    runDurations =
      [(runId, runDurationMilliseconds now activeRunId runId records) | runId <- runIds]
    currentRunDuration =
      activeRunId >>= \runId ->
        join (snd <$> find ((== runId) . fst) runDurations)
    currentPhase =
      phaseFromRecords activeRunId runningTools records
    modelDurations =
      modelTurnDurations now activeRunId currentPhase records
    renderRunConfig = do
      latestRunId <- viaNonEmpty last runIds
      AgentAudit.AgentAuditRecord
        { event = AgentAudit.AgentRunStarted{maxTurns, exposedTools}
        } <- viaNonEmpty last
          [ record
          | record@AgentAudit.AgentAuditRecord{event} <- records
          , AgentAudit.eventRunId event == latestRunId
          , AgentAudit.AgentRunStarted{} <- [event]
          ]
      pure [i|- run config: #{maxTurns} max tool turns, #{length exposedTools} exposed tools|]
    renderRunningTool toolUse =
      let toolName = toolUse.toolName
          auditId = toolUse.auditId
          elapsed = max 0 (floor (diffUTCTime now toolUse.occurredAt * 1000))
      in [i|`#{toolName}` (`id=#{auditId}`, #{renderMilliseconds elapsed})|]

threadRunIds :: Maybe Text -> [AgentAudit.AgentAuditRecord] -> [Text]
threadRunIds activeRunId records =
  ordNub (map (AgentAudit.eventRunId . (.event)) records <> maybeToList activeRunId)

renderSubAgentStats :: UTCTime -> [Text] -> [SubAgentRunAudit] -> [Text]
renderSubAgentStats _ _ [] =
  ["- subagents: 0 runs"]
renderSubAgentStats now rootRunIds runs =
  [i|- subagents: #{length runs} runs|] : concatMap (renderRun 2) rootRuns
  where
    rootRuns =
      filter ((`elem` rootRunIds) . (.parentRunId)) runs

    renderRun indentation SubAgentRunAudit{subagentId, runId, records} =
      let modelUsages =
            [ tokenUsage
            | AgentAudit.AgentAuditRecord
                { event = AgentAudit.ModelTurnFinished{tokenUsage}
                } <- records
            ]
          compactionUsages =
            [ tokenUsage
            | AgentAudit.AgentAuditRecord
                { event = AgentAudit.ContextCompacted{tokenUsage}
                } <- records
            ]
          toolUses = AgentAudit.toolUsesFromAuditRecords records
          modelDurations = modelTurnDurations now Nothing "complete" records
          status = subAgentRunStatus records
          active = [runId | status == "running"]
          duration = runDurationMilliseconds now (viaNonEmpty head active) runId records
          durationText = maybe "time unreported" renderMilliseconds duration
          children = filter ((== runId) . (.parentRunId)) runs
          nested =
            [spaces (indentation + 2) <> [i|- subagents: #{length children} runs|] | not (null children)]
              <> concatMap (renderRun (indentation + 4)) children
      in
      [ spaces indentation <> [i|- `#{subagentId}` (`#{runId}`): #{status}, #{length modelUsages} model turns, #{length toolUses} tool calls, #{durationText}|]
      , spaces (indentation + 2) <> renderRunUsage "tokens" modelUsages
      , spaces (indentation + 2) <> renderModelTime modelDurations
      , spaces (indentation + 2) <> renderCompactionUsage compactionUsages
      , spaces (indentation + 2) <> renderToolTime toolUses
      ] <> nested

    spaces count =
      Text.replicate count " "

subAgentRunStatus :: [AgentAudit.AgentAuditRecord] -> Text
subAgentRunStatus records =
  fromMaybe "running" . viaNonEmpty last $
    [ status
    | AgentAudit.AgentAuditRecord{event} <- records
    , status <- case event of
        AgentAudit.AgentRunFinished{status} -> ["finished:" <> status]
        AgentAudit.AgentRunInterrupted{} -> ["interrupted"]
        _ -> []
    ]

renderThreadResources :: [Resource.SomeResourceObject] -> [Text]
renderThreadResources resources =
  [i|- resources: #{length resources}|]
    : map renderResource resources
  where
    renderResource :: Resource.SomeResourceObject -> Text
    renderResource resource =
      let resourceId = resource.resourceId
          resourceType = resource.resourceType
          status :: Text
          status = either ("error: " <>) id resource.probeResult
          life :: Text
          life = maybe "permanent" ((<> "m") . show) resource.remainingLifeMinutes
      in [i|  - `#{resourceId}` (`#{resourceType}`): #{status}, #{life}|]

renderEnabledTools :: Maybe [(Text, Int)] -> Text
renderEnabledTools Nothing =
  "- tool enabled: unreported"
renderEnabledTools (Just groups) =
  "- tool enabled: "
    <> Text.intercalate ", " [[i|#{name} (#{count})|] | (name, count) <- groups]

renderUsage :: Text -> Maybe LLM.TokenUsage -> Text
renderUsage label Nothing =
  "- " <> label <> ": unreported"
renderUsage label (Just usage) =
  renderUsageValues label usage.totalTokens usage.promptTokens usage.completionTokens (cacheSuffix usage)

renderRunUsage :: Text -> [Maybe LLM.TokenUsage] -> Text
renderRunUsage label usages =
  renderUsage label (traverse id usages >>= viaNonEmpty sumTokenUsage)

sumTokenUsage :: NonEmpty LLM.TokenUsage -> LLM.TokenUsage
sumTokenUsage usages =
  LLM.TokenUsage
    { promptTokens = sum (fmap (.promptTokens) usages)
    , completionTokens = sum (fmap (.completionTokens) usages)
    , totalTokens = sum (fmap (.totalTokens) usages)
    , cachedPromptTokens = sum <$> traverse (.cachedPromptTokens) usages
    }

renderUsageValues :: Text -> Int -> Int -> Int -> Text -> Text
renderUsageValues label totalTokens promptTokens completionTokens cache =
  [i|- #{label}: #{totalTokens} total (#{promptTokens} prompt, #{completionTokens} completion#{cache})|]

cacheSuffix :: LLM.TokenUsage -> Text
cacheSuffix usage =
  case usage.cachedPromptTokens of
    Nothing ->
      "; request cache: unreported"
    Just cached ->
      let hitRate = renderPercentage cached usage.promptTokens
      in [i|; request cache: #{cached} hit, #{hitRate}|]

renderPercentage :: Int -> Int -> Text
renderPercentage cacheHits promptCount
  | promptCount <= 0 =
      "0.0%"
  | otherwise =
      let tenths = cacheHits * 1000 `div` promptCount
      in [i|#{tenths `div` 10}.#{tenths `mod` 10}%|]

renderCompactionUsage :: [Maybe LLM.TokenUsage] -> Text
renderCompactionUsage [] =
  "- context compactions: 0 calls, 0 total (0 prompt, 0 completion)"
renderCompactionUsage usages =
  case catMaybes usages of
    [] ->
      [i|- context compactions: #{length usages} calls, tokens unreported|]
    reported ->
      let promptTokens = sum (map (.promptTokens) reported)
          completionTokens = sum (map (.completionTokens) reported)
          totalTokens = sum (map (.totalTokens) reported)
          unreported = length usages - length reported
          suffix :: Text
          suffix
            | unreported == 0 = ""
            | otherwise = [i|; #{unreported} unreported|]
      in [i|- context compactions: #{length usages} calls, #{totalTokens} total (#{promptTokens} prompt, #{completionTokens} completion#{suffix})|]

renderToolTime :: [AgentAudit.ToolUseDetail] -> Text
renderToolTime toolUses =
  [i|- tool time: #{renderMilliseconds (sum (mapMaybe duration toolUses))} cumulative across calls|]
  where
    duration toolUse =
      case toolUse.status of
        AgentAudit.ToolUseFinished{durationMilliseconds} -> Just durationMilliseconds
        AgentAudit.ToolUseInterrupted{durationMilliseconds} -> Just durationMilliseconds
        AgentAudit.ToolUseInProgress -> Nothing

renderContextMessages :: [Int] -> Text
renderContextMessages [] =
  "- context messages: unreported"
renderContextMessages counts@(_ : _) =
  let nowCount = fromMaybe 0 (viaNonEmpty last counts)
  in [i|- context messages: #{nowCount} now / #{foldl' max 0 counts} peak|]

renderModelTime :: [(Bool, Maybe Integer)] -> Text
renderModelTime durations
  | null reported =
      [i|- model time: unreported (#{length durations} turns)|]
  | otherwise =
      [i|- model time: #{renderMilliseconds (sum completed)} completed#{currentSuffix}#{unreportedSuffix}|]
  where
    completed =
      [ duration
      | (False, Just duration) <- durations
      ]
    current =
      [ duration
      | (True, Just duration) <- durations
      ]
    reported = completed <> current
    currentSuffix :: Text
    currentSuffix =
      case current of
        [] -> ""
        values -> [i|, #{renderMilliseconds (sum values)} current|]
    unreported = length durations - length reported
    unreportedSuffix :: Text
    unreportedSuffix
      | unreported == 0 = ""
      | otherwise = [i|; #{unreported} unreported|]

renderRunWallTime :: [Text] -> [(Text, Maybe Integer)] -> Maybe Integer -> Text
renderRunWallTime runIds runDurations currentDuration
  | null reported =
      [i|- run wall time: unreported (#{length runIds} runs)|]
  | otherwise =
      [i|- run wall time: #{renderMilliseconds (sum reported)} total (includes model and tools)#{currentSuffix}#{unreportedSuffix}|]
  where
    reported =
      catMaybes (map snd runDurations)
    currentSuffix :: Text
    currentSuffix =
      maybe "" (\duration -> ", " <> renderMilliseconds duration <> " current") currentDuration
    unreportedRuns =
      length runIds - length reported
    unreportedSuffix :: Text
    unreportedSuffix
      | unreportedRuns == 0 = ""
      | otherwise = [i|; #{unreportedRuns} unreported runs|]

modelTurnDurations
  :: UTCTime
  -> Maybe Text
  -> Text
  -> [AgentAudit.AgentAuditRecord]
  -> [(Bool, Maybe Integer)]
modelTurnDurations now activeRunId currentPhase records =
  map duration starts
  where
    starts =
      [ record
      | record@AgentAudit.AgentAuditRecord{event = AgentAudit.ModelTurnStarted{}} <- records
      ]
    duration AgentAudit.AgentAuditRecord{id = startedId, occurredAt = startedAt, event = AgentAudit.ModelTurnStarted{runId, turn}} =
      case viaNonEmpty head
        [ occurredAt
        | AgentAudit.AgentAuditRecord{id = finishedId, occurredAt, event = AgentAudit.ModelTurnFinished{runId = finishedRunId, turn = finishedTurn}} <- records
        , finishedId > startedId
        , finishedRunId == runId
        , finishedTurn == turn
        ] of
        Just finishedAt ->
          (False, Just (millisecondsBetween startedAt finishedAt))
        Nothing
          | activeRunId == Just runId
          , currentPhase == "model" ->
              (True, Just (millisecondsBetween startedAt now))
          | otherwise ->
              (False, Nothing)
    duration _ =
      (False, Nothing)

millisecondsBetween :: UTCTime -> UTCTime -> Integer
millisecondsBetween startedAt finishedAt =
  max 0 (floor (diffUTCTime finishedAt startedAt * 1000))

renderMilliseconds :: Integer -> Text
renderMilliseconds milliseconds =
  let seconds = milliseconds `div` 1000
      tenths = (milliseconds `mod` 1000) `div` 100
  in [i|#{seconds}.#{tenths}s|]

runDurationMilliseconds
  :: UTCTime
  -> Maybe Text
  -> Text
  -> [AgentAudit.AgentAuditRecord]
  -> Maybe Integer
runDurationMilliseconds now activeRunId runId records = do
  startedAt <- viaNonEmpty head
    [ occurredAt
    | AgentAudit.AgentAuditRecord{occurredAt, event = AgentAudit.AgentRunStarted{runId = startedRunId}} <- records
    , startedRunId == runId
    ]
  finishedAt <-
    if activeRunId == Just runId
      then Just now
      else viaNonEmpty last
        [ occurredAt
        | AgentAudit.AgentAuditRecord{occurredAt, event} <- records
        , AgentAudit.eventRunId event == runId
        , isRunEnd event
        ]
  pure (millisecondsBetween startedAt finishedAt)
  where
    isRunEnd = \case
      AgentAudit.AgentRunFinished{} -> True
      AgentAudit.AgentRunInterrupted{} -> True
      _ -> False

phaseFromRecords
  :: Maybe Text
  -> [AgentAudit.ToolUseDetail]
  -> [AgentAudit.AgentAuditRecord]
  -> Text
phaseFromRecords Nothing _ _ =
  "complete"
phaseFromRecords (Just runId) runningTools records
  | not (null runningTools) =
      "tools"
  | otherwise =
      maybe "starting" eventPhase . viaNonEmpty last $
        [ record.event
        | record <- records
        , AgentAudit.eventRunId record.event == runId
        ]
  where
    eventPhase = \case
      AgentAudit.AgentRunStarted{} -> "starting"
      AgentAudit.ModelTurnStarted{} -> "model"
      AgentAudit.ModelTurnFinished{answerKind = "tool_request"} -> "tools"
      AgentAudit.ModelTurnFinished{} -> "finishing"
      AgentAudit.ContextCompacted{} -> "model"
      AgentAudit.SubAgentRunStarted{} -> "tools"
      AgentAudit.ToolCallStarted{} -> "tools"
      AgentAudit.ToolCallFinished{} -> "between turns"
      AgentAudit.AgentRunFinished{} -> "finishing"
      AgentAudit.AgentRunInterrupted{} -> "interrupted"
      AgentAudit.AgentThreadLinked{} -> "finishing"

linkedMessageAudit :: [MessageId] -> [AgentAudit.AgentAuditRecord] -> [AgentAudit.AgentAuditRecord]
linkedMessageAudit messageIds =
  concat . filter linksRequestedMessage . auditOccurrences
  where
    linksRequestedMessage records =
      any
        (\record -> case record.event of
          AgentAudit.AgentThreadLinked{linkedMessageId} ->
            linkedMessageId `elem` messageIds
          _ ->
            False
        )
        records

auditOccurrences :: [AgentAudit.AgentAuditRecord] -> [[AgentAudit.AgentAuditRecord]]
auditOccurrences =
  go []
  where
    go current [] =
      [reverse current | not (null current)]
    go current (record : rest) =
      case record.event of
        AgentAudit.AgentThreadLinked{} ->
          reverse (record : current) : go [] rest
        _ ->
          go (record : current) rest

renderToolUseLine :: AgentAudit.ToolUseDetail -> Text
renderToolUseLine toolUse =
  let auditId = toolUse.auditId
      toolName = toolUse.toolName
      occurredAt = timestamp toolUse.occurredAt
      status = renderStatus toolUse.status
  in
  [i|- #{occurredAt} id=#{auditId} tool=`#{toolName}` #{status}|]

renderAuditDetail :: AgentAudit.ToolUseDetail -> Text
renderAuditDetail toolUse =
  let auditId = toolUse.auditId
      occurredAt = timestamp toolUse.occurredAt
      finishedAt = maybe "(still running)" timestamp toolUse.finishedAt
      runId = toolUse.runId
      turn = toolUse.turn
      toolName = toolUse.toolName
      status = renderStatus toolUse.status
      messageIds = renderMessageIds toolUse.messageIds
      arguments = toolUse.arguments
      result = fromMaybe "(still running)" toolUse.result
  in
  Text.unlines
    [ [i|*Audit `#{auditId}`*|]
    , [i|- started: `#{occurredAt}`|]
    , [i|- finished: `#{finishedAt}`|]
    , [i|- run: `#{runId}`|]
    , [i|- turn: `#{turn}`|]
    , [i|- tool: `#{toolName}`|]
    , [i|- status: `#{status}`|]
    , [i|- message ids: `#{messageIds}`|]
    , ""
    , "*Arguments*"
    , fenced "json" arguments
    , "*Result*"
    , fenced "" result
    ]

renderThreadToolUses :: MessageId -> [AgentAudit.AgentAuditRecord] -> Text
renderThreadToolUses parentId [] =
  [i|没有找到消息 #{messageIdText parentId} 对应的 agent audit。|]
renderThreadToolUses _ records =
  case AgentAudit.toolUsesFromAuditRecords records of
    [] ->
      "该 agent audit 中没有 tool use。"
    toolUses ->
      Text.intercalate "\n" ("*Thread tool uses*" : map renderToolUseBlock toolUses)

renderToolUseBlock :: AgentAudit.ToolUseDetail -> Text
renderToolUseBlock toolUse =
  let auditId = toolUse.auditId
      toolName = toolUse.toolName
      status = renderStatus toolUse.status
      occurredAt = timestamp toolUse.occurredAt
      finishedAt = maybe "(still running)" timestamp toolUse.finishedAt
      runId = toolUse.runId
      turn = toolUse.turn
      resultChars = maybe 0 Text.length toolUse.result
      arguments = toolUse.arguments
  in
  Text.unlines
    [ [i|- `id=#{auditId}` `tool=#{toolName}` `#{status}`|]
    , [i|  - started: `#{occurredAt}`|]
    , [i|  - finished: `#{finishedAt}`|]
    , [i|  - run: `#{runId}` turn: `#{turn}` result chars: `#{resultChars}`|]
    , "  - arguments:"
    , indent (fenced "json" arguments)
    ]

renderThreadAuditLog :: MessageId -> [AgentAudit.AgentAuditRecord] -> Text
renderThreadAuditLog parentId [] =
  [i|没有找到消息 #{messageIdText parentId} 对应的 agent audit。|]
renderThreadAuditLog _ records =
  Text.unlines ("*Thread audit log*" : map renderAuditRecord records)

renderAuditRecord :: AgentAudit.AgentAuditRecord -> Text
renderAuditRecord record =
  let eventId :: Text
      eventId = show record.id
      occurredAt = timestamp record.occurredAt
      event = renderAuditEvent record.id record.event
  in
  [i|- `#{occurredAt}` `event_id=#{eventId}` #{event}|]

renderAuditEvent :: Integer -> AgentAudit.AgentAuditEvent -> Text
renderAuditEvent recordId = \case
  AgentAudit.AgentRunStarted{runId, messageId, maxTurns, exposedTools} ->
    [i|run_started run=#{runId} message=#{maybe "-" messageIdText messageId} max_turns=#{maxTurns} exposed_tools=#{length exposedTools}|]
  AgentAudit.ModelTurnStarted{runId, turn, messageCount, exposedTools} ->
    [i|model_started run=#{runId} turn=#{turn} messages=#{messageCount} exposed_tools=#{length exposedTools}|]
  AgentAudit.ModelTurnFinished{runId, turn, answerKind, contentLength, toolCalls, tokenUsage} ->
    let usage = maybe "tokens=unreported" renderTokenUsage tokenUsage
    in [i|model_finished run=#{runId} turn=#{turn} kind=#{answerKind} content_chars=#{contentLength} tool_calls=#{length toolCalls} #{usage}|]
  AgentAudit.ContextCompacted{runId, turn, messageCount, tokenUsage} ->
    let usage = maybe "tokens=unreported" renderTokenUsage tokenUsage
    in [i|context_compacted run=#{runId} turn=#{turn} messages_before=#{messageCount} #{usage}|]
  AgentAudit.SubAgentRunStarted{runId, childRunId, subagentId} ->
    [i|subagent_started run=#{runId} child_run=#{childRunId} resource=`#{subagentId}`|]
  AgentAudit.ToolCallStarted{runId, turn, toolCall} ->
    let toolName = toolCall.name
        auditId :: Text
        auditId = show recordId
    in [i|started audit_id=#{auditId} run=#{runId} turn=#{turn} tool=`#{toolName}`|]
  AgentAudit.ToolCallFinished{runId, turn, toolName, status, resultLength} ->
    [i|finished run=#{runId} turn=#{turn} tool=`#{toolName}` status=#{status} result_chars=#{resultLength}|]
  AgentAudit.AgentRunFinished{runId, status, finalLength, turnsUsed} ->
    [i|run_finished run=#{runId} status=#{status} final_chars=#{finalLength} turns=#{turnsUsed}|]
  AgentAudit.AgentRunInterrupted{runId, reason} ->
    [i|run run=#{runId} reason=`#{reason}`|]
  AgentAudit.AgentThreadLinked{runId, linkedMessageId, parentMessageId} ->
    let parent = maybe "-" messageIdText parentMessageId
    in [i|`thread_linked` run=#{runId} message=#{messageIdText linkedMessageId} parent=#{parent}|]

renderTokenUsage :: LLM.TokenUsage -> Text
renderTokenUsage usage =
  let LLM.TokenUsage{promptTokens, completionTokens, totalTokens, cachedPromptTokens} = usage
      cached :: Text
      cached = maybe "unreported" show cachedPromptTokens
  in [i|tokens=#{totalTokens} prompt=#{promptTokens} completion=#{completionTokens} cached_prompt=#{cached}|]

renderMessageIds :: [Maybe MessageId] -> Text
renderMessageIds messageIds =
  "[" <> Text.intercalate ", " (map (maybe "null" messageIdText) messageIds) <> "]"

renderStatus :: AgentAudit.ToolUseStatus -> Text
renderStatus = \case
  AgentAudit.ToolUseInProgress ->
    "running"
  AgentAudit.ToolUseFinished{status, durationMilliseconds} ->
    [i|finished(#{status}, #{durationMilliseconds}ms)|]
  AgentAudit.ToolUseInterrupted{reason, durationMilliseconds} ->
    [i|interrupted(#{reason}, #{durationMilliseconds}ms)|]

timestamp :: FormatTime t => t -> Text
timestamp =
  Text.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"

fenced :: Text -> Text -> Text
fenced language body =
  Text.unlines
    [ "```" <> language
    , body
    , "```"
    ]

indent :: Text -> Text
indent =
  Text.unlines . map ("    " <>) . Text.lines
