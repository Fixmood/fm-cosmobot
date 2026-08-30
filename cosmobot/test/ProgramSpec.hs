{-# LANGUAGE DataKinds #-}

module Main (main) where

import Bot.Agent.Core
import Bot.Agent.Control (finishToolTurn)
import Bot.Agent.Middleware.Python (interpretPython)
import Bot.Agent.Program.Python
import Bot.Agent.Tool (Tool, ToolTag (..), toolTags)
import Bot.Agent.Tools.Common (specialTag, workTag)
import Bot.Agent.Tools.Python
import Bot.Agent.Types (ToolResult, permanentArgumentFailure, toolText, toolTextWithImages)
import Bot.Core.Transcript (Transcript (..))
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Prelude (Show (show))
import qualified Streaming.Prelude as S hiding (show)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup "agent core interaction-tree laws"
      [ testProperty "Tau t ≈ t" propTauInvisible
      , testProperty "Vis is congruent when every continuation is" propVisCongruence
      , testProperty "lift pure ≈ pure" propLiftPure
      , testProperty "fmap agrees with bind and pure" propFmapIsBind
      , testProperty "bind preserves Tau" propBindTau
      , testProperty "bind distributes through Vis" propBindVis
      , testProperty "bind preserves model and tool continuations" propBindBothEvents
      , testProperty "Kleisli associativity" propKleisliAssociativity
      , testGroup "Python structural interpretation"
          [ testCase "enters Python before applying the saved continuation" testPythonEntry
          , testCase "orchestrates multiple tools before resuming once" testPythonMultiToolOrchestration
          , testCase "completed and failed exits each apply the saved continuation once" testPythonExits
          , testCase "mixed batches reject before Python or sibling work" testPythonMixedBatch
          , testCase "unrelated nodes and continuation branches are preserved" testPythonPreservesUnrelated
          , testCase "tool turn keeps results before image context" testToolTurnMessageOrder
          , testCase "py is a special composition tool" testPythonToolContract
          ]
      ]

-- QuickCheck generates the real core type. The wrapper exists only because a
-- function-valued 'Program' cannot have ordinary 'Show' or 'Arbitrary'
-- instances. Generated visible events have two continuations, and 'eutt'
-- checks both.
newtype ProgramCase = ProgramCase
  { program :: Program (Eff '[]) Int
  }

instance Show ProgramCase where
  show _ =
    "<agent program>"

instance Arbitrary ProgramCase where
  arbitrary =
    sized generateProgram

  shrink _ =
    []

generateProgram :: Int -> Gen ProgramCase
generateProgram 0 =
  ProgramCase . pure <$> arbitrary
generateProgram size =
  frequency
    [ (3, ProgramCase . pure <$> arbitrary)
    , (2, tauCase)
    , (2, outputCase)
    , (3, visibleCase)
    ]
  where
    smaller =
      (.program) <$> generateProgram (size - 1)
    branch =
      (.program) <$> generateProgram (size `div` 2)
    tauCase =
      ProgramCase . tau <$> smaller
    outputCase = do
      value <- arbitrary :: Gen Int
      next <- smaller
      pure (ProgramCase (emit [i|output:#{value}|] next))
    visibleCase =
      ProgramCase <$> (visible <$> arbitrary <*> arbitrary <*> branch <*> branch)

propTauInvisible :: ProgramCase -> Property
propTauInvisible ProgramCase{program} =
  tau program ~= program

propVisCongruence :: Bool -> Int -> ProgramCase -> ProgramCase -> Property
propVisCongruence useModel key ProgramCase{program = onLeft} ProgramCase{program = onRight} =
  visible useModel key (tau onLeft) (tau onRight)
    ~= visible useModel key onLeft onRight

propLiftPure :: Int -> Property
propLiftPure value =
  (lift (pure value) :: Program (Eff '[]) Int) ~= pure value

propFmapIsBind :: ProgramCase -> Fun Int Int -> Property
propFmapIsBind ProgramCase{program} generated =
  fmap f program
    ~= (program >>= pure . f)
  where
    f = applyFun generated

propBindTau :: ProgramCase -> Fun Int ProgramCase -> Property
propBindTau ProgramCase{program} generated =
  (tau program >>= next)
    ~= tau (program >>= next)
  where
    next = arrow generated

propBindVis
  :: Bool
  -> Int
  -> ProgramCase
  -> ProgramCase
  -> Fun Int ProgramCase
  -> Property
propBindVis
  useModel
  key
  ProgramCase{program = onLeft}
  ProgramCase{program = onRight}
  generated =
    (visible useModel key onLeft onRight >>= next)
      ~= visible useModel key (onLeft >>= next) (onRight >>= next)
    where
      next = arrow generated

propBindBothEvents
  :: Int
  -> ProgramCase
  -> ProgramCase
  -> ProgramCase
  -> ProgramCase
  -> Fun Int ProgramCase
  -> Property
propBindBothEvents
  key
  ProgramCase{program = onFinalEven}
  ProgramCase{program = onFinalOdd}
  ProgramCase{program = onToolsEven}
  ProgramCase{program = onToolsOdd}
  generated =
    (bothVisible key onFinalEven onFinalOdd onToolsEven onToolsOdd >>= next)
      ~= bothVisible
            key
            (onFinalEven >>= next)
            (onFinalOdd >>= next)
            (onToolsEven >>= next)
            (onToolsOdd >>= next)
  where
    next = arrow generated

propKleisliAssociativity
  :: ProgramCase
  -> Fun Int ProgramCase
  -> Fun Int ProgramCase
  -> Property
propKleisliAssociativity ProgramCase{program} generatedF generatedG =
  ((program >>= f) >>= g)
    ~= (program >>= \value -> f value >>= g)
  where
    f = arrow generatedF
    g = arrow generatedG

testPythonEntry :: Assertion
testPythonEntry =
  case observeEvent (pythonTransform enteredInterpreter solePythonProgram) of
    Returned outputs (turn, contents) -> do
      outputs @?= [ObservedContent "python-state", ObservedContent "continued"]
      turn @?= 1
      contents @?= ["done"]
    _ ->
      assertFailure "py was not structurally consumed"
  where
    enteredInterpreter _ _ _ =
      emit "python-state" (pure (toolText "done"))

testPythonMultiToolOrchestration :: Assertion
testPythonMultiToolOrchestration =
  case observeEvent (pythonTransform interpreter solePythonProgram) of
    Returned outputs (turn, contents) -> do
      outputs @?=
        [ ObservedContent "tool:first"
        , ObservedContent "tool:second:first-result"
        , ObservedContent "continued"
        ]
      turn @?= 1
      contents @?= ["first-result + second-result"]
    _ ->
      assertFailure "multi-tool Python orchestration did not resume its saved continuation"
  where
    interpreter _ _ (Right _) = do
      firstResult <- emit "tool:first" (pure "first-result")
      secondResult <- emit ("tool:second:" <> firstResult) (pure "second-result")
      pure (toolText (firstResult <> " + " <> secondResult))
    interpreter _ _ (Left err) =
      pure (toolText err)

testPythonExits :: Assertion
testPythonExits = do
  assertExit (PythonCompleted "complete") ["complete"]
  assertExit
    (PythonFailed (permanentArgumentFailure "failed" "failed"))
    ["failed"]
  where
    assertExit exit expected =
      case observeEvent (pythonTransform (\_ _ _ -> pure (pythonExitResult exit)) solePythonProgram) of
        Returned outputs (turn, contents) -> do
          outputs @?= [ObservedContent "continued"]
          turn @?= 1
          contents @?= expected
        _ ->
          assertFailure "Python exit did not resume its saved continuation"

testPythonMixedBatch :: Assertion
testPythonMixedBatch =
  case observeEvent (pythonTransform enteredInterpreter mixedPythonProgram) of
    Returned outputs (turn, contents) -> do
      outputs @?= [ObservedContent "continued"]
      turn @?= 1
      length contents @?= 2
      assertBool "every mixed-batch call gets a protocol result" $
        all ("must be called alone" `Text.isInfixOf`) contents
    _ ->
      assertFailure "mixed py batch was not rejected as one program"
  where
    enteredInterpreter _ _ _ =
      emit "python-state" (pure (toolText "must not run"))

testPythonPreservesUnrelated :: Assertion
testPythonPreservesUnrelated = do
  let original = tau (emit "before" (bothVisible 7 (pure (1 :: Int)) (pure 2) (pure 3) (pure 4)))
      transformed = pythonTransform (\_ _ _ -> pure (toolText "unused")) original
  assertBool "unrelated branches changed" (eutt original transformed)

testToolTurnMessageOrder :: Assertion
testToolTurnMessageOrder = do
  let firstCall = LLM.ToolCall "call-1" "first" "{}"
      secondCall = LLM.ToolCall "call-2" "second" "{}"
      request = requestWith (firstCall :| [secondCall])
      results = toolTextWithImages "first-result\n" ["https://example.invalid/image"] :| [toolText "second-result"]
      (continuedState, _) = runPureEff (finishToolTurn (\_ _ action -> action) request (pure results))
      messages = Foldable.toList continuedState.transcript.messages
  map (\message -> (message.role, messageText message)) messages @?=
    [ ("tool", "first-result\n")
    , ("tool", "second-result")
    , ("user", "Image context returned by tool first:\nfirst-result")
    ]

testPythonToolContract :: Assertion
testPythonToolContract = do
  toolTags (runPythonTool :: Tool (Eff '[])) @?=
    [Named specialTag, Named workTag]
  assertBool "description must recommend py for composing multiple tools" $
    all (`Text.isInfixOf` runPythonDescription)
      [ "multiple tool calls"
      , "sequencing, branching, loops, aggregation, or recovery"
      , "def run_tool(name: str, args: Json) -> ToolSuccess"
      , "def run_tools(calls: list[ToolCall]) -> list[ToolSuccess]"
      ]

messageText :: LLM.ChatMessage -> Text
messageText message =
  case message.content of
    Just (LLM.TextContent content) -> content
    Just (LLM.PartsContent (LLM.TextPart content : _)) -> content
    _ -> ""

pythonTransform
  :: (ToolRequest -> LLM.ToolCall -> Either Text PythonRequest -> Program (Eff '[]) ToolResult)
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
pythonTransform interpreter =
  interpretPython interpreter [runPythonToolName] (\_ _ action -> action)

solePythonProgram :: Program (Eff '[]) (Int, [Text])
solePythonProgram =
  runTools (requestWith (pythonCall "complete" :| [])) >>= continued

mixedPythonProgram :: Program (Eff '[]) (Int, [Text])
mixedPythonProgram =
  runTools (requestWith (pythonCall "mixed" :| [toolCall 9])) >>= continued

continued :: TurnState -> Program (Eff '[]) (Int, [Text])
continued continuedState =
  emit "continued" (pure (continuedState.turn, transcriptToolTexts continuedState.transcript))

requestWith :: NonEmpty LLM.ToolCall -> ToolRequest
requestWith calls =
  ToolRequest
    { agentState = emptyState 0
    , answered = emptyTranscript
    , toolContent = ""
    , toolCalls = calls
    }

pythonCall :: Text -> LLM.ToolCall
pythonCall code =
  LLM.ToolCall
    { id = "call-python"
    , name = runPythonToolName
    , arguments = TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode $
        Aeson.object ["code" Aeson..= code]
    }

transcriptToolTexts :: Transcript -> [Text]
transcriptToolTexts (Transcript messages) =
  [ content
  | message <- Foldable.toList messages
  , message.role == "tool"
  , Just (LLM.TextContent content) <- [message.content]
  ]

arrow :: Fun Int ProgramCase -> Int -> Program (Eff '[]) Int
arrow generated =
  (.program) . applyFun generated

tau :: Program (Eff '[]) result -> Program (Eff '[]) result
tau =
  Program . pure . Continues

emit :: Text -> Program (Eff '[]) result -> Program (Eff '[]) result
emit output next =
  Program do
    S.yield (ContentDelta output)
    next.observe

data Choice
  = ChooseEven
  | ChooseOdd
  deriving (Eq, Show, Enum, Bounded)

toolVisible
  :: Int
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
toolVisible key onEven onOdd =
  trigger (RunTools (toolRequest key)) >>= \turnState ->
    if even turnState.turn then onEven else onOdd

modelVisible
  :: Int
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
modelVisible key onFinal onTools =
  trigger (RunModel (emptyState key)) >>= \(_, answer) ->
    case answer of
      LLM.ChatFinalAnswer{} -> onFinal
      LLM.ChatToolRequest{} -> onTools

visible
  :: Bool
  -> Int
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
visible useModel
  | useModel = modelVisible
  | otherwise = toolVisible

bothVisible
  :: Int
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
  -> Program (Eff '[]) result
bothVisible key onFinalEven onFinalOdd onToolsEven onToolsOdd = do
  (_, answer) <- runModel (emptyState key)
  case answer of
    LLM.ChatFinalAnswer{} ->
      toolVisible key onFinalEven onFinalOdd
    LLM.ChatToolRequest{} ->
      toolVisible key onToolsEven onToolsOdd

-- Equality up to finite Tau: silently follow 'Continues', retain output and
-- visible-event order, and compare every response branch of a visible event.
(~=) :: (Eq result, Show result) => Program (Eff '[]) result -> Program (Eff '[]) result -> Property
left ~= right =
  counterexample "programs are not equivalent up to Tau" $
    eutt left right

eutt :: Eq result => Program (Eff '[]) result -> Program (Eff '[]) result -> Bool
eutt left right =
  case (observeEvent left, observeEvent right) of
    (Returned leftOutputs leftResult, Returned rightOutputs rightResult) ->
      leftOutputs == rightOutputs && leftResult == rightResult
    (ObservedTools leftOutputs leftKey leftContinue, ObservedTools rightOutputs rightKey rightContinue) ->
      leftOutputs == rightOutputs
        && leftKey == rightKey
        && all
          (\choice -> eutt (leftContinue (choiceState choice)) (rightContinue (choiceState choice)))
          [ChooseEven, ChooseOdd]
    (ObservedModel leftOutputs leftTurn leftContinue, ObservedModel rightOutputs rightTurn rightContinue) ->
      leftOutputs == rightOutputs
        && leftTurn == rightTurn
        && all
          (\response -> eutt (leftContinue response) (rightContinue response))
          modelResponses
    _ ->
      False

data ObservedEvent result
  = Returned ![ObservedOutput] !result
  | ObservedTools ![ObservedOutput] !Text !(TurnState -> Program (Eff '[]) result)
  | ObservedModel ![ObservedOutput] !Int !((TurnState, LLM.ChatAnswer) -> Program (Eff '[]) result)

observeEvent :: Program (Eff '[]) result -> ObservedEvent result
observeEvent =
  runPureEff . go []
  where
    go
      :: [ObservedOutput]
      -> Program (Eff '[]) value
      -> Eff '[] (ObservedEvent value)
    go prior program = do
      outputs S.:> step <- S.toList program.observe
      let observed = prior <> map observeOutput outputs
      case step of
        Finished result ->
          pure (Returned observed result)
        Continues next ->
          go observed next
        Visible (RunTools request) continue ->
          pure (ObservedTools observed request.toolContent continue)
        Visible (RunModel turnState) continue ->
          pure (ObservedModel observed turnState.turn continue)

modelResponses :: [(TurnState, LLM.ChatAnswer)]
modelResponses =
  [ (emptyState 0, LLM.ChatFinalAnswer "answer" Nothing)
  , (emptyState 1, LLM.ChatToolRequest "" (toolCall 0 :| []) Nothing)
  ]

data ObservedOutput
  = ObservedContent !Text
  | ObservedToolCalls ![(Text, Text)]
  | ObservedReplyBoundary
  deriving (Eq, Show)

observeOutput :: Output -> ObservedOutput
observeOutput = \case
  ContentDelta output ->
    ObservedContent output
  ToolCallNotification calls ->
    ObservedToolCalls [(call.id, call.name) | call <- toList calls]
  ReplyBoundary ->
    ObservedReplyBoundary

choiceState :: Choice -> TurnState
choiceState = \case
  ChooseEven ->
    emptyState 0
  ChooseOdd ->
    emptyState 1

toolRequest :: Int -> ToolRequest
toolRequest key =
  ToolRequest
    { agentState = emptyState 0
    , answered = emptyTranscript
    , toolContent = [i|#{key}|]
    , toolCalls = toolCall key :| []
    }

toolCall :: Int -> LLM.ToolCall
toolCall key =
  LLM.ToolCall
    { id = [i|call-#{key}|]
    , name = "algebra"
    , arguments = "{}"
    }

emptyState :: Int -> TurnState
emptyState currentTurn =
  TurnState
    { transcript = emptyTranscript
    , nextModelTranscript = Nothing
    , turn = currentTurn
    , modelTokenUsage = Nothing
    }

emptyTranscript :: Transcript
emptyTranscript =
  Transcript Seq.empty
