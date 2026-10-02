{-# LANGUAGE OverloadedLabels #-}
{-|
Module      : Bot.Handler.Huma
Description : Deterministic "*fix <codes>" decoding, straight from the code table
Stability   : experimental
-}

module Bot.Handler.Huma
  ( humaHandlers
  )
where

import Bot.Core.Message
import Bot.Core.Route
import Bot.Prelude
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Util.Process as ProcessUtil
import qualified Data.Text as Text
import qualified Effectful.Process.Typed as TypedProcess
import qualified Effectful.Timeout as Timeout
import Effectful.Timeout (Timeout)
import System.Exit (ExitCode (..))

-- | The prefix the owner types. A constant so the matcher, the help text and
-- the reply cannot drift apart.
humaFixPrefix :: Text
humaFixPrefix = "*fix"

-- | The decoder reads this table and prints the sentence. Lives on the mounted
-- runtime volume, so replacing the table needs no rebuild and no restart.
humaDecodeScript :: FilePath
humaDecodeScript = "/data/huma/decode.py"

-- | The decoder takes ~140ms. This is a backstop so a wedged process can never
-- hold a route open -- not a budget to spend.
humaDecodeTimeoutMicros :: Int
humaDecodeTimeoutMicros = 10 * 1000000

-- | Owner-only for now. Setting this to False opens the command to everyone,
-- which costs nothing extra now that the model is out of the loop.
humaOwnerOnly :: Bool
humaOwnerOnly = True

-- | Answers "*fix <codes>" without waking the agent.
--
-- Why this exists: measured on run agent--hxbXEri1l1W8EFZPkLjZA, the agent path
-- took 3150ms of which 2978ms (94.5%) was LLM turns (tool_enable, run_bash,
-- then the answer) and only 156ms was the decode. A code-table lookup is
-- deterministic, so routing it here removes the model entirely.
--
-- A second effect: this route replies through 'Chat.replyTo', so it never goes
-- through the Ask reply path that prepends the "😻 FM：" speaker prefix. The
-- answer is the sentence and nothing else, which is what the owner asked for.
humaHandlers
  :: ( Chat.Chat :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => [RouteHandler es]
humaHandlers =
  [humaFixRoute]

humaFixRoute
  :: ( Chat.Chat :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => RouteHandler es
humaFixRoute =
  withHelp
    (RouteHelp (humaFixPrefix <> " <虎码编码>") "把 *fix 后面的虎码编码解回句子（直接查码表，不经过模型）。")
    $ stopOn humaFixFilter \message codes ->
      if humaOwnerOnly && not message.digest.senderIsSuperuser
        then void $ Chat.replyTo message "这条现在只对主人开放。"
        else runDecode message codes

runDecode
  :: ( Chat.Chat :> es
     , Timeout :> es
     , Concurrent :> es
     , IOE :> es
     , TypedProcess.TypedProcess :> es
     )
  => IncomingMessage
  -> Text
  -> Eff es ()
runDecode message codes
  | Text.null codes =
      void $ Chat.replyTo message ("用法：" <> humaFixPrefix <> " <虎码编码>")
  | otherwise = do
      result <- Timeout.timeout humaDecodeTimeoutMicros $
        ProcessUtil.readProcessGroupWithExitCode "python3" [humaDecodeScript, Text.unpack codes]
      case result of
        Nothing ->
          void $ Chat.replyTo message "解码超时了，稍后再试。"
        Just (exitCode, stdoutText, stderrText) ->
          let output = Text.strip stdoutText
              detail = Text.strip stderrText
           in case exitCode of
                ExitSuccess
                  | Text.null output -> void $ Chat.replyTo message "解码器没有输出。"
                  | otherwise -> void $ Chat.replyTo message output
                ExitFailure _ ->
                  void $
                    Chat.replyTo message $
                      "解码失败：" <> if Text.null detail then "解码器返回了非零状态。" else detail

-- | Matches "*fix <codes>" and nothing else.
--
-- Deliberately stricter than 'command': that one strips a prefix and accepts
-- whatever follows, so "*fixfoo" would be read as the codes "foo". Here the
-- prefix must be followed by whitespace or end the message.
humaFixFilter :: MessageFilter Text
humaFixFilter =
  MessageFilter \message -> do
    rest <- Text.stripPrefix humaFixPrefix (Text.strip message.text)
    if Text.null rest || Text.isPrefixOf " " rest || Text.isPrefixOf "　" rest
      then Just (Text.strip rest)
      else Nothing
