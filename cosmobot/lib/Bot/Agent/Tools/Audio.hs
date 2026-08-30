{-|
Module      : Bot.Agent.Tools.Audio
Description : Agent audio-generation tool
Stability   : experimental
-}

module Bot.Agent.Tools.Audio
  ( generateAudioTool
  , generateAudioForMessage
  )
where

import Bot.Agent.Failure (externalServiceFailure)
import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Message (IncomingMessage, MessageId)
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Prelude
import qualified Data.Text as Text

-- | Deterministic path used by the natural-language voice route. This keeps
-- explicit voice requests out of the general file/sandbox tool loop.
generateAudioForMessage
  :: (Chat.Chat :> es, LLM.LLM :> es)
  => IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
generateAudioForMessage message prompt = do
  generated <- LLM.askAudioWithHistoryWithOptions LLM.defaultAudioRequestOptions [LLM.userText (Text.strip prompt)]
  if "Audio generation is not configured:" `Text.isPrefixOf` Text.strip generated
    then pure (Left "尚未配置语音生成 API。需要设置 llm.audio 和 llm.audio_provider.<name> 的 api_key。")
    else Chat.replyAudio message generated Nothing

generateAudioTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool (Eff es)
generateAudioTool =
  tagged [workTag]
  . noisy
  . withDescription "Generate and send a native voice/audio message to the current chat. MUST use this tool for requests such as '说句话让我听听', '发一条语音', '用语音回答', or any request to hear FM speak. Do not use sandbox or send_media for a voice request: send_media is only for an already-existing file when the user explicitly asks for the file itself. After using this tool, keep the final answer brief and do not send a second audio/file message."
  $ tool "audio_generate"
      (requiredText "prompt" "The words to be converted into audio")
      \rawPrompt -> do
        context <- askToolContext
        sent <- generateAudioForMessage context.message rawPrompt
        case sent of
          Right messageId -> do
            let sentText = show messageId :: String
            pure (toolText [i|Generated and sent audio message id: #{sentText}|])
          Left err ->
            pure (toolFailure (externalServiceFailure ("发送音频失败：" <> err) err))
