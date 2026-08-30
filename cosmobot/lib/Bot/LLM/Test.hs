{-|
Module      : Bot.LLM.Test
Description : Test interpreter for the LLM effect
Stability   : experimental
-}

module Bot.LLM.Test
  ( runLLMWith
  )
where

import Bot.Prelude
import qualified Bot.Effect.LLM as LLM
import Bot.LLM.Types

runLLMWith
  :: ([ChatMessage] -> Stream (Of Text) (Eff es) Text)
  -> (LLM.ImageRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text)
  -> (LLM.ImageRequestOptions -> Text -> [Text] -> Maybe Text -> Stream (Of Text) (Eff es) Text)
  -> (LLM.AudioRequestOptions -> [ChatMessage] -> Stream (Of Text) (Eff es) Text)
  -> ([FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer)
  -> Eff (LLM.LLM : es) a
  -> Eff es a
runLLMWith askTextStream askImageStream askImageEditStream askAudioStream askToolsStream = interpret $ \localEnv operation ->
  localSeqLift localEnv \liftLocal ->
    case operation of
      LLM.AskStream messages -> pure (LLM.liftLocalStream liftLocal (askTextStream messages))
      LLM.AskImageStream options messages -> pure (LLM.liftLocalStream liftLocal (askImageStream options messages))
      LLM.AskImageEditStream options prompt imageRefs maskRef -> pure (LLM.liftLocalStream liftLocal (askImageEditStream options prompt imageRefs maskRef))
      LLM.AskAudioStream options messages -> pure (LLM.liftLocalStream liftLocal (askAudioStream options messages))
      LLM.AskToolsStream tools messages -> pure (LLM.liftLocalStream liftLocal (askToolsStream tools messages))
      LLM.ListChatModels -> pure []
      LLM.CurrentChatModel -> pure Nothing
      LLM.SelectChatModel _ -> pure (Left "Chat model switching is unavailable in the test interpreter.")
      LLM.ProbeChatModel _ -> pure (Right ())
      LLM.AddChatModel _ -> pure (Right ())
      LLM.EditChatModel _ _ -> pure (Right ())
      LLM.DeleteChatModel _ -> pure (Right ())
      LLM.ResetChatModel -> pure Nothing
