{-|
Module      : Bot.Agent.Tools.Typst
Description : Agent Typst rendering tool
Stability   : experimental
-}

module Bot.Agent.Tools.Typst
  ( typstRenderTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Typst as Typst
import Bot.Prelude
import Bot.System.Typst.Types
import qualified Data.Text as Text

typstRenderTool :: (Chat.Chat :> es, Typst.Typst :> es) => Tool (Eff es)
typstRenderTool =
  noisy
  . withDescription "Render a Typst document and send it to the current chat. Use this for diagrams, tables, formulas, posters, or other precise layouts that should be generated from Typst source. The source must be a complete Typst document."
  $ tool "typst_render"
      ( requiredText "source" "Complete Typst source. Use self-contained content; external files are not available."
      , requiredArgument (fieldText "format" "'png' or 'pdf'. For QQ: only use PNG.")
      , optionalText "caption" "Optional short caption to include in the tool result for context. It is not sent as a separate message."
      )
      \source format caption -> do
        context <- askToolContext
        Typst.withTypst format source \outputPath -> do
          sent <- case format of
            TypstOutputPNG -> Chat.replyTo context.message (Chat.imageDirective ("file://" <> Text.pack outputPath))
            TypstOutputPDF -> (: []) <$> Chat.uploadFile context.message outputPath Nothing
          let sentText = show sent :: String
              captionText :: Text
              captionText =
                maybe "" (" Caption: " <>) caption
          pure (toolText [i|Rendered and sent Typst document message id: #{sentText}.#{captionText}|])
