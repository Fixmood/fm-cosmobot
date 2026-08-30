{-|
Module      : Bot.Agent.Tools.Image
Description : Agent image-generation tool
Stability   : experimental
-}

module Bot.Agent.Tools.Image
  ( generateImageTool
  , editImageTool
  , viewImageTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Media as Media
import Bot.Prelude
import qualified Data.Text as Text
import qualified Streaming.Prelude as S

generateImageTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool (Eff es)
generateImageTool =
  allowWhen hasExplicitImageGenerationIntent
  . noisy
  . withDescription "Generate an actual image from a prompt and send it to the current chat. Use this when the user *literally* asks to *draw*, *create*, or *generate* an image, including scheduled future image requests. After using this tool, keep the final answer brief and do not repeat the image URL. Never use this when the user is merely asking for, finding, or searching for an image; instead, use the web search tool."
  $ tool "image_generate"
      ( requiredText "prompt" "Image generation prompt. Include the user's visual requirements, style, subject, text, and constraints."
      , optionalImageText "quality" "Optional provider-supported image quality control, such as low, medium, high, auto, standard, or hd."
      , optionalImageText "size" "Optional provider-supported image size control, such as 1024x1024, 1024x1536, 1536x1024, or auto."
      , optionalImageText "background" "Optional provider-supported background control, such as transparent, opaque, or auto."
      , optionalImageText "moderation" "Optional provider-supported moderation control, such as auto or low."
      )
      \rawPrompt quality size background moderation -> do
        context <- askToolContext
        let prompt = Text.strip rawPrompt
            options = LLM.ImageRequestOptions{quality, size, background, moderation}
        generated <- LLM.askImageWithHistoryWithOptions options [LLM.userWithImages prompt (contextDefaultImageUrls context)]
        case ordNub (Chat.replyImageUrls generated) of
          [] ->
            pure (toolText generated)
          imageRefs ->
            sendImageToolResult context.message "Generated" imageRefs generated

hasExplicitImageGenerationIntent :: Context -> Bool
hasExplicitImageGenerationIntent context =
  any (`Text.isInfixOf` normalized)
    [ "生图", "生成图片", "图片生成", "生成一张图", "生成一张", "画图", "画一张"
    , "做张图", "来张图", "绘制", "出图", "生成头像", "生成海报"
    ]
  where
    normalized = Text.toLower context.input.text

editImageTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool (Eff es)
editImageTool =
  noisy
  . withDescription "Edit one or more existing images with the configured image edit model and send the result to the current chat. Use this when the user asks to modify, restyle, inpaint, combine, or use attached/reference images to create an edited image. Omit image_urls to edit images attached to the current message. Use mask_image_url only when the user supplies an explicit mask image; the mask applies to the first input image."
  $ tool "image_edit"
      ( requiredText "prompt" "Image edit instruction. Describe exactly what should change and what should stay preserved."
      , mapArgument (pure . cleanImageUrls . fromMaybe [])
          (optionalTextArray "image_urls" "Optional input image URLs or data image references. Omit this to use the images attached to the current user message. GPT image edit models accept up to 16 input images. Only base64, https://, or file:// is supported.")
      , optionalImageText "mask_image_url" "Optional mask image URL or data image reference. The mask must match the first input image size and format and contain an alpha channel."
      , optionalImageText "quality" "Optional provider-supported image quality control, such as low, medium, high, auto, standard, or hd."
      , optionalImageText "size" "Optional provider-supported image size control, such as 1024x1024, 1024x1536, 1536x1024, or auto."
      , optionalImageText "background" "Optional provider-supported background control, such as transparent, opaque, or auto."
      , optionalImageText "moderation" "Optional provider-supported moderation control, such as auto or low."
      )
      \rawPrompt imageUrls maskImageUrl quality size background moderation -> do
        context <- askToolContext
        let prompt = Text.strip rawPrompt
            options = LLM.ImageRequestOptions{quality, size, background, moderation}
            editArgs = EditImageArgs{prompt, imageUrls, maskImageUrl, options}
        let imageRefs = editImageInputRefs context editArgs
        case validateEditImageRefs imageRefs of
          Just failure ->
            pure (toolFailure failure)
          Nothing -> do
            edited <- S.effects (LLM.askImageEditStreamingWithOptions editArgs.options editArgs.prompt imageRefs editArgs.maskImageUrl)
            case ordNub (Chat.replyImageUrls edited) of
              [] ->
                pure (toolText edited)
              editedRefs ->
                sendImageToolResult context.message "Edited" editedRefs edited

viewImageTool :: Media.Media :> es => Tool (Eff es)
viewImageTool =
  tool "image_view"
    (requiredText "url" "Image URL to add to the current model context. Use an http://, https://, data:image/*, mxc:// (in Matrix), or existing media: media ID.")
    viewImageUrl

data EditImageArgs = EditImageArgs
  { prompt :: !Text
  , imageUrls :: ![Text]
  , maskImageUrl :: !(Maybe Text)
  , options :: !LLM.ImageRequestOptions
  }

optionalImageText :: Text -> Text -> ToolArgument (Maybe Text)
optionalImageText name description =
  mapArgument (pure . (>>= nonEmptyText)) (optionalText name description)

cleanImageUrls :: [Text] -> [Text]
cleanImageUrls =
  filter (not . Text.null) . map Text.strip

editImageInputRefs :: Context -> EditImageArgs -> [Text]
editImageInputRefs context editArgs =
  if null editArgs.imageUrls
    then contextDefaultImageUrls context
    else editArgs.imageUrls

contextDefaultImageUrls :: Context -> [Text]
contextDefaultImageUrls context =
  messageInputImageUrls context.input

validateEditImageRefs :: [Text] -> Maybe Failure
validateEditImageRefs imageRefs
  | null imageRefs =
      Just (permanentArgumentFailure "image_edit requires at least one input image." "image_edit requires at least one input image. Attach an image to the message or provide image_urls.")
  | length imageRefs > 16 =
      Just (permanentArgumentFailure "image_edit accepts at most 16 input images." "image_edit accepts at most 16 input images.")
  | otherwise =
      Nothing

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped

viewImageUrl :: Media.Media :> es => Text -> Eff es ToolResult
viewImageUrl rawUrl
  | not (supportedViewImageUrl url) =
      pure (toolFailure (permanentArgumentFailure "image_view does not support this URL scheme." "image_view accepts only HTTP(S), data:image/*, mxc://, or existing media: references."))
  | otherwise = do
      mediaRef <- Media.normalizeMediaRef url
      if isMediaRef mediaRef
        then cachedImageContext mediaRef
        else pure (toolFailure (permanentArgumentFailure "image_view could not cache an image URL." "image_view could not cache the image URL."))
  where
    url = Text.strip rawUrl

supportedViewImageUrl :: Text -> Bool
supportedViewImageUrl url =
  isMediaRef url || any (`Text.isPrefixOf` Text.toLower url) ["http://", "https://", "data:image/", "mxc://"]

isMediaRef :: Text -> Bool
isMediaRef ref =
  "media:" `Text.isPrefixOf` Text.strip ref

cachedImageContext :: Media.Media :> es => Text -> Eff es ToolResult
cachedImageContext mediaRef =
  Media.mediaFileInfoByRef mediaRef >>= \case
    Just info
      | "image/" `Text.isPrefixOf` Text.toLower info.mimeType ->
          pure (toolTextWithImages [i|Added image to current context: #{mediaRef}|] [mediaRef])
    _ ->
      pure (toolFailure (permanentArgumentFailure "image_view URL is not a cached image." "image_view URL is not a cached image."))

sendImageToolResult :: Chat.Chat :> es => IncomingMessage -> Text -> [Text] -> Text -> Eff es ToolResult
sendImageToolResult message label imageRefs body = do
  sent <- Chat.replyTo message body
  let sentText = show sent :: String
      mediaRefs = filter isMediaRef imageRefs
      mediaText
        | null mediaRefs = ""
        | otherwise = "\nMedia ids: " <> Text.intercalate ", " mediaRefs
  pure (toolTextWithImages [i|#{label} and sent image message id: #{sentText}#{mediaText}|] mediaRefs)
