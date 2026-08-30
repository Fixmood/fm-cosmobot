{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Saucenao
Description : SauceNAO command handler
Stability   : experimental
-}

module Bot.Handler.Saucenao
  ( saucenaoHandlers
  )
where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Media as Media
import Bot.Core.Route
import Bot.Core.Message
import Bot.Handler.Saucenao.Config
import Bot.Prelude
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Req
import System.IO.Error (userError)

saucenaoCommand :: Text
saucenaoCommand =
  "!saucenao"

-- | Routes for SauceNAO reverse image search commands.
saucenaoHandlers
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, HTTP.HTTP :> es, Media.Media :> es, KatipE :> es, IOE :> es)
  => SaucenaoConfig
  -> [RouteHandler es]
saucenaoHandlers saucenaoCfg =
  [ saucenaoRoute saucenaoCfg
  ]

saucenaoRoute
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, HTTP.HTTP :> es, Media.Media :> es, KatipE :> es, IOE :> es)
  => SaucenaoConfig
  -> RouteHandler es
saucenaoRoute saucenaoCfg =
  withHelp (RouteHelp "!saucenao" "Reverse-search images in the replied-to message.") $
  stopOn (command saucenaoCommand) \message _ -> do
    logInfo [i|matched saucenao route: #{incomingMessageLogLine message}|]
    Concurrency.fire "saucenao.search" (sendSaucenaoResults saucenaoCfg message)

sendSaucenaoResults
  :: (Chat.Chat :> es, HTTP.HTTP :> es, Media.Media :> es, KatipE :> es, IOE :> es)
  => SaucenaoConfig
  -> IncomingMessage
  -> Eff es ()
sendSaucenaoResults cfg message =
  handleError do
    case cfg.apiKey of
      Nothing ->
        void $ Chat.replyTo message "SauceNAO 需要配置 API key。"
      Just _ -> do
        referenced <- fetchReferencedMessage message
        case referenced >>= nonEmptyImageUrls of
          Nothing ->
            void $ Chat.replyTo message "请回复一条包含图片的消息。"
          Just imageUrls -> do
            results <- traverse (searchOne cfg) imageUrls
            let body = Text.intercalate "\n\n" (mapMaybe renderResult results)
            if Text.null body
              then void $ Chat.replyTo message "没有找到相似度大于 80% 的结果。"
              else void $ Chat.replyTo message body
  where
    handleError action =
      action `catchSync` \err -> do
        logError [i|SauceNAO search failed: #{show err :: String}|]
        void $ Chat.replyTo message [i|SauceNAO 搜索失败：#{displayException err}|]

fetchReferencedMessage
  :: Chat.Chat :> es
  => IncomingMessage
  -> Eff es (Maybe ReferencedMessage)
fetchReferencedMessage message =
  traverse (Chat.getMessageContent message) message.replyToMessageId <&> join

nonEmptyImageUrls :: ReferencedMessage -> Maybe [Text]
nonEmptyImageUrls referenced =
  viaNonEmpty toList referenced.imageUrls

searchOne
  :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es)
  => SaucenaoConfig
  -> Text
  -> Eff es (Maybe SearchResult)
searchOne cfg imageUrl = do
  result <- searchImage cfg imageUrl
  pure (result >>= highSimilarityResult)

data SearchResult = SearchResult
  { similarity :: !Double
  , thumbnail  :: !(Maybe Text)
  , urls       :: ![Text]
  , title      :: !(Maybe Text)
  , source     :: !(Maybe Text)
  }
  deriving (Show, Generic, Aeson.ToJSON)

searchImage :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es) => SaucenaoConfig -> Text -> Eff es (Maybe SearchResult)
searchImage cfg imageUrl = do
  resolvedUrl <- resolveSearchImageUrl imageUrl
  value <- HTTP.runReq $
    responseBody <$> req GET saucenaoUrl NoReqBody jsonResponse (saucenaoSearchOptions cfg resolvedUrl)
  case join (AesonTypes.parseMaybe saucenaoApiError value) of
    Just err -> throwIO (userError (Text.unpack err))
    Nothing  -> pure ()
  pure (join (AesonTypes.parseMaybe firstResult value))

saucenaoUrl :: Url 'Https
saucenaoUrl =
  https "saucenao.com" /: "search.php"

saucenaoSearchOptions :: SaucenaoConfig -> Text -> Option 'Https
saucenaoSearchOptions cfg imageUrl =
  saucenaoRequestOptions
    <> "output_type" =: (2 :: Int)
    <> "numres" =: (1 :: Int)
    <> "db" =: (999 :: Int)
    <> "url" =: imageUrl
    <> maybe mempty ("api_key" =:) cfg.apiKey

resolveSearchImageUrl :: Media.Media :> es => Text -> Eff es Text
resolveSearchImageUrl imageUrl
  | "media:" `Text.isPrefixOf` stripped = do
      publicUrl <- Media.publicMediaRef stripped
      if isPublicImageUrl publicUrl
        then pure publicUrl
        else throwIO (userError [i|SauceNAO media id has no public URL: #{imageUrl}|])
  | isPublicImageUrl stripped =
      pure stripped
  | otherwise =
      throwIO (userError [i|Unsupported SauceNAO image URL: #{imageUrl}|])
  where
    stripped = Text.strip imageUrl

isPublicImageUrl :: Text -> Bool
isPublicImageUrl url =
  "https://" `Text.isPrefixOf` stripped || "http://" `Text.isPrefixOf` stripped
  where
    stripped = Text.strip url

saucenaoRequestOptions :: Option scheme
saucenaoRequestOptions =
  header "User-Agent" (TextEncoding.encodeUtf8 saucenaoUserAgent)

saucenaoUserAgent :: Text
saucenaoUserAgent =
  "cosmobot/0.1 (+https://github.com/ksqsf/cosmobot)"

firstResult :: Aeson.Value -> AesonTypes.Parser (Maybe SearchResult)
firstResult =
  Aeson.withObject "SauceNAOResponse" $ \o -> do
    results <- fromMaybe [] <$> o Aeson..:? "results"
    traverse parseResult (viaNonEmpty head results)

saucenaoApiError :: Aeson.Value -> AesonTypes.Parser (Maybe Text)
saucenaoApiError =
  Aeson.withObject "SauceNAOResponse" $ \o -> do
    sauceHeader <- o Aeson..: "header"
    status <- sauceHeader Aeson..:? "status" :: AesonTypes.Parser (Maybe Int)
    message <- sauceHeader Aeson..:? "message"
    shortRemaining <- sauceHeader Aeson..:? "short_remaining" :: AesonTypes.Parser (Maybe Int)
    longRemaining <- sauceHeader Aeson..:? "long_remaining" :: AesonTypes.Parser (Maybe Int)
    pure case (status, message, shortRemaining, longRemaining) of
      (Just value, Just msg, _, _) | value < 0 ->
        Just ("SauceNAO API error: " <> msg)
      (_, Just msg, _, _) | "anonymous account" `Text.isInfixOf` Text.toLower msg ->
        Just ("SauceNAO API error: " <> msg)
      (_, _, Just value, _) | value <= 0 ->
        Just "SauceNAO API short-term rate limit exceeded."
      (_, _, _, Just value) | value <= 0 ->
        Just "SauceNAO API daily rate limit exceeded."
      _ ->
        Nothing

parseResult :: Aeson.Value -> AesonTypes.Parser SearchResult
parseResult =
  Aeson.withObject "SauceNAOResult" $ \o -> do
    resultHeader <- o Aeson..: "header"
    similarity <- resultHeader Aeson..: "similarity" >>= parseSimilarity
    thumbnail <- resultHeader Aeson..:? "thumbnail"
    data_ <- o Aeson..: "data"
    urls <- fromMaybe [] <$> data_ Aeson..:? "ext_urls"
    title <- data_ Aeson..:? "title"
    source <- data_ Aeson..:? "source"
    pure SearchResult
      { similarity = similarity
      , thumbnail = thumbnail
      , urls = urls
      , title = title
      , source = source
      }

parseSimilarity :: Text -> AesonTypes.Parser Double
parseSimilarity text =
  maybe (fail [i|Invalid SauceNAO similarity: #{text}|]) pure $
    readMaybe (toString (Text.strip text))

highSimilarityResult :: SearchResult -> Maybe SearchResult
highSimilarityResult result
  | result.similarity > 80 = Just result
  | otherwise              = Nothing

renderResult :: Maybe SearchResult -> Maybe Text
renderResult result = do
  match <- result
  resultUrl <- viaNonEmpty head match.urls
  let similarityText = show match.similarity :: Text
  Just $ Text.unlines $
    [ "相似度：" <> similarityText <> "%"
    , resultUrl
    ]
      <> [ ReplyBody.imageDirective thumbnail | thumbnail <- maybeToList match.thumbnail ]
