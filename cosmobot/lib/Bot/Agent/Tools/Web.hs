{-|
Module      : Bot.Agent.Tools.Web
Description : Agent web search and fetch tools
Stability   : experimental
-}

module Bot.Agent.Tools.Web
  ( webSearchTool
  , searchQuotaTool
  , webFetchTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Chat.Bridge.FM as FMBridge
import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Util.Html as Html
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import Network.HTTP.Req
import System.Directory (doesFileExist)
import System.IO.Error (userError)
import qualified Text.URI as URI

-- | 一次运行里最多搜几次。和 fetch_url 的预算同一个道理：**撞到上限要能收尾**。
--
-- 2026-10-03 实测：最坏一次运行 24 次搜索/抓取里 **15 次是 search_web，而它当时完全没有上限**；
-- 加上 fetch_url 被拒后模型仍继续调，一起烧了 90 秒，最后还是没找到。
webSearchRunBudget :: Int
webSearchRunBudget = 6

webSearchTool :: (HTTP.HTTP :> es, Chat.Chat :> es, IOE :> es) => Tool (Eff es)
webSearchTool =
  noisy
  . allowWhen (.toolConfig.webSearchEnable)
  . withDescription "Search the web for current information. Returns title, url, and snippet results plus image_urls when the provider finds usable image URLs. For an explicit image-search request, set include_images=true; this tool will immediately send the first returned image as an actual chat image."
  $ toolWithRunState "search_web"
      ( requiredText "query" "Search query."
      , optionalInteger "max_results" "Maximum number of results to return. Defaults to 5 and is capped at 20."
      , optionalBoolean "include_images" "Whether to request image URLs from the search provider. Defaults to true; use true for photo or image requests."
      )
      (\context -> newUseLimiter (Just webSearchRunBudget))
      \checkUseLimit rawQuery requestedMaxResults requestedImages -> do
        -- 预算用尽时**必须告诉它下一步做什么**：只说 "limit reached" 是状态播报，
        -- 实测模型会无视它继续调，光垒一道墙没有用。
        raise checkUseLimit >>= \case
          UseLimitReached currentUses ->
            pure (toolText [i|search_web budget for this run is used up (#{currentUses} calls). Do not call search_web again: answer from what you already have, and if that is not enough, say plainly that you could not find it.|])
          UseAllowed -> do
            context <- askToolContext
            case webArguments "query" rawQuery "max_results" 20
              (fromMaybe 5 context.toolConfig.webSearchMaxResults)
              requestedMaxResults of
              Left err ->
                pure (argumentFailure err)
              Right (query, maxResults) -> do
                let searchConfig = context.toolConfig
                (results, imageUrls) <- webSearch searchConfig query maxResults (fromMaybe True requestedImages)
                if explicitImageSearchRequest context.input.text
                  then case imageUrls of
                    imageRef : _ -> do
                      let body = FMBridge.fmReplyRelayBodyForRequest context.input.text (ReplyBody.imageDirective imageRef)
                      sent <- Chat.replyTo context.message body
                      case rights sent of
                        _ : _ ->
                          pure (toolText (jsonText (Aeson.object
                            [ "query" Aeson..= query
                            , "source" Aeson..= webSearchSource searchConfig.webSearchApi
                            , "result_count" Aeson..= length results
                            , "image_sent" Aeson..= True
                            , "instruction" Aeson..= ("The image was sent as an actual chat image. Do not repeat its URL." :: Text)
                            ])))
                        [] ->
                          let err = Text.intercalate "\n" (lefts sent)
                          in pure (toolFailure Failure
                            { category = ExternalServiceUnavailable
                            , userMessage = "搜索到了图片，但作为图片消息发送失败。"
                            , detail = err
                            })
                    [] ->
                      pure (toolText (jsonText (Aeson.object
                        [ "query" Aeson..= query
                        , "source" Aeson..= webSearchSource searchConfig.webSearchApi
                        , "result_count" Aeson..= length results
                        , "image_sent" Aeson..= False
                        , "instruction" Aeson..= ("No usable image URL was returned. Tell the user the image could not be sent; do not paste ordinary page links." :: Text)
                        ])))
                  else
                    pure (toolText (jsonText (Aeson.object
                      [ "query" Aeson..= query
                      , "source" Aeson..= webSearchSource searchConfig.webSearchApi
                      , "results" Aeson..= results
                      , "image_urls" Aeson..= imageUrls
                      ])))

explicitImageSearchRequest :: Text -> Bool
explicitImageSearchRequest raw =
  let normalized = Text.toCaseFold raw
      action = any (`Text.isInfixOf` normalized)
        [ "搜", "搜索", "找", "查找", "想看", "给我", "来张", "来一张", "发张", "发一张" ]
      subject = any (`Text.isInfixOf` normalized)
        [ "图", "图片", "照片", "海报", "壁纸", "剧照" ]
  in action && subject

webFetchTool :: (HTTP.HTTP :> es, IOE :> es) => Tool (Eff es)
webFetchTool =
  allowWhen (.toolConfig.webFetch)
  . withDescription "Fetch a web page by URL and return extracted readable text. Supports http and https URLs."
  $ toolWithRunState "fetch_url"
      ( requiredText "url" "HTTP or HTTPS URL to fetch."
      , optionalInteger "max_content_tokens" "Approximate maximum content tokens to return. Defaults to the configured tool.web_fetch.max_content_tokens or 50000."
      )
      (\context -> newUseLimiter context.toolConfig.webFetchMaxUses)
      \checkUseLimit rawUrl requestedMaxContentTokens -> do
        context <- askToolContext
        case webArguments "url" rawUrl "max_content_tokens" 200000
          (fromMaybe 50000 context.toolConfig.webFetchMaxContentTokens)
          requestedMaxContentTokens of
          Left err ->
            pure (argumentFailure err)
          Right (url, maxContentTokens) ->
            raise checkUseLimit >>= \case
              UseLimitReached currentUses ->
                -- 这句话原来只是播报状态（"use limit reached: 5."），**模型会无视它继续调**
                -- —— 2026-10-03 实测：一次 run 里 fetch_url 被拒后仍继续，配合当时
                -- 完全没有预算的 search_web，一起烧了 24 次调用 / 90 秒，最后还是没找到。
                -- 拒绝必须**同时告诉它下一步做什么**，否则只是把墙垒在那儿。
                pure (toolText [i|fetch_url budget for this run is used up (#{currentUses} calls). Do not call fetch_url again: answer from what you already have, and if that is not enough, say plainly that you could not find it.|])
              UseAllowed -> do
                page <- fetchWebPage url maxContentTokens
                pure (toolText (jsonText page))

-- | 可轮换的 Tavily key 文件。
data TavilyKeyFile = TavilyKeyFile
  { keyList :: ![Text]
  , activeKey :: !Int
  }

instance Aeson.FromJSON TavilyKeyFile where
  parseJSON = Aeson.withObject "tavily keys" \object ->
    TavilyKeyFile
      <$> object Aeson..: "keys"
      <*> object Aeson..:? "active" Aeson..!= 0

instance Aeson.ToJSON TavilyKeyFile where
  toJSON file =
    Aeson.object
      [ "keys" Aeson..= file.keyList
      , "active" Aeson..= file.activeKey
      ]

tavilyKeyStatePath :: FilePath
tavilyKeyStatePath = "/data/tavily-keys.json"

-- | 所有轮换 key 的额度汇总。为什么需要它：单看 config 里那个 key 会报出
-- 「已用完」，而轮换列表里可能还有一个全新的账号 —— 报总额才有意义。
searchQuotaTool :: (HTTP.HTTP :> es, IOE :> es) => Tool (Eff es)
searchQuotaTool =
  withDescription
    "Report Tavily search quota across every key in the rotation list: used, limit and remaining per account plus the total. Use this whenever the user asks how much search quota is left."
    $ tool "search_quota" noArguments do
      context <- askToolContext
      file <- loadTavilyKeyFile
      let keys = case file of
            Just saved | not (null saved.keyList) -> saved.keyList
            _ -> maybe [] pure context.toolConfig.tavilyApiKey
          activeIndex = maybe 0 (.activeKey) file
      if null keys
        then pure (toolText "还没有配置任何 Tavily key。")
        else do
          usages <- traverse quotaOf keys
          let rows = zip [1 :: Int ..] (zip keys usages)
              totalUsed = sum [used | Just (used, _) <- usages]
              totalLimit = sum [limit | Just (_, limit) <- usages]
              renderRow :: (Int, (Text, Maybe (Int, Int))) -> Text
              renderRow (index, (key, usage)) =
                "  "
                  <> Text.pack (show index)
                  <> ". "
                  <> maskKey key
                  <> "  "
                  <> renderUsage usage
                  <> (if index - 1 == activeIndex then "   <<< 当前在用" else "")
              body = Text.unlines (map renderRow rows)
              summary =
                "Tavily 搜索额度（"
                  <> Text.pack (show (length keys))
                  <> " 个账号）："
                  <> "\n"
                  <> body
                  <> "\n"
                  <> "合计：已用 "
                  <> Text.pack (show totalUsed)
                  <> " / "
                  <> Text.pack (show totalLimit)
                  <> "，剩余 "
                  <> Text.pack (show (max 0 (totalLimit - totalUsed)))
                  <> " 次"
          pure (toolText summary)
  where
    maskKey :: Text -> Text
    maskKey key = Text.pack (take 14 (Text.unpack key)) <> "..."
    renderUsage :: Maybe (Int, Int) -> Text
    renderUsage Nothing = "查询失败"
    renderUsage (Just (used, limit)) =
      "已用 " <> Text.pack (show used) <> " / " <> Text.pack (show limit)
        <> "，剩余 " <> Text.pack (show (max 0 (limit - used)))

-- | 单个 key 的 (已用, 额度)。Tavily /usage 的 account 段里给这两个数。
quotaOf :: HTTP.HTTP :> es => Text -> Eff es (Maybe (Int, Int))
quotaOf apiKey = do
  outcome <-
    try @SomeException $
      HTTP.runReq $
        req
          GET
          (https "api.tavily.com" /: "usage")
          NoReqBody
          jsonResponse
          (header "Authorization" (ByteString.pack [i|Bearer #{apiKey}|]))
  pure $ case outcome of
    Left _ -> Nothing
    Right response ->
      AesonTypes.parseMaybe
        ( Aeson.withObject "tavily usage" \object -> do
            account <- object Aeson..: "account"
            used <- account Aeson..: "plan_usage"
            limit <- account Aeson..: "plan_limit"
            pure (used, limit)
        )
        (responseBody response)


loadTavilyKeyFile :: IOE :> es => Eff es (Maybe TavilyKeyFile)
loadTavilyKeyFile = do
  -- 文件不存在是**正常情况**（没配轮换），不能让它抛异常 ——
  -- 否则一次搜索会变成 "withBinaryFile: does not exist" 的堆栈报错给用户看。
  exists <- liftIO (doesFileExist tavilyKeyStatePath)
  if not exists
    then pure Nothing
    else liftIO (Aeson.decodeFileStrict' tavilyKeyStatePath)

saveTavilyKeyFile :: IOE :> es => TavilyKeyFile -> Eff es ()
saveTavilyKeyFile file = liftIO (Aeson.encodeFile tavilyKeyStatePath file)

-- | 从当前 active 开始试，成功就记住，失败就换下一个。
tavilySearchRotating
  :: (HTTP.HTTP :> es, IOE :> es)
  => ToolConfig
  -> Text
  -> Int
  -> Bool
  -> Eff es ([Aeson.Value], [Text])
tavilySearchRotating cfg query maxResults includeImages = do
  file <- loadTavilyKeyFile
  let fromConfig = maybe [] pure cfg.tavilyApiKey
      candidates = case file of
        Just saved | not (null saved.keyList) -> saved.keyList
        _ -> fromConfig
      startAt = maybe 0 (.activeKey) file
  case candidates of
    [] ->
      throwIO
        ( userError
            "Tavily search is not configured: set tool.web_search.tavily_api_key or add keys to /data/tavily-keys.json."
        )
    _ -> attempt candidates startAt (length candidates)
  where
    -- 从 active 开始，绕一圈把所有 key 都试一遍
    attempt candidates index remaining
      | remaining <= 0 =
          throwIO
            ( userError
                [i|所有 Tavily key 的额度都用完了（#{length candidates} 个都试过）。请补新 key 到 /data/tavily-keys.json。|]
            )
      | otherwise = do
          let key = candidates List.!! (index `mod` length candidates)
          outcome <- try @SomeException (tavilySearch key query maxResults includeImages)
          case outcome of
            Right value -> do
              -- 成功：记住这个位置，下次直接用它，不在死 key 上浪费请求
              saveTavilyKeyFile TavilyKeyFile
                { keyList = candidates
                , activeKey = index `mod` length candidates
                }
              pure value
            Left _ -> attempt candidates (index + 1) (remaining - 1)

webSearch :: (HTTP.HTTP :> es, IOE :> es) => ToolConfig -> Text -> Int -> Bool -> Eff es ([Aeson.Value], [Text])
webSearch cfg query maxResults includeImages =
  case cfg.webSearchApi of
    WebSearchTavily ->
      tavilySearchRotating cfg query maxResults includeImages
    WebSearchBrave ->
      case cfg.braveApiKey of
        Nothing -> throwIO (userError "Brave search is not configured: set tool.web_search.brave_api_key.")
        Just key -> (, []) <$> braveSearch key query maxResults
    WebSearchExa ->
      case cfg.exaApiKey of
        Nothing -> throwIO (userError "Exa search is not configured: set tool.web_search.exa_api_key.")
        Just key -> (, []) <$> exaSearch key query maxResults

webSearchSource :: WebSearchApi -> Text
webSearchSource = \case
  WebSearchTavily -> "tavily"
  WebSearchBrave -> "brave"
  WebSearchExa -> "exa"

tavilySearch :: HTTP.HTTP :> es => Text -> Text -> Int -> Bool -> Eff es ([Aeson.Value], [Text])
tavilySearch apiKey query maxResults includeImages = do
  response <- HTTP.runReq $
    req POST
      (https "api.tavily.com" /: "search")
      (ReqBodyJson (Aeson.object
        [ "query" Aeson..= query
        , "max_results" Aeson..= maxResults
        , "search_depth" Aeson..= Aeson.String "basic"
        , "include_answer" Aeson..= False
        , "include_raw_content" Aeson..= False
        , "include_images" Aeson..= includeImages
        ]))
      jsonResponse
      ( header "Authorization" (ByteString.pack [i|Bearer #{apiKey}|])
          <> webRequestOptions
      )
  either (throwIO . userError) pure $
    AesonTypes.parseEither parseTavilyResults (responseBody response)

braveSearch :: HTTP.HTTP :> es => Text -> Text -> Int -> Eff es [Aeson.Value]
braveSearch apiKey query maxResults = do
  response <- HTTP.runReq $
    req GET
      (https "api.search.brave.com" /: "res" /: "v1" /: "web" /: "search")
      NoReqBody
      jsonResponse
      ( "q" =: query
          <> "count" =: maxResults
          <> header "X-Subscription-Token" (TextEncoding.encodeUtf8 apiKey)
          <> webRequestOptions
      )
  either (throwIO . userError) pure $
    AesonTypes.parseEither parseBraveResults (responseBody response)

exaSearch :: HTTP.HTTP :> es => Text -> Text -> Int -> Eff es [Aeson.Value]
exaSearch apiKey query maxResults = do
  response <- HTTP.runReq $
    req POST
      (https "api.exa.ai" /: "search")
      (ReqBodyJson (Aeson.object
        [ "query" Aeson..= query
        , "numResults" Aeson..= maxResults
        , "contents" Aeson..= Aeson.object
            [ "highlights" Aeson..= Aeson.object ["maxCharacters" Aeson..= (1000 :: Int)]
            ]
        ]))
      jsonResponse
      ( header "x-api-key" (TextEncoding.encodeUtf8 apiKey)
          <> webRequestOptions
      )
  either (throwIO . userError) pure $
    AesonTypes.parseEither parseExaResults (responseBody response)

parseTavilyResults :: Aeson.Value -> AesonTypes.Parser ([Aeson.Value], [Text])
parseTavilyResults =
  Aeson.withObject "Tavily search response" $ \o -> do
    results <- o Aeson..: Key.fromText "results"
    imageUrls <- fromMaybe [] <$> o Aeson..:? Key.fromText "images"
    (, imageUrls) <$> traverse parseResult results
  where
    parseResult =
      Aeson.withObject "Tavily result" $ \o -> do
        title <- o Aeson..: Key.fromText "title"
        url <- o Aeson..: Key.fromText "url"
        snippet <- fromMaybe "" <$> o Aeson..:? Key.fromText "content"
        pure (searchResult title url snippet)

parseBraveResults :: Aeson.Value -> AesonTypes.Parser [Aeson.Value]
parseBraveResults =
  Aeson.withObject "Brave search response" $ \o -> do
    web <- o Aeson..:? Key.fromText "web"
    case web of
      Nothing ->
        pure []
      Just webObject ->
        Aeson.withObject "Brave web results" parseWeb webObject
  where
    parseWeb o = do
      results <- fromMaybe [] <$> o Aeson..:? Key.fromText "results"
      traverse parseResult results

    parseResult =
      Aeson.withObject "Brave result" $ \o -> do
        title <- o Aeson..: Key.fromText "title"
        url <- o Aeson..: Key.fromText "url"
        snippet <- fromMaybe "" <$> o Aeson..:? Key.fromText "description"
        pure (searchResult title url snippet)

parseExaResults :: Aeson.Value -> AesonTypes.Parser [Aeson.Value]
parseExaResults =
  Aeson.withObject "Exa search response" $ \o -> do
    results <- o Aeson..: Key.fromText "results"
    traverse parseResult results
  where
    parseResult =
      Aeson.withObject "Exa result" $ \o -> do
        title <- fromMaybe "" <$> o Aeson..:? Key.fromText "title"
        url <- o Aeson..: Key.fromText "url"
        highlights <- fromMaybe [] <$> o Aeson..:? Key.fromText "highlights"
        pure (searchResult title url (Text.intercalate "\n" highlights))

searchResult :: Text -> Text -> Text -> Aeson.Value
searchResult title url snippet =
  Aeson.object
    [ "title" Aeson..= title
    , "url" Aeson..= url
    , "snippet" Aeson..= snippet
    ]

fetchWebPage :: (HTTP.HTTP :> es, IOE :> es) => Text -> Int -> Eff es Aeson.Value
fetchWebPage rawUrl maxContentTokens = do
  uri <- URI.mkURI rawUrl
  case useURI uri of
    Nothing ->
      throwIO (userError [i|Unsupported URL: #{rawUrl}. Use an http or https URL.|])
    Just (Left (url, options)) ->
      fetch url options
    Just (Right (url, options)) ->
      fetch url options
  where
    fetch :: (HTTP.HTTP :> es, IOE :> es) => Url scheme -> Option scheme -> Eff es Aeson.Value
    fetch url options = do
      response <- HTTP.runReq $
        req GET url NoReqBody bsResponse (options <> webRequestOptions)
      let contentType = TextEncoding.decodeUtf8With TextEncoding.lenientDecode <$> responseHeader response "Content-Type"
          body = Html.htmlToPlainText (decodeResponseBody (responseBody response))
          text = takeApproxTokens maxContentTokens body
      pure (Aeson.object
        [ "url" Aeson..= rawUrl
        , "status" Aeson..= responseStatusCode response
        , "content_type" Aeson..= contentType
        , "content" Aeson..= text
        , "truncated" Aeson..= (Text.length text < Text.length body)
        ])

takeApproxTokens :: Int -> Text -> Text
takeApproxTokens maxTokens =
  Text.take (maxTokens * 4)

webRequestOptions :: Option scheme
webRequestOptions =
  header "User-Agent" "cosmobot/0.1 (+https://github.com/ksqsf/cosmobot)"
    <> responseTimeout (15 * 1_000_000)

decodeResponseBody :: ByteString.ByteString -> Text
decodeResponseBody =
  TextEncoding.decodeUtf8With TextEncoding.lenientDecode

webArguments
  :: Text
  -> Text
  -> Text
  -> Int
  -> Int
  -> Maybe Integer
  -> Either Text (Text, Int)
webArguments textName rawText limitName limitCap defaultLimit requestedLimit
  | Text.null text =
      Left (textName <> " must not be empty.")
  | limit <= 0 =
      Left (limitName <> " must be positive.")
  | otherwise =
      Right (text, fromInteger (min (fromIntegral limitCap) limit))
  where
    text = Text.strip rawText
    limit = fromMaybe (fromIntegral defaultLimit) requestedLimit

argumentFailure :: Text -> ToolResult
argumentFailure err =
  toolFailure (permanentArgumentFailure err err)
