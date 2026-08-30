{-|
Module      : Bot.Agent.Tools.Web
Description : Agent web search and fetch tools
Stability   : experimental
-}

module Bot.Agent.Tools.Web
  ( webSearchTool
  , webFetchTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Tool
import Bot.Agent.Types
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Util.Html as Html
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import Network.HTTP.Req
import System.IO.Error (userError)
import qualified Text.URI as URI

webSearchTool :: HTTP.HTTP :> es => Tool (Eff es)
webSearchTool =
  noisy
  . allowWhen (.toolConfig.webSearchEnable)
  . withDescription "Search the web for current information. Returns title, url, and snippet results plus image_urls when the provider finds usable image URLs. If the user asks for a photo or image, set include_images=true, then pass returned image_urls to send_reply; do not claim an image was sent unless send_reply succeeds."
  $ tool "search_web"
      ( requiredText "query" "Search query."
      , optionalInteger "max_results" "Maximum number of results to return. Defaults to 5 and is capped at 20."
      , optionalBoolean "include_images" "Whether to request image URLs from the search provider. Defaults to true; use true for photo or image requests."
      )
      \rawQuery requestedMaxResults requestedImages -> do
        context <- askToolContext
        case webArguments "query" rawQuery "max_results" 20
          (fromMaybe 5 context.toolConfig.webSearchMaxResults)
          requestedMaxResults of
          Left err ->
            pure (argumentFailure err)
          Right (query, maxResults) -> do
            let searchConfig = context.toolConfig
            (results, imageUrls) <- webSearch searchConfig query maxResults (fromMaybe True requestedImages)
            pure (toolText (jsonText (Aeson.object
              [ "query" Aeson..= query
              , "source" Aeson..= webSearchSource searchConfig.webSearchApi
              , "results" Aeson..= results
              , "image_urls" Aeson..= imageUrls
              ])))

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
                pure (toolText [i|fetch_url use limit reached for this agent run: #{currentUses}.|])
              UseAllowed -> do
                page <- fetchWebPage url maxContentTokens
                pure (toolText (jsonText page))

webSearch :: HTTP.HTTP :> es => ToolConfig -> Text -> Int -> Bool -> Eff es ([Aeson.Value], [Text])
webSearch cfg query maxResults includeImages =
  case cfg.webSearchApi of
    WebSearchTavily ->
      case cfg.tavilyApiKey of
        Nothing -> throwIO (userError "Tavily search is not configured: set tool.web_search.tavily_api_key.")
        Just key -> tavilySearch key query maxResults includeImages
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
