{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Safebooru
Description : Safebooru image command handler
Stability   : experimental
-}

module Bot.Handler.Safebooru
  ( BallRequest (..)
  , safebooruHandlers
  , safebooruHandlersWith
  , parseBallRequest
  )
where

import Bot.Core.Message
import qualified Bot.Core.ReplyBody as ReplyBody
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Char as Char
import qualified Data.Int as Int
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Req
import System.IO.Error (userError)

ballCommand :: Text
ballCommand =
  "!ball"

maxRequestedImages :: Int
maxRequestedImages =
  5

storedImageLimit :: Int
storedImageLimit =
  20

data BallRequest = BallRequest
  { keyword    :: !Text
  , imageCount :: !Int
  }
  deriving (Eq, Show)

data SafebooruLinkRow = SafebooruLinkRow
  { id      :: ID SafebooruLinkRow
  , keyword :: Text
  , link    :: Text
  }
  deriving (Generic)

instance SqlRow SafebooruLinkRow

data StoredSafebooruLink = StoredSafebooruLink
  { rowId :: !Integer
  , link  :: !Text
  }

type SafebooruSearch es = Text -> Eff es [Text]

safebooruLinkRows :: Table SafebooruLinkRow
safebooruLinkRows =
  table "safebooru_image_links"
    [ #id :- autoPrimary
    , #keyword :- index
    ]

-- | Routes for public Safebooru image commands.
safebooruHandlers
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, HTTP.HTTP :> es, Storage.Storage :> es, KatipE :> es, IOE :> es, Fail :> es)
  => [RouteHandler es]
safebooruHandlers =
  safebooruHandlersWith (searchSafebooruImageLinks)

safebooruHandlersWith
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, KatipE :> es, IOE :> es)
  => SafebooruSearch es
  -> [RouteHandler es]
safebooruHandlersWith search =
  [ safebooruRoute search
  ]

safebooruRoute
  :: (Chat.Chat :> es, Concurrency.Concurrency :> es, Storage.Storage :> es, KatipE :> es, IOE :> es)
  => SafebooruSearch es
  -> RouteHandler es
safebooruRoute search =
  withHelp (RouteHelp "!ball <keyword> [1-5]" "Find random Safebooru images.") $
  stopOn ballCommandArgs \message args -> do
    case parseBallRequest args of
      Left err ->
        void $ Chat.replyTo message err
      Right request -> do
        logInfo [i|matched safebooru route: #{incomingMessageLogLine message}|]
        Concurrency.fire "safebooru.search" (sendSafebooruImages search message request)

ballCommandArgs :: MessageFilter Text
ballCommandArgs =
  MessageFilter \message -> do
    rest <- Text.stripPrefix ballCommand message.text
    case Text.uncons rest of
      Nothing ->
        Just ""
      Just (firstChar, _) | Char.isSpace firstChar ->
        Just (Text.strip rest)
      _ ->
        Nothing

parseBallRequest :: Text -> Either Text BallRequest
parseBallRequest rawArgs =
  case Text.words rawArgs of
    [keyword] ->
      Right BallRequest
        { keyword = normalizeKeyword keyword
        , imageCount = 1
        }
    [keyword, countText] -> do
      imageCount <- parseImageCount countText
      Right BallRequest
        { keyword = normalizeKeyword keyword
        , imageCount
        }
    _ ->
      Left ballUsage

parseImageCount :: Text -> Either Text Int
parseImageCount countText =
  case readMaybe (toString countText) of
    Just requested | requested >= 1 && requested <= maxRequestedImages ->
      Right requested
    _ ->
      Left ballCountError

ballUsage :: Text
ballUsage =
  "用法：!ball <keyword> [num]，num 为 1 到 5。"

ballCountError :: Text
ballCountError =
  "num 必须是 1 到 5。"

sendSafebooruImages
  :: (Chat.Chat :> es, Storage.Storage :> es, KatipE :> es, IOE :> es)
  => SafebooruSearch es
  -> IncomingMessage
  -> BallRequest
  -> Eff es ()
sendSafebooruImages search message request =
  handleError do
    links <- drawSafebooruLinks search request.keyword request.imageCount
    if null links
      then void $ Chat.replyTo message "没有找到匹配的 Safebooru 图片。"
      else void $ Chat.replyTo message (renderImageLinks links)
  where
    handleError action =
      action `catchSync` \err -> do
        logError [i|Safebooru search failed: #{show err :: String}|]
        void $ Chat.replyTo message "Safebooru 搜索失败。"

drawSafebooruLinks
  :: (Storage.Storage :> es, IOE :> es)
  => SafebooruSearch es
  -> Text
  -> Int
  -> Eff es [Text]
drawSafebooruLinks search keyword requestedCount = do
  storedLinks <- loadStoredLinks keyword
  when (length storedLinks < requestedCount) do
    freshLinks <- take storedImageLimit <$> search keyword
    replaceStoredLinks keyword (take storedImageLimit (uniqueTexts (map (.link) storedLinks <> freshLinks)))
  drawStoredLinks keyword requestedCount

ensureSafebooruTable :: Storage.Storage :> es => Eff es ()
ensureSafebooruTable =
  runSelda (tryCreateTable safebooruLinkRows)

loadStoredLinks :: Storage.Storage :> es => Text -> Eff es [StoredSafebooruLink]
loadStoredLinks keyword = do
  ensureSafebooruTable
  rows <- runSelda do
    query do
      row <- select safebooruLinkRows
      restrict (row ! #keyword .== literal keyword)
      order (row ! #id) ascending
      pure row
  pure (map storedSafebooruLink rows)

replaceStoredLinks :: Storage.Storage :> es => Text -> [Text] -> Eff es ()
replaceStoredLinks keyword links = do
  ensureSafebooruTable
  runSelda do
    deleteFrom_ safebooruLinkRows \row ->
      row ! #keyword .== literal keyword
    unless (null links) $
      insert_ safebooruLinkRows
        [ SafebooruLinkRow
            { id = def
            , keyword = keyword
            , link = imglink
            }
        | imglink <- links
        ]

drawStoredLinks :: (Storage.Storage :> es, IOE :> es) => Text -> Int -> Eff es [Text]
drawStoredLinks keyword requestedCount = do
  storedLinks <- loadStoredLinks keyword
  seed <- liftIO currentSeed
  let (selected, _) = selectRandomLinks requestedCount seed storedLinks
  deleteStoredLinks (map (.rowId) selected)
  pure (map (.link) selected)

deleteStoredLinks :: Storage.Storage :> es => [Integer] -> Eff es ()
deleteStoredLinks rowIds = do
  ensureSafebooruTable
  runSelda do
    for_ rowIds \rowId ->
      deleteFrom_ safebooruLinkRows (linkRowId rowId)

storedSafebooruLink :: SafebooruLinkRow -> StoredSafebooruLink
storedSafebooruLink row =
  StoredSafebooruLink
    { rowId = fromIntegral (fromId row.id)
    , link = row.link
    }

linkRowId
  :: forall (backend :: Type).
     Integer
  -> Row backend SafebooruLinkRow
  -> Col backend Bool
linkRowId rowId row =
  row ! #id .== wantedId
  where
    wantedId :: Col backend (ID SafebooruLinkRow)
    wantedId = literal (toId (fromIntegral rowId :: Int.Int64))

selectRandomLinks :: Int -> Integer -> [StoredSafebooruLink] -> ([StoredSafebooruLink], [StoredSafebooruLink])
selectRandomLinks requestedCount seed links =
  go requestedCount seed [] links
  where
    go remainingCount seed_ selected available
      | remainingCount <= 0 || null available =
          (reverse selected, available)
      | otherwise =
          let nextSeed_ = nextRandomSeed seed_
              selectedIndex = fromInteger (nextSeed_ `mod` toInteger (length available))
          in case removeAt selectedIndex available of
            Nothing ->
              (reverse selected, available)
            Just (picked, rest) ->
              go (remainingCount - 1) nextSeed_ (picked : selected) rest

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt selectedIndex values =
  case splitAt selectedIndex values of
    (before, picked : after) ->
      Just (picked, before <> after)
    _ ->
      Nothing

nextRandomSeed :: Integer -> Integer
nextRandomSeed seed =
  (seed * 1103515245 + 12345) `mod` 2147483648

currentSeed :: IO Integer
currentSeed =
  round . (* 1000000) <$> getPOSIXTime

uniqueTexts :: [Text] -> [Text]
uniqueTexts =
  reverse . snd . foldl' step (Set.empty, [])
  where
    step (seen, retained) imglink
      | imglink `Set.member` seen = (seen, retained)
      | otherwise                 = (Set.insert imglink seen, imglink : retained)

normalizeKeyword :: Text -> Text
normalizeKeyword =
  Text.toLower . Text.strip

renderImageLinks :: [Text] -> Text
renderImageLinks =
  Text.unlines . fmap ReplyBody.imageDirective

searchSafebooruImageLinks :: (Fail :> es, HTTP.HTTP :> es) => Text -> Eff es [Text]
searchSafebooruImageLinks keyword = do
  value <- HTTP.runReq $
    responseBody <$> req GET safebooruUrl NoReqBody jsonResponse (safebooruOptions keyword)
  either (throwIO . userError) pure $
    AesonTypes.parseEither parseSafebooruImageLinks value

safebooruUrl :: Url 'Https
safebooruUrl =
  https "safebooru.org" /: "index.php"

safebooruOptions :: Text -> Option 'Https
safebooruOptions keyword =
  "page" =: ("dapi" :: Text)
    <> "s" =: ("post" :: Text)
    <> "q" =: ("index" :: Text)
    <> "json" =: (1 :: Int)
    <> "limit" =: storedImageLimit
    <> "tags" =: keyword
    <> header "User-Agent" (TextEncoding.encodeUtf8 safebooruUserAgent)
    <> responseTimeout (15 * 1000000)

safebooruUserAgent :: Text
safebooruUserAgent =
  "cosmobot/0.1 (+https://github.com/ksqsf/cosmobot)"

parseSafebooruImageLinks :: Aeson.Value -> AesonTypes.Parser [Text]
parseSafebooruImageLinks =
  Aeson.withArray "Safebooru posts" \posts ->
    catMaybes <$> traverse parseSafebooruPostLink (toList posts)

parseSafebooruPostLink :: Aeson.Value -> AesonTypes.Parser (Maybe Text)
parseSafebooruPostLink =
  Aeson.withObject "Safebooru post" \o -> do
    fileUrl <- o Aeson..:? "file_url"
    sampleUrl <- o Aeson..:? "sample_url"
    previewUrl <- o Aeson..:? "preview_url"
    pure (fileUrl <|> sampleUrl <|> previewUrl)
