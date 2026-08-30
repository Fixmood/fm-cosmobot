{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Typing
Description : Typing website commands
Stability   : experimental
-}

module Bot.Handler.Typing
  ( typingHandlers
  )
where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Concurrency as Concurrency
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Typst as Typst
import Bot.Core.Route
import Bot.Handler.Typing.Typst (typstDocument)
import qualified Bot.Util.Html as Html
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Core.ReplyBody as ReplyBody
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time
import Network.HTTP.Req
import System.IO.Error (userError)
import Text.Printf

championshipRankCommand :: Text
championshipRankCommand = "fm jb"

speedCupRankCommand :: Text
speedCupRankCommand = "fm jsb"

tigerRankCommand :: Text
tigerRankCommand = "fm hb"

-- | Routes that render typing leaderboard snapshots.
typingHandlers
  :: (Chat.Chat :> es, HTTP.HTTP :> es)
  => [RouteHandler es]
typingHandlers =
  [ rankRoute championshipRankCommand "锦标赛"
  , rankRoute speedCupRankCommand "极速杯"
  , rankRoute tigerRankCommand "虎杯"
  ]

rankRoute
  :: (Chat.Chat :> es, HTTP.HTTP :> es)
  => Text
  -> Text
  -> RouteHandler es
rankRoute commandText source =
  withHelp (RouteHelp commandText ("Render the " <> source <> " leaderboard.")) $
  stopOn (matching (\message -> Text.strip message.text == commandText)) \message _ -> do
    sendUnifiedRankImage source message

sendUnifiedRankImage
  :: (Chat.Chat :> es, HTTP.HTTP :> es)
  => Text
  -> IncomingMessage
  -> Eff es ()
sendUnifiedRankImage source message = do
  response <- HTTP.runReq $
    req GET (http "172.20.0.4" /: "competition" /: "live") NoReqBody jsonResponse
      ( "source" =: source
        <> "group_id" =: maybe ("" :: Text) show message.chatId
        <> "soft" =: True
        <> port 8077
      )
  case AesonTypes.parseMaybe parseRankStatus (responseBody response :: Aeson.Value) of
    Just ("ok", actualDate, _) -> do
      let imageUrl = "http://172.20.0.4:8077/reports/live-competition.png?source="
            <> case source of
              "锦标赛" -> "champ"
              "极速杯" -> "comp"
              _ -> "tiger"
            <> "&date=" <> actualDate <> "&combined=1"
      sent <- Chat.replyTo (standaloneMessage message) (ReplyBody.imageDirective imageUrl)
      when (null (rights sent)) $
        void $ Chat.replyTo (standaloneMessage message) (source <> "排行榜已生成，但图片发送失败。")
    Just (_, _, errorMessage) ->
      void $ Chat.replyTo (standaloneMessage message) (source <> "排行榜获取失败：" <> errorMessage)
    Nothing ->
      void $ Chat.replyTo (standaloneMessage message) (source <> "排行榜获取失败：领域服务返回格式不正确。")
  where
    parseRankStatus :: Aeson.Value -> AesonTypes.Parser (Text, Text, Text)
    parseRankStatus = Aeson.withObject "FM live leaderboard" \o ->
      (,,)
        <$> o Aeson..:? "status" Aeson..!= "error"
        <*> o Aeson..:? "date" Aeson..!= ""
        <*> o Aeson..:? "message" Aeson..!= "公开赛事网站当前没有返回可用数据。"

standaloneMessage :: IncomingMessage -> IncomingMessage
standaloneMessage message =
  message
    { messageId = Nothing
    , replyToMessageId = Nothing
    , raw = Aeson.Null
    }

sendRankImage
  :: (Chat.Chat :> es, Typst.Typst :> es, KatipE :> es, IOE :> es)
  => Text
  -> Text
  -> Eff es [[Text]]
  -> IncomingMessage
  -> Eff es ()
sendRankImage titleSuffix failureMessage fetchRows message =
  handleError do
    logInfo [i|Fetching typing rank rows: #{titleSuffix}|]
    title <- liftIO (rankTitle titleSuffix)
    rows <- fetchRows
    logInfo [i|Fetched typing rank rows: #{title}, #{length rows} rows|]
    Typst.withTypstPng (typstDocument title rows) \imagePath -> do
      logInfo [i|Rendered typing rank image: #{imagePath}|]
      sent <- Chat.replyTo message (ReplyBody.imageDirective ("file://" <> Text.pack imagePath))
      logInfo [i|Sent typing rank image: #{show sent :: Text}|]
      when (null (rights sent)) do
        void $ Chat.replyTo message [i|#{title}已生成，但图片发送失败。|]
  where
    handleError action =
      action `catchSync` \err -> do
        logWarning [i|Failed to render typing rank: #{show err :: String}|]
        void $ Chat.replyTo message failureMessage

rankTitle :: Text -> IO Text
rankTitle suffix = do
  date <- currentDateText
  pure (date <> suffix)

currentDateText :: IO Text
currentDateText = do
  now <- getCurrentTime
  let tz = hoursToTimeZone 8
      day = localDay (utcToLocalTime tz now)
  pure (Text.pack (formatTime defaultTimeLocale "%F" day))

fetchChampionshipRows :: (HTTP.HTTP :> es, IOE :> es) => Eff es [[Text]]
fetchChampionshipRows = do
  html <- fetchChampionshipPage
  table <- maybe (throwIO (userError "championship rank table not found")) pure (extractRankTable html)
  let rows = rankRows table
  case rows of
    [] -> throwIO (userError "championship rank table is empty")
    _  -> pure rows

fetchChampionshipPage :: HTTP.HTTP :> es => Eff es Text
fetchChampionshipPage = do
  body <- HTTP.runReq $
    responseBody <$> req GET url NoReqBody bsResponse mempty
  pure (TextEncoding.decodeUtf8Lenient body)
  where
    -- The site's TLS endpoint fails with Haskell's TLS stack because it does
    -- not support Extended Main Secret. The same page is available over HTTP.
    url = http "www.jsxiaoshi.com" /: "championships_rank.html"

fetchTigerRows :: (HTTP.HTTP :> es, IOE :> es) => Eff es [[Text]]
fetchTigerRows = do
  date <- liftIO currentDateText
  value <- HTTP.runReq $
    responseBody <$> req GET (tigerLeaderboardUrl date) NoReqBody jsonResponse ("limit" =: (50 :: Int))
  maybe (throwIO (userError "tiger leaderboard response had unexpected shape")) pure
    (AesonTypes.parseMaybe tigerRows value)

tigerLeaderboardUrl :: Text -> Url 'Https
tigerLeaderboardUrl date =
  https "race.tiger-code.com" /: "api" /: "leaderboard" /: "date" /: date

tigerRows :: Aeson.Value -> AesonTypes.Parser [[Text]]
tigerRows =
  Aeson.withObject "TigerLeaderboardResponse" $ \o -> do
    data_ <- o Aeson..: "data"
    leaderboard <- data_ Aeson..: "leaderboard"
    body <- traverse tigerRow leaderboard
    pure (tigerHeader : body)

tigerHeader :: [Text]
tigerHeader =
  ["排名", "用户名", "VIP", "速度", "击键", "码长", "打词率", "时间", "键准", "输入法"]

tigerRow :: Aeson.Value -> AesonTypes.Parser [Text]
tigerRow =
  Aeson.withObject "TigerLeaderboardRow" $ \o -> do
    rank <- o Aeson..:? "rank" :: AesonTypes.Parser (Maybe Int)
    username <- o Aeson..:? "username" :: AesonTypes.Parser (Maybe Text)
    vipLevel <- o Aeson..:? "vip_level" :: AesonTypes.Parser (Maybe Int)
    speed <- o Aeson..:? "speed" :: AesonTypes.Parser (Maybe Double)
    hitRate <- o Aeson..:? "hit_rate" :: AesonTypes.Parser (Maybe Double)
    kpw <- o Aeson..:? "kpw" :: AesonTypes.Parser (Maybe Double)
    wordRatio <- o Aeson..:? "word_ratio" :: AesonTypes.Parser (Maybe Double)
    time <- (o Aeson..:? "total_time" <|> o Aeson..:? "time") :: AesonTypes.Parser (Maybe Double)
    accuracy <- o Aeson..:? "accuracy" :: AesonTypes.Parser (Maybe Double)
    inputMethod <- o Aeson..:? "input_method" :: AesonTypes.Parser (Maybe Text)
    pure
      [ maybeText rank
      , fromMaybe "-" username
      , maybeText vipLevel
      , maybeNumber speed
      , maybeNumber hitRate
      , maybeNumber kpw
      , maybePercent ((* 100) <$> wordRatio)
      , maybe "-" formatSeconds time
      , maybePercent accuracy
      , fromMaybe "-" inputMethod
      ]

extractRankTable :: Text -> Maybe Text
extractRankTable html = do
  let (beforeId, fromId) = Text.breakOn "id=\"sdph\"" html
  guard (not (Text.null fromId))
  tablePrefix <- viaNonEmpty last (Text.splitOn "<table" beforeId)
  let tableStart = "<table" <> tablePrefix <> fromId
      (table, rest) = Text.breakOn "</table>" tableStart
  guard (not (Text.null rest))
  pure (table <> "</table>")

rankRows :: Text -> [[Text]]
rankRows table =
  case parsedRows of
    [] -> []
    heading : body ->
      let keep = keepIndexes heading
          heading' = select keep heading
      in heading' : map (normalizeRow (length heading') . select keep) body
  where
    parsedRows =
      filter (not . null)
        $ map parseRow
        $ drop 1
        $ Text.splitOn "<tr" table

keepIndexes :: [Text] -> [Int]
keepIndexes heading =
  [ index
  | (index, name) <- zip [0 :: Int ..] heading
  , name `notElem` ["头像", "操作"]
  ]

select :: [Int] -> [Text] -> [Text]
select indexes cells =
  mapMaybe (`safeIndex` cells) indexes

safeIndex :: Int -> [a] -> Maybe a
safeIndex index values =
  viaNonEmpty head (drop index values)

normalizeRow :: Int -> [Text] -> [Text]
normalizeRow size row =
  take size (row <> repeat "")

parseRow :: Text -> [Text]
parseRow row =
  parseCells row []

parseCells :: Text -> [Text] -> [Text]
parseCells input acc =
  case Text.breakOn "<td" input of
    (_, "") ->
      reverse acc
    (_, rest) ->
      let (tag, afterTag) = Text.breakOn ">" rest
          contentStart = Text.drop 1 afterTag
          (content, afterCell) = Text.breakOn "</td>" contentStart
          next = Text.drop (Text.length "</td>") afterCell
      in if "hidden" `Text.isInfixOf` tag
        then parseCells next acc
        else parseCells next (cleanCell content : acc)

cleanCell :: Text -> Text
cleanCell =
  Text.unwords
    . Text.words
    . Html.htmlDecode
    . Html.stripHtmlTags

maybeText :: Show a => Maybe a -> Text
maybeText =
  maybe "-" (Text.pack . show)

maybeNumber :: Maybe Double -> Text
maybeNumber =
  maybe "-" (Text.pack . printf "%.2f")

maybePercent :: Maybe Double -> Text
maybePercent =
  maybe "-" (Text.pack . printf "%.2f%%")

formatSeconds :: Double -> Text
formatSeconds seconds =
  Text.pack (printf "%02d:%02d" minutes restSeconds)
  where
    totalSeconds = max 0 (round seconds :: Int)
    minutes = totalSeconds `div` 60
    restSeconds = totalSeconds `mod` 60
