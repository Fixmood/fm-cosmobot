module Bot.Agent.Tools.MatrixAccess
  ( matrixAccessTool
  )
where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message (ChatPlatform (PlatformMatrix), IncomingMessage (..))
import qualified Bot.Effect.Matrix as Matrix
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory

matrixOverridePath :: FilePath
matrixOverridePath = "/data/matrix-room-overrides.txt"

matrixAccessTool :: (Matrix.Matrix :> es, IOE :> es) => Tool (Eff es)
matrixAccessTool =
  allowWhen superuserOnly
  . withDescription "Manage Matrix rooms from any platform. Only the owner can use this. Actions: list, view, open, close, leave. Use room for a Matrix room ID or room name. In a Matrix room, omit room to manage the current room. From QQ or another platform, specify room by ID or name. Leaving is destructive and requires confirm=true after the owner explicitly asks FM to leave that room."
  $ tool "matrix_access"
      (parsedArguments
        (objectSchema
          [ fieldText "action" "One of: list, view, open, close, leave."
          , fieldText "room" "Optional Matrix room ID or room name. Required outside Matrix except for list."
          , fieldBoolean "confirm" "Must be true when action is leave and the owner explicitly requested leaving the target room."
          ]
          ["action"])
        parseMatrixAccessRequest)
      runMatrixAccess

data MatrixAccessAction
  = MatrixAccessList
  | MatrixAccessView
  | MatrixAccessOpen
  | MatrixAccessClose
  | MatrixAccessLeave

data MatrixAccessRequest = MatrixAccessRequest
  { action :: !MatrixAccessAction
  , room :: !(Maybe Text)
  , confirm :: !Bool
  }

parseMatrixAccessRequest :: Aeson.Value -> AesonTypes.Parser MatrixAccessRequest
parseMatrixAccessRequest = Aeson.withObject "matrix_access arguments" $ \object -> do
  actionText <- Text.toLower . Text.strip <$> object Aeson..: Key.fromText "action"
  action <- case actionText of
    "list" -> pure MatrixAccessList
    "view" -> pure MatrixAccessView
    "open" -> pure MatrixAccessOpen
    "close" -> pure MatrixAccessClose
    "leave" -> pure MatrixAccessLeave
    _ -> fail "action must be one of: list, view, open, close, leave"
  room <- fmap Text.strip <$> object Aeson..:? Key.fromText "room"
  confirm <- object Aeson..:? Key.fromText "confirm" Aeson..!= False
  pure MatrixAccessRequest{action, room, confirm}

runMatrixAccess request = do
  context <- askToolContext
  overrides <- liftIO readOverrides
  rooms <- joinedMatrixRooms
  case request.action of
    MatrixAccessList ->
      pure (toolText (renderRoomList rooms overrides))
    action ->
      case resolveRequestedRoom context request.room rooms of
        Left reason -> pure (toolText reason)
        Right target ->
          case action of
            MatrixAccessView ->
              pure (toolText (renderNamedStatus target overrides))
            MatrixAccessOpen -> do
              liftIO (writeOverrides (updateOverride target.roomId True overrides))
              pure (toolText ("已开放 Matrix 房间：" <> renderRoom target))
            MatrixAccessClose -> do
              liftIO (writeOverrides (updateOverride target.roomId False overrides))
              pure (toolText ("已关闭 Matrix 房间：" <> renderRoom target))
            MatrixAccessLeave
              | not request.confirm ->
                  pure (toolText ("退出房间需要明确确认。请确认是否让 FM 退出：" <> renderRoom target))
              | otherwise -> do
                  leaveResult <- leaveMatrixRoom target.roomId
                  case leaveResult of
                    Left reason ->
                      pure (toolText ("退出 Matrix 房间失败：" <> reason))
                    Right () -> do
                      liftIO (writeOverrides (removeOverride target.roomId overrides))
                      pure (toolText ("FM 已退出 Matrix 房间：" <> renderRoom target))
            MatrixAccessList ->
              pure (toolText (renderRoomList rooms overrides))

data MatrixRoom = MatrixRoom
  { roomId :: !Text
  , roomName :: !(Maybe Text)
  }

joinedMatrixRooms :: Matrix.Matrix :> es => Eff es [MatrixRoom]
joinedMatrixRooms = do
  result <- trySync (Matrix.matrixClientCall Matrix.MatrixClientRequest
    { method = Matrix.MatrixGet
    , path = ["_matrix", "client", "v3", "joined_rooms"]
    , query = []
    , body = Nothing
    })
  case result of
    Left (_ :: SomeException) -> pure []
    Right value ->
      forM (parseJoinedRoomIds value) $ \roomId -> do
        roomName <- fetchRoomName roomId
        pure MatrixRoom{roomId, roomName}

parseJoinedRoomIds :: Aeson.Value -> [Text]
parseJoinedRoomIds =
  fromMaybe [] . AesonTypes.parseMaybe (Aeson.withObject "joined rooms" (Aeson..: "joined_rooms"))

fetchRoomName :: Matrix.Matrix :> es => Text -> Eff es (Maybe Text)
fetchRoomName roomId = do
  result <- trySync (Matrix.matrixClientCall Matrix.MatrixClientRequest
    { method = Matrix.MatrixGet
    , path = ["_matrix", "client", "v3", "rooms", roomId, "state", "m.room.name"]
    , query = []
    , body = Nothing
    })
  pure $ case result of
    Left (_ :: SomeException) -> Nothing
    Right value -> AesonTypes.parseMaybe (Aeson.withObject "room name" (Aeson..: "name")) value

leaveMatrixRoom :: Matrix.Matrix :> es => Text -> Eff es (Either Text ())
leaveMatrixRoom roomId = do
  result <- trySync (Matrix.matrixClientCall Matrix.MatrixClientRequest
    { method = Matrix.MatrixPost
    , path = ["_matrix", "client", "v3", "rooms", roomId, "leave"]
    , query = []
    , body = Just (Aeson.object [])
    })
  pure $ case result of
    Left (err :: SomeException) -> Left (toText (displayException err))
    Right _ -> Right ()

resolveRequestedRoom :: Context -> Maybe Text -> [MatrixRoom] -> Either Text MatrixRoom
resolveRequestedRoom context requested rooms =
  case nonEmptyTarget requested <|> currentMatrixRoom context of
    Nothing -> Left "请指定 Matrix 房间名称或 room_id；也可以先让我列出 Matrix 房间。"
    Just target
      | "!" `Text.isPrefixOf` target ->
          Right (fromMaybe (MatrixRoom target Nothing) (List.find ((== target) . (.roomId)) rooms))
      | otherwise ->
          case filter (roomMatches target) rooms of
            [] -> Left ("没有找到 Matrix 房间：“" <> target <> "”。请先让我列出 Matrix 房间。")
            [matchedRoom] -> Right matchedRoom
            matches -> Left ("找到多个同名 Matrix 房间，请改用 room_id：\n" <> Text.unlines (renderRoom <$> matches))

nonEmptyTarget :: Maybe Text -> Maybe Text
nonEmptyTarget = (>>= \target -> if Text.null target then Nothing else Just target)

currentMatrixRoom :: Context -> Maybe Text
currentMatrixRoom context
  | context.message.platform /= PlatformMatrix = Nothing
  | otherwise = do
      roomId <- viaNonEmpty head context.message.chatAliases
      guard ("!" `Text.isPrefixOf` roomId)
      pure roomId

roomMatches :: Text -> MatrixRoom -> Bool
roomMatches query room =
  let normalized = Text.toCaseFold . Text.strip
      needle = normalized query
  in needle == normalized room.roomId
      || maybe False ((== needle) . normalized) room.roomName

renderRoom :: MatrixRoom -> Text
renderRoom room =
  maybe room.roomId (\name -> name <> "（" <> room.roomId <> "）") room.roomName

renderRoomList :: [MatrixRoom] -> [(Text, Override)] -> Text
renderRoomList rooms overrides
  | null rooms = "暂时无法读取 FM 已加入的 Matrix 房间。"
  | otherwise = Text.unlines
      ("FM 已加入的 Matrix 房间：" : fmap render rooms)
  where
    render room = "- " <> statusLabel room.roomId overrides <> " " <> renderRoom room

renderNamedStatus :: MatrixRoom -> [(Text, Override)] -> Text
renderNamedStatus room overrides =
  statusLabel room.roomId overrides <> "：" <> renderRoom room

statusLabel :: Text -> [(Text, Override)] -> Text
statusLabel room overrides =
  case List.lookup room overrides of
    Just OverrideOpen -> "已开放"
    Just OverrideClosed -> "已关闭"
    Nothing -> "使用默认白名单"

data Override = OverrideOpen | OverrideClosed
  deriving (Eq)

readOverrides :: IO [(Text, Override)]
readOverrides = do
  exists <- Directory.doesFileExist matrixOverridePath
  if not exists
    then pure []
    else parseOverrides <$> Text.lines <$> TextIO.readFile matrixOverridePath

parseOverrides :: [Text] -> [(Text, Override)]
parseOverrides = mapMaybe \line ->
  case Text.uncons (Text.strip line) of
    Just ('+', room) | not (Text.null room) -> Just (room, OverrideOpen)
    Just ('-', room) | not (Text.null room) -> Just (room, OverrideClosed)
    _ -> Nothing

updateOverride :: Text -> Bool -> [(Text, Override)] -> [(Text, Override)]
updateOverride room open overrides =
  (room, if open then OverrideOpen else OverrideClosed)
    : filter ((/= room) . fst) overrides

removeOverride :: Text -> [(Text, Override)] -> [(Text, Override)]
removeOverride room =
  filter ((/= room) . fst)

writeOverrides :: [(Text, Override)] -> IO ()
writeOverrides overrides =
  TextIO.writeFile matrixOverridePath $ Text.unlines
    [ (if status == OverrideOpen then "+" else "-") <> room
    | (room, status) <- overrides
    ]
