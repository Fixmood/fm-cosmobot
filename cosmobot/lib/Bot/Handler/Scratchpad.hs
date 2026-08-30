{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Bot.Handler.Scratchpad
Description : Per-sender scratchpad todo commands
Stability   : experimental
-}

module Bot.Handler.Scratchpad
  ( scratchpadHandlers
  , renderTodoList
  )
where

import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Storage as Storage
import Bot.Core.Route
import Bot.Core.Message
import Bot.Prelude
import Bot.Storage.Prelude
import qualified Data.Int as Int
import qualified Data.Text as Text

scratchpadHandlers
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => [RouteHandler es]
scratchpadHandlers =
  [ scratchpadRoute "!todo" TodoCommand
  , scratchpadRoute "!done" DoneCommand
  , scratchpadRoute "!list" ListCommand
  , scratchpadRoute "!clear" ClearCommand
  , scratchpadRoute "!rm" RemoveCommand
  ]

data ScratchpadCommand
  = TodoCommand
  | DoneCommand
  | ListCommand
  | ClearCommand
  | RemoveCommand

data TodoItem = TodoItem
  { body :: !Text
  , done :: !Bool
  }
  deriving stock (Eq, Show)

data StoredTodo = StoredTodo
  { rowId :: !Integer
  , value :: !TodoItem
  }
  deriving (Eq, Show)

data TodoRow = TodoRow
  { id :: ID TodoRow
  , platform_key :: Text
  , sender_key :: Text
  , body :: Text
  , done :: Bool
  }
  deriving (Generic)

instance SqlRow TodoRow

todoRows :: Table TodoRow
todoRows =
  table "scratchpad_todos"
    [ #id :- autoPrimary
    , #platform_key :- index
    , #sender_key :- index
    ]

ensureTodoTable :: Storage.Storage :> es => Eff es ()
ensureTodoTable =
  runSelda (tryCreateTable todoRows)

scratchpadRoute
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> ScratchpadCommand
  -> RouteHandler es
scratchpadRoute commandText commandKind =
  withHelp (scratchpadHelp commandKind) $
    stopOn (command commandText) \message args ->
      handleScratchpadCommand commandKind message args

scratchpadHelp :: ScratchpadCommand -> RouteHelp
scratchpadHelp = \case
  TodoCommand -> RouteHelp "!todo [text]" "Add a todo, or list todos when text is omitted."
  DoneCommand -> RouteHelp "!done <id>" "Mark a todo as done."
  ListCommand -> RouteHelp "!list" "List todos."
  ClearCommand -> RouteHelp "!clear" "Clear all todos."
  RemoveCommand -> RouteHelp "!rm <id>..." "Remove todos by number."

handleScratchpadCommand
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => ScratchpadCommand
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleScratchpadCommand commandKind message args =
  case senderScope message of
    Nothing ->
      void $ Chat.replyTo message "无法识别发送者，不能保存 todo。"
    Just (platform, sender) ->
      case commandKind of
        TodoCommand -> handleTodo platform sender message args
        DoneCommand -> handleDone platform sender message args
        ListCommand -> replyWithList platform sender message
        ClearCommand -> handleClear platform sender message
        RemoveCommand -> handleRemove platform sender message args

handleTodo
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleTodo platformKey senderKey message args
  | Text.null body =
      replyWithList platformKey senderKey message
  | otherwise = do
      addTodoItem platformKey senderKey body
      todos <- loadTodoList platformKey senderKey
      void $ Chat.replyTo message ("已添加 #" <> show (length todos) <> ": " <> body)
  where
    body = Text.strip args

handleDone
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleDone platformKey senderKey message args =
  case parseSingleIndex args of
    Nothing ->
      void $ Chat.replyTo message "用法：!done <todo编号>"
    Just todoIndex -> do
      todos <- loadTodoList platformKey senderKey
      case todoAt todoIndex todos of
        Nothing ->
          void $ Chat.replyTo message [i|没有编号 #{todoIndex} 的 todo。|]
        Just todo -> do
          markTodoDone platformKey senderKey todo
          void $ Chat.replyTo message ("已完成 #" <> show todoIndex <> ": " <> todo.value.body)

handleClear
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Eff es ()
handleClear platformKey senderKey message = do
  clearTodoList platformKey senderKey
  void $ Chat.replyTo message "已清空 todo list。"

handleRemove
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Text
  -> Eff es ()
handleRemove platformKey senderKey message args =
  case parseIndices args of
    [] ->
      void $ Chat.replyTo message "用法：!rm <编号1> <编号2> ..."
    indices -> do
      todos <- loadTodoList platformKey senderKey
      let rows = mapMaybe (`todoAt` todos) indices
      deleteTodoRows platformKey senderKey (map (.rowId) rows)
      void $ Chat.replyTo message [i|已删除 #{length rows} 项。|]

replyWithList
  :: (Chat.Chat :> es, Storage.Storage :> es)
  => Text
  -> Text
  -> IncomingMessage
  -> Eff es ()
replyWithList platformKey senderKey message = do
  todos <- loadTodoList platformKey senderKey
  void $ Chat.replyTo message (renderTodoList todos)

renderTodoList :: [StoredTodo] -> Text
renderTodoList [] =
  "todo list 为空。"
renderTodoList todos =
  Text.unlines (zipWith render [1 :: Int ..] todos)
  where
    render todoIndex todo =
      "- [" <> status todo.value <> "] " <> show todoIndex <> ". " <> todo.value.body
    status todo
      | todo.done = "X"
      | otherwise = " "

loadTodoList :: Storage.Storage :> es => Text -> Text -> Eff es [StoredTodo]
loadTodoList platformKey senderKey = do
  ensureTodoTable
  rows <- runSelda do
    query do
      todo <- select todoRows
      restrict $
        todo ! #platform_key .== literal platformKey
          .&& todo ! #sender_key .== literal senderKey
      order (todo ! #id) ascending
      pure todo
  pure (map storedTodo rows)

addTodoItem :: Storage.Storage :> es => Text -> Text -> Text -> Eff es ()
addTodoItem platformKey senderKey body = do
  ensureTodoTable
  runSelda $
    insert_ todoRows
      [ TodoRow
          { id = def
          , platform_key = platformKey
          , sender_key = senderKey
          , body = body
          , done = False
          }
      ]

markTodoDone :: Storage.Storage :> es => Text -> Text -> StoredTodo -> Eff es ()
markTodoDone platformKey senderKey todo = do
  ensureTodoTable
  runSelda $
    update_ todoRows
      (ownedTodo platformKey senderKey todo.rowId)
      (`with` [#done $= const (literal True)])

deleteTodoRows :: Storage.Storage :> es => Text -> Text -> [Integer] -> Eff es ()
deleteTodoRows platformKey senderKey rowIds = do
  ensureTodoTable
  runSelda do
    for_ rowIds \rowId ->
      deleteFrom_ todoRows (ownedTodo platformKey senderKey rowId)

clearTodoList :: Storage.Storage :> es => Text -> Text -> Eff es ()
clearTodoList platformKey senderKey = do
  ensureTodoTable
  runSelda $
    deleteFrom_ todoRows \todo ->
      todo ! #platform_key .== literal platformKey
        .&& todo ! #sender_key .== literal senderKey

storedTodo :: TodoRow -> StoredTodo
storedTodo todo =
  StoredTodo
    { rowId = fromIntegral (fromId todo.id)
    , value = TodoItem{body = todo.body, done = todo.done}
    }

ownedTodo
  :: forall (backend :: Type).
     Text
  -> Text
  -> Integer
  -> Row backend TodoRow
  -> Col backend Bool
ownedTodo platformKey senderKey rowId todo =
  todo ! #id .== wantedId
    .&& todo ! #platform_key .== literal platformKey
    .&& todo ! #sender_key .== literal senderKey
  where
    wantedId :: Col backend (ID TodoRow)
    wantedId = literal (toId (fromIntegral rowId :: Int.Int64))

todoAt :: Int -> [StoredTodo] -> Maybe StoredTodo
todoAt todoIndex todos
  | todoIndex <= 0 = Nothing
  | otherwise = viaNonEmpty head (drop (todoIndex - 1) todos)

parseSingleIndex :: Text -> Maybe Int
parseSingleIndex args =
  case parseIndices args of
    [todoIndex] -> Just todoIndex
    _ -> Nothing

parseIndices :: Text -> [Int]
parseIndices =
  mapMaybe (readMaybe . toString) . Text.words

senderScope :: IncomingMessage -> Maybe (Text, Text)
senderScope message =
  (messagePlatformKey message,) <$> messageSenderKey message

messagePlatformKey :: IncomingMessage -> Text
messagePlatformKey message =
  show message.platform

messageSenderKey :: IncomingMessage -> Maybe Text
messageSenderKey message =
  case message.senderId of
    Just senderId ->
      Just ("id:" <> senderId)
    Nothing ->
      ("username:" <>) <$> message.senderUsername
