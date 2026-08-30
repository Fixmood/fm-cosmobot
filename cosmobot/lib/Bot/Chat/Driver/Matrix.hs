{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Matrix
Description : Matrix Client-Server chat driver
Stability   : experimental
-}

module Bot.Chat.Driver.Matrix
  ( MatrixDriver
  , newMatrixDriver
  , runMatrixClient
  , matrixClientJsonCall
  , chatHandler
  , Config (..)
  , SyncResponse (..)
  , JoinedRoom (..)
  , Timeline (..)
  , Event (..)
  , EventContent (..)
  , SendMessageResponse (..)
  , RoomEvent (..)
  , incomingMessages
  , eventToIncomingMessage
  , eventToIncomingMessageWith
  , syncInvitedRoomIds
  , matrixReferencedMessage
  , decryptMatrixEncryptedBytesForTest
  , formatMatrixMarkdown
  , formatMatrixMarkdownWithMentionNames
  )
where

import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Chat.Bridge.FM as FMBridge
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Matrix as Matrix
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Mime as Mime
import qualified Bot.Storage.Matrix as MatrixStorage
import Bot.Util.Aeson
import qualified Bot.Effect.Chat as Chat
import Bot.Core.Message
import Bot.Prelude
import qualified Bot.Effect.HTTP as HTTP
import Commonmark
import Commonmark.Extensions
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Crypto.Cipher.AES as CryptoAES
import qualified Crypto.Cipher.Types as CryptoCipher
import qualified Crypto.Error as CryptoError
import qualified Crypto.Hash as CryptoHash
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString as StrictByteString
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy as LazyText
import qualified Effectful.Concurrent.MVar as MVar
import qualified Effectful.Prim.IORef as IORef
import GHC.Clock (getMonotonicTimeNSec)
import qualified Network.HTTP.Client as Client
import Network.HTTP.Req
import qualified Network.HTTP.Types.Header as HTTPHeader
import qualified Network.HTTP.Types.Status as HTTPStatus
import qualified Streaming as S
import qualified Data.ByteString.Streaming.HTTP as StreamingHTTP
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as SP
import qualified Streaming.Prelude as S
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.Temporary as Temporary
import qualified System.Directory as Directory
import System.FilePath ((</>), (<.>), takeFileName)
import System.IO.Error (ioError, userError)
import qualified Text.URI as URI

data Config = Config
  { homeserver :: !Text
  , loginUser :: !(Maybe Text)
  , loginPassword :: !(Maybe Text)
  , deviceId :: !(Maybe Text)
  , directRooms :: ![Text]
  , userId :: !(Maybe Text)
  , allowedRooms :: ![Text]
  , superusers :: ![Text]
  }
  deriving (Show)

newtype MatrixRoomId = MatrixRoomId Text
  deriving (Show, Eq, Ord)

newtype MatrixEventId = MatrixEventId Text
  deriving (Show, Eq, Ord)
    deriving (Aeson.ToJSON, Aeson.FromJSON) via Text

newtype MatrixReplyTo = MatrixReplyTo MatrixEventId
  deriving (Show, Eq)

matrixRoomIdText :: MatrixRoomId -> Text
matrixRoomIdText (MatrixRoomId roomId) =
  roomId

matrixEventIdText :: MatrixEventId -> Text
matrixEventIdText (MatrixEventId eventId) =
  eventId

matrixRoomId :: Text -> MatrixRoomId
matrixRoomId =
  MatrixRoomId

matrixEventId :: Text -> MatrixEventId
matrixEventId =
  MatrixEventId

matrixEventMessageId :: MatrixEventId -> MessageId
matrixEventMessageId =
  textMessageId . matrixEventIdText

instance IsString MatrixRoomId where
  fromString =
    matrixRoomId . Text.pack

instance IsString MatrixEventId where
  fromString =
    matrixEventId . Text.pack

data MatrixDriver = MatrixDriver
  { config :: !Config
  , auth :: !MatrixAuth
  , eventIds :: !(IORef.IORef (Map MessageId MatrixEventId))
  , streamTextMessages :: !(IORef.IORef (Map MatrixEventId MatrixEditMessageRequest))
  , directRoomIds :: !(IORef.IORef (Set MatrixRoomId))
  , joinedMemberCounts :: !(IORef.IORef (Map MatrixRoomId Int))
  }

newMatrixDriver
  :: (Concurrent :> es, Prim :> es)
  => Config
  -> Eff es MatrixDriver
newMatrixDriver cfg = do
  eventIds <- IORef.newIORef Map.empty
  streamTextMessages <- IORef.newIORef Map.empty
  directRoomIdsRef <- IORef.newIORef (Set.fromList (matrixRoomId <$> cfg.directRooms))
  joinedMemberCountsRef <- IORef.newIORef Map.empty
  authState <- IORef.newIORef (initialMatrixAuthState cfg)
  refreshLock <- MVar.newMVar ()
  let auth = MatrixAuth cfg authState refreshLock
  pure MatrixDriver
    { config = cfg
    , auth
    , eventIds
    , streamTextMessages
    , directRoomIds = directRoomIdsRef
    , joinedMemberCounts = joinedMemberCountsRef
    }

instance Driver.ChatDriver MatrixDriver where
  type ChatDriverEffects MatrixDriver es =
    (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es, Storage.Storage :> es)

  driverPlatform _ =
    PlatformMatrix

  sendReplyMessage =
    replyToMatrix

  sendReplyMessages =
    replyToMatrixMessages

  sendStreamingReplyMessage =
    streamingReplyToMatrix

  replyAudio =
    replyAudioMatrix

  uploadFile =
    uploadFileMatrix

  editMessage =
    editMessageMatrix

  completeMessageEdit =
    completeMessageEditMatrix

  deleteMessage =
    deleteMessageMatrix

  messageOutPolicy _ _ =
    pure (Chat.EditableMessage matrixEditChunkChars matrixStreamingMessageLimit)

  getMessageContent =
    getMessageContentMatrix

  getSenderMemberInfo =
    getSenderMemberInfoMatrix

  getMemberInfo =
    getMemberInfoMatrix

  getUserAvatar =
    getUserAvatarMatrix

  listGroupMembers =
    listGroupMembersMatrix

  normalizeMediaRef driver ref =
    normalizeMatrixMediaRef driver Nothing ref

  mentionUser =
    mentionUserMatrix

  setTyping driver message timeoutMs =
    case viaNonEmpty head message.chatAliases of
      Just roomId -> typing driver (matrixRoomId roomId) timeoutMs
      Nothing -> pure ()

chatHandler
  :: Driver.ChatDriverEffects MatrixDriver es
  => MatrixDriver
  -> Chat.ChatHandler es
chatHandler =
  Chat.chatDriverHandler

runMatrixClient
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Maybe MatrixDriver
  -> Eff (Matrix.Matrix : es) a
  -> Eff es a
runMatrixClient driver =
  interpret \_ -> \case
    Matrix.MatrixClientCall request ->
      maybe
        (liftIO (ioError (userError "Matrix driver is not configured.")))
        (`matrixClientRequest` request)
        driver

matrixClientRequest
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Matrix.MatrixClientRequest
  -> Eff es Aeson.Value
matrixClientRequest driver Matrix.MatrixClientRequest{method, path, query, body} =
  case (method, body) of
    (Matrix.MatrixGet, Nothing) -> matrixClientJsonCall driver "client request" "client request" requestOptions GET requestUrl NoReqBody
    (Matrix.MatrixDelete, Nothing) -> matrixClientJsonCall driver "client request" "client request" requestOptions DELETE requestUrl NoReqBody
    (Matrix.MatrixPost, Just value) -> matrixClientJsonCall driver "client request" "client request" requestOptions POST requestUrl (ReqBodyJson value)
    (Matrix.MatrixPut, Just value) -> matrixClientJsonCall driver "client request" "client request" requestOptions PUT requestUrl (ReqBodyJson value)
    _ -> liftIO (ioError (userError "Invalid Matrix client request body."))
  where
    requestUrl :: forall scheme. Url scheme -> Url scheme
    requestUrl baseUrl = List.foldl' (/:) baseUrl path
    requestOptions :: forall scheme. Option scheme -> Option scheme
    requestOptions options = List.foldl' (\current (optionKey, optionValue) -> current <> (optionKey =: optionValue)) options query

matrixStreamingMessageLimit :: Int
matrixStreamingMessageLimit = 4000

matrixEditChunkChars :: Int
matrixEditChunkChars = 128

loadSyncToken :: Storage.Storage :> es => Eff es (Maybe Text)
loadSyncToken =
  MatrixStorage.loadSyncToken

storeSyncToken :: Storage.Storage :> es => Text -> Eff es ()
storeSyncToken =
  MatrixStorage.saveSyncToken

sync :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> Maybe Text -> Eff es (Maybe SyncResponse)
sync driver since = do
  response <- call driver MatrixSync
    { syncSince = since
    , syncMode = matrixSyncMode since
    }
  acceptInvitations driver response
  rememberMatrixRoomState driver.directRoomIds driver.joinedMemberCounts response
  pure (Just response)

matrixSyncMode :: Maybe Text -> MatrixSyncMode
matrixSyncMode = \case
  Nothing ->
    MatrixInitialSync
  Just{} ->
    MatrixLongPollSync

directRooms :: Prim :> es => MatrixDriver -> Eff es (Set MatrixRoomId)
directRooms driver =
  IORef.readIORef driver.directRoomIds

joinedMemberCounts :: Prim :> es => MatrixDriver -> Eff es (Map MatrixRoomId Int)
joinedMemberCounts driver =
  IORef.readIORef driver.joinedMemberCounts

joinedMemberCount :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> Eff es (Maybe Int)
joinedMemberCount driver roomId = do
  maybeCall driver (MatrixJoinedMembers roomId) >>= \case
    Nothing ->
      pure Nothing
    Just response -> do
      let count = Map.size response.joinedMembers
      rememberJoinedMemberCount driver.directRoomIds driver.joinedMemberCounts roomId count
      pure (Just count)

sendText :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> Maybe MatrixReplyTo -> Text -> Eff es (Either Text SendMessageResponse)
sendText driver roomId replyToEventId body =
  sendTextWithMentionsStreamComplete driver roomId replyToEventId body [] True

sendTextWithMentions :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> Maybe MatrixReplyTo -> Text -> [Text] -> Eff es (Either Text SendMessageResponse)
sendTextWithMentions driver roomId replyToEventId body mentionUserIds =
  sendTextWithMentionsStreamComplete driver roomId replyToEventId body mentionUserIds True

sendTextWithMentionsStreamComplete :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> Maybe MatrixReplyTo -> Text -> [Text] -> Bool -> Eff es (Either Text SendMessageResponse)
sendTextWithMentionsStreamComplete driver roomId replyToEventId body mentionUserIds complete = do
  let mentions = matrixOutgoingMentionUserIds body mentionUserIds
  mentionNames <- fetchMatrixMentionNames driver roomId mentions
  response <- eitherCall "send m.room.message" driver (MatrixSendMessage roomId replyToEventId body mentions mentionNames complete)
  traverse_ (rememberMatrixEvent driver.eventIds) response
  traverse_ (\sent -> rememberInitialStreamTextMessage driver.streamTextMessages sent.eventId body mentions mentionNames complete) response
  pure response

uploadMedia :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> FilePath -> Text -> Text -> Eff es MatrixUploadResponse
uploadMedia driver path fileName mime =
  call driver (MatrixUploadMedia path fileName mime)

sendFileMessage :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> Text -> Maybe MatrixReplyTo -> MatrixFileMessage -> Eff es (Either Text SendMessageResponse)
sendFileMessage driver roomId replyRelation message@MatrixFileMessage{msgtype = mediaMsgtype} = do
  response <- eitherCall [i|send #{mediaMsgtype}|] driver (MatrixSendFile roomId replyRelation message)
  traverse_ (rememberMatrixEvent driver.eventIds) response
  pure response

editText :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> MatrixEventId -> Text -> Eff es (Either Text SendMessageResponse)
editText driver roomId eventId body = do
  let mentions = matrixOutgoingMentionUserIds body []
  mentionNames <- fetchMatrixMentionNames driver roomId mentions
  let request = matrixEditMessageRequest eventId body mentions mentionNames False
  response <- sendMatrixTextEdit driver roomId request
  traverse_ (rememberMatrixEvent driver.eventIds) response
  traverse_ (const (rememberStreamTextMessage driver.streamTextMessages eventId request)) response
  pure response

matrixEditMessageRequest :: MatrixEventId -> Text -> [Text] -> Map Text Text -> Bool -> MatrixEditMessageRequest
matrixEditMessageRequest eventId body mentions mentionNames complete =
  MatrixEditMessageRequest
    { body = nonEmptyMatrixBody displayBody
    , formattedBody = formatMatrixMarkdownWithMentionNames mentionNames body
    , mentions = MatrixMentions mentions
    , replacesEventId = eventId
    , streamMetadata = MatrixStreamMetadata complete
    }
  where
    displayBody = matrixMentionDisplayBody mentionNames body

sendMatrixTextEdit :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> MatrixEditMessageRequest -> Eff es (Either Text SendMessageResponse)
sendMatrixTextEdit driver roomId request =
  eitherCall "edit m.room.message" driver (MatrixEditMessage roomId request)

deleteEvent :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> Text -> MessageId -> Maybe MatrixEventId -> Eff es Bool
deleteEvent driver roomId messageId knownEventId = do
  stored <- IORef.readIORef driver.eventIds
  case knownEventId <|> Map.lookup messageId stored of
    Nothing ->
      pure False
    Just eventId ->
      isJust <$> maybeCall driver (MatrixRedactEvent roomId eventId)

fetchEvent :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> MatrixEventId -> Eff es (Maybe Event)
fetchEvent driver roomId eventId =
  maybeCall driver (MatrixFetchEvent roomId eventId)

fetchMember :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> Text -> Eff es (Maybe MatrixMember)
fetchMember driver roomId userId =
  maybeCall driver (MatrixFetchMember roomId userId)

fetchProfile :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> Text -> Eff es (Maybe MatrixProfile)
fetchProfile driver userId =
  maybeCall driver (MatrixFetchProfile userId)

fetchJoinedMembers :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> Eff es (Maybe JoinedMembersResponse)
fetchJoinedMembers driver roomId =
  maybeCall driver (MatrixJoinedMembers roomId)

downloadMedia :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> Text -> Eff es (Maybe MatrixDownloadedMedia)
downloadMedia driver mxcRef =
  maybeCall driver (MatrixDownloadMedia mxcRef)

typing :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixRoomId -> Int -> Eff es ()
typing driver roomId timeoutMs =
  case driver.config.userId of
    Just userId ->
      void $ maybeCall driver (MatrixSetTyping roomId userId timeoutMs)
    Nothing ->
      logWarning [i|Matrix typing notification skipped: bot_id is not configured.|]

rememberMatrixEvent :: Prim :> es => IORef.IORef (Map MessageId MatrixEventId) -> SendMessageResponse -> Eff es ()
rememberMatrixEvent eventIds response =
  IORef.modifyIORef' eventIds (Map.insert (matrixEventMessageId response.eventId) response.eventId)

rememberStreamTextMessage :: Prim :> es => IORef.IORef (Map MatrixEventId MatrixEditMessageRequest) -> MatrixEventId -> MatrixEditMessageRequest -> Eff es ()
rememberStreamTextMessage streamTextMessages eventId request =
  IORef.modifyIORef' streamTextMessages (Map.insert eventId request)

rememberInitialStreamTextMessage :: Prim :> es => IORef.IORef (Map MatrixEventId MatrixEditMessageRequest) -> MatrixEventId -> Text -> [Text] -> Map Text Text -> Bool -> Eff es ()
rememberInitialStreamTextMessage streamTextMessages eventId body mentions mentionNames complete
  | complete =
      pure ()
  | otherwise =
      rememberStreamTextMessage streamTextMessages eventId (matrixEditMessageRequest eventId body mentions mentionNames False)

popStreamTextMessage :: Prim :> es => IORef.IORef (Map MatrixEventId MatrixEditMessageRequest) -> MatrixEventId -> Eff es (Maybe MatrixEditMessageRequest)
popStreamTextMessage streamTextMessages eventId =
  IORef.atomicModifyIORef' streamTextMessages \messages ->
    (Map.delete eventId messages, Map.lookup eventId messages)

rememberMatrixRoomState
  :: Prim :> es
  => IORef.IORef (Set MatrixRoomId)
  -> IORef.IORef (Map MatrixRoomId Int)
  -> SyncResponse
  -> Eff es ()
rememberMatrixRoomState directRoomIdsRef joinedMemberCountsRef response = do
  IORef.modifyIORef' directRoomIdsRef (<> syncDirectRoomIds response)
  for_ (syncJoinedMemberCounts response) \(roomId, count) ->
    rememberJoinedMemberCount directRoomIdsRef joinedMemberCountsRef roomId count

rememberJoinedMemberCount
  :: Prim :> es
  => IORef.IORef (Set MatrixRoomId)
  -> IORef.IORef (Map MatrixRoomId Int)
  -> MatrixRoomId
  -> Int
  -> Eff es ()
rememberJoinedMemberCount directRoomIdsRef joinedMemberCountsRef roomId count = do
  IORef.modifyIORef' joinedMemberCountsRef (Map.insert roomId count)
  IORef.modifyIORef' directRoomIdsRef \directRoomIds ->
    if count == 2
      then Set.insert roomId directRoomIds
      else Set.delete roomId directRoomIds

data MatrixAuth = MatrixAuth
  { authConfig :: !Config
  , authState :: !(IORef.IORef MatrixAuthState)
  , authRefreshLock :: !(MVar.MVar ())
  }

data MatrixAuthState = MatrixAuthState
  { authAccessToken :: !(Maybe Text)
  , authRefreshToken :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (PrefixedSnakeJSON "auth" MatrixAuthState)

data MatrixDownloadedMedia = MatrixDownloadedMedia
  { downloadedBytes :: !(Q.ByteStream (ResourceT IO) ())
  , downloadedMimeType :: !Text
  , downloadedName :: !(Maybe Text)
  }

data MatrixMediaRef = MatrixMediaRef
  { matrixMediaRefUrl :: !Text
  , matrixMediaRefMimeType :: !(Maybe Text)
  , matrixMediaRefEncrypted :: !(Maybe MatrixEncryptedFile)
  }

data MatrixEncryptedFile = MatrixEncryptedFile
  { encryptedFileUrl :: !Text
  , encryptedFileKey :: !Text
  , encryptedFileIv :: !Text
  , encryptedFileSha256 :: !Text
  }

data MatrixDecryptionPlan = MatrixDecryptionPlan
  { decryptionCipher :: !CryptoAES.AES256
  , decryptionIv :: !(CryptoCipher.IV CryptoAES.AES256)
  , decryptionExpectedHash :: !(CryptoHash.Digest CryptoHash.SHA256)
  }

data MatrixSync = MatrixSync
  { syncSince :: Maybe Text
  , syncMode :: !MatrixSyncMode
  }

data MatrixSyncMode
  = MatrixInitialSync
  | MatrixLongPollSync

newtype MatrixJoinedMembers = MatrixJoinedMembers
  { joinedMembersRoomId :: MatrixRoomId
  }

data MatrixSendMessage = MatrixSendMessage
  { sendMessageRoomId :: !MatrixRoomId
  , sendMessageReplyTo :: !(Maybe MatrixReplyTo)
  , sendMessageBody :: !Text
  , sendMessageMentions :: ![Text]
  , sendMessageMentionNames :: !(Map Text Text)
  , sendMessageStreamComplete :: !Bool
  }

data MatrixUploadMedia = MatrixUploadMedia
  { uploadMediaPath :: !FilePath
  , uploadMediaFileName :: !Text
  , uploadMediaMime :: !Text
  }

data MatrixSendFile = MatrixSendFile
  { sendFileRoomId :: !Text
  , sendFileReplyTo :: !(Maybe MatrixReplyTo)
  , sendFileMessage :: !MatrixFileMessage
  }

data MatrixEditMessage = MatrixEditMessage
  { editMessageRoomId :: !MatrixRoomId
  , editMessageRequest :: !MatrixEditMessageRequest
  }

data MatrixCompleteStreamMessage = MatrixCompleteStreamMessage
  { completeStreamMessageRoomId :: !MatrixRoomId
  , completeStreamMessageRequest :: !MatrixEditMessageRequest
  }

data MatrixRedactEvent = MatrixRedactEvent
  { redactRoomId :: !Text
  , redactEventId :: !MatrixEventId
  }

data MatrixFetchEvent = MatrixFetchEvent
  { fetchEventRoomId :: !MatrixRoomId
  , fetchEventId :: !MatrixEventId
  }

data MatrixFetchMember = MatrixFetchMember
  { fetchMemberRoomId :: !MatrixRoomId
  , fetchMemberUserId :: !Text
  }

newtype MatrixFetchProfile = MatrixFetchProfile
  { fetchProfileUserId :: Text
  }

newtype MatrixDownloadMedia = MatrixDownloadMedia
  { downloadMediaRef :: Text
  }

data MatrixSetTyping = MatrixSetTyping
  { typingRoomId :: !MatrixRoomId
  , typingUserId :: !Text
  , typingTimeoutMs :: !Int
  }

newtype MatrixJoinRoom = MatrixJoinRoom
  { joinRoomId :: MatrixRoomId
  }

class MatrixAPI request where
  type MatrixResponse request

  call
    :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
    => MatrixDriver
    -> request
    -> Eff es (MatrixResponse request)

instance MatrixAPI MatrixSync where
  type MatrixResponse MatrixSync = SyncResponse

  call driver request@MatrixSync{syncSince} = do
    let sinceLabel :: Text
        sinceLabel = maybe "<initial>" (const "<next_batch>") syncSince
    matrixClientJsonCall driver "sync" [i|sync since=#{sinceLabel}|] (matrixSyncOptions request)
      GET
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "sync")
      NoReqBody

instance MatrixAPI MatrixJoinedMembers where
  type MatrixResponse MatrixJoinedMembers = JoinedMembersResponse

  call driver MatrixJoinedMembers{joinedMembersRoomId} =
    matrixClientJsonCall driver "joined_members" [i|joined_members room=#{joinedMembersRoomId}|] matrixApiOptions
      GET
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText joinedMembersRoomId /: "joined_members")
      NoReqBody

instance MatrixAPI MatrixJoinRoom where
  type MatrixResponse MatrixJoinRoom = Aeson.Value

  call driver MatrixJoinRoom{joinRoomId} =
    matrixClientJsonCall driver "join room" [i|join room=#{joinRoomId}|] matrixApiOptions
      POST
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText joinRoomId /: "join")
      (ReqBodyJson (Aeson.object []))

instance MatrixAPI MatrixSendMessage where
  type MatrixResponse MatrixSendMessage = SendMessageResponse

  call driver MatrixSendMessage{sendMessageRoomId, sendMessageReplyTo, sendMessageBody, sendMessageMentions, sendMessageMentionNames, sendMessageStreamComplete} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    let displayBody = matrixMentionDisplayBody sendMessageMentionNames sendMessageBody
        request = SendMessageRequest
          { msgtype = "m.text"
          , body = nonEmptyMatrixBody displayBody
          , formattedBody = formatMatrixMarkdownWithMentionNames sendMessageMentionNames sendMessageBody
          , replyRelation = sendMessageReplyTo
          , mentions = MatrixMentions sendMessageMentions
          , streamMetadata = MatrixStreamMetadata sendMessageStreamComplete
          }
    matrixClientJsonCall driver "send m.room.message" [i|send m.room.message room=#{matrixRoomIdText sendMessageRoomId}|] matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText sendMessageRoomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson request)

instance MatrixAPI MatrixUploadMedia where
  type MatrixResponse MatrixUploadMedia = MatrixUploadResponse

  call driver MatrixUploadMedia{uploadMediaPath, uploadMediaFileName, uploadMediaMime} =
    matrixClientJsonCall driver "upload media" "upload media" (matrixUploadOptions uploadMediaFileName uploadMediaMime)
      POST
      (\baseUrl -> baseUrl /: "_matrix" /: "media" /: "v3" /: "upload")
      (ReqBodyFile uploadMediaPath)

instance MatrixAPI MatrixSendFile where
  type MatrixResponse MatrixSendFile = SendMessageResponse

  call driver MatrixSendFile{sendFileRoomId, sendFileReplyTo, sendFileMessage = fileMessage} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    let mediaMsgtype = fileMessage.msgtype
        request = MatrixFileMessageRequest
          { message = fileMessage
          , replyRelation = sendFileReplyTo
          , streamMetadata = MatrixStreamMetadata True
          }
    matrixClientJsonCall driver [i|send #{mediaMsgtype}|] [i|send #{mediaMsgtype}|] matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: sendFileRoomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson request)

instance MatrixAPI MatrixEditMessage where
  type MatrixResponse MatrixEditMessage = SendMessageResponse

  call driver MatrixEditMessage{editMessageRoomId, editMessageRequest} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    matrixClientJsonCall driver "edit m.room.message" "edit m.room.message" matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText editMessageRoomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson editMessageRequest)

instance MatrixAPI MatrixCompleteStreamMessage where
  type MatrixResponse MatrixCompleteStreamMessage = SendMessageResponse

  call driver MatrixCompleteStreamMessage{completeStreamMessageRoomId, completeStreamMessageRequest} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    matrixClientJsonCall driver "complete stream m.room.message" "complete stream m.room.message" matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText completeStreamMessageRoomId /: "send" /: "m.room.message" /: txnId)
      (ReqBodyJson completeStreamMessageRequest)

instance MatrixAPI MatrixRedactEvent where
  type MatrixResponse MatrixRedactEvent = Aeson.Value

  call driver MatrixRedactEvent{redactRoomId, redactEventId} = do
    txnId <- liftIO (show <$> getMonotonicTimeNSec)
    let request = RedactEventRequest{reason = Nothing}
    matrixClientJsonCall driver "redact event" "redact event" matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: redactRoomId /: "redact" /: matrixEventIdText redactEventId /: txnId)
      (ReqBodyJson request)

instance MatrixAPI MatrixFetchEvent where
  type MatrixResponse MatrixFetchEvent = Event

  call driver MatrixFetchEvent{fetchEventRoomId, fetchEventId} =
    matrixClientJsonCall driver "room event" [i|room event room=#{fetchEventRoomId}|] matrixApiOptions
      GET
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText fetchEventRoomId /: "event" /: matrixEventIdText fetchEventId)
      NoReqBody

instance MatrixAPI MatrixFetchMember where
  type MatrixResponse MatrixFetchMember = MatrixMember

  call driver MatrixFetchMember{fetchMemberRoomId, fetchMemberUserId} = do
    content :: MatrixMemberContent <-
      matrixClientJsonCall driver "room member" [i|room member room=#{fetchMemberRoomId}|] matrixApiOptions
        GET
        (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText fetchMemberRoomId /: "state" /: "m.room.member" /: fetchMemberUserId)
        NoReqBody
    pure (matrixMemberFromContent fetchMemberUserId content)

instance MatrixAPI MatrixFetchProfile where
  type MatrixResponse MatrixFetchProfile = MatrixProfile

  call driver MatrixFetchProfile{fetchProfileUserId} = do
    profile :: MatrixProfileContent <-
      matrixClientJsonCall driver "profile" "profile" matrixApiOptions
        GET
        (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "profile" /: fetchProfileUserId)
        NoReqBody
    pure MatrixProfile
      { profileUserId = fetchProfileUserId
      , profileDisplayName = profile.profileContentDisplayName
      , profileAvatarUrl = profile.profileContentAvatarUrl
      }

instance MatrixAPI MatrixDownloadMedia where
  type MatrixResponse MatrixDownloadMedia = MatrixDownloadedMedia

  call driver MatrixDownloadMedia{downloadMediaRef} =
    case parseMxcUri downloadMediaRef of
      Nothing ->
        liftIO (ioError (userError [i|Invalid Matrix media URI: #{downloadMediaRef}|]))
      Just (serverName, mediaId) -> do
        logInfo [i|Matrix API request: authenticated media download #{downloadMediaRef}|]
        withMatrixAccessToken driver.auth \token -> do
          manager <- HTTP.manager
          request <- liftIO (matrixMediaDownloadRequest driver.config token serverName mediaId)
          downloadedMimeType <- matrixReq "authenticated media download" (probeMatrixMediaDownload request manager)
          pure MatrixDownloadedMedia
            { downloadedBytes = matrixResponseByteStream request manager
            , downloadedMimeType
            , downloadedName = Just mediaId
            }

instance MatrixAPI MatrixSetTyping where
  type MatrixResponse MatrixSetTyping = Aeson.Value

  call driver MatrixSetTyping{typingRoomId, typingUserId, typingTimeoutMs} = do
    let request = SetTypingRequest typingTimeoutMs
    matrixClientJsonCall driver "set typing" [i|set typing room=#{matrixRoomIdText typingRoomId} user=#{typingUserId}|] matrixApiOptions
      PUT
      (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "rooms" /: matrixRoomIdText typingRoomId /: "typing" /: typingUserId)
      (ReqBodyJson request)

initialMatrixAuthState :: Config -> MatrixAuthState
initialMatrixAuthState _cfg =
  -- It's unnecessary to log in here. /sync should handle it.
  MatrixAuthState{authAccessToken = Nothing, authRefreshToken = Nothing}

matrixAuthAvailable :: Prim :> es => MatrixDriver -> Eff es Bool
matrixAuthAvailable driver = do
  currentAuthState <- IORef.readIORef driver.auth.authState
  pure (isJust currentAuthState.authAccessToken || isJust currentAuthState.authRefreshToken)

maybeCall
  :: (MatrixAPI request, HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> request
  -> Eff es (Maybe (MatrixResponse request))
maybeCall driver request = do
  available <- matrixAuthAvailable driver
  if available
    then Just <$> call driver request
    else pure Nothing

eitherCall
  :: (MatrixAPI request, HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Text
  -> MatrixDriver
  -> request
  -> Eff es (Either Text (MatrixResponse request))
eitherCall label driver request = do
  available <- matrixAuthAvailable driver
  if available
    then first (matrixUserFacingExceptionText label) <$> trySync (call driver request)
    else pure (Left [i|Matrix #{label} requires a configured access token, refresh token, or login credentials.|])

withMatrixAccessToken
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> (Text -> Eff es a)
  -> Eff es a
withMatrixAccessToken auth action = do
  currentAuthState <- IORef.readIORef auth.authState
  token <- case currentAuthState.authAccessToken of
    Just accessToken ->
      pure accessToken
    Nothing ->
      refreshMatrixAccessToken auth ""
  action token `catch` \(err :: MatrixApiException) ->
    if matrixAuthTokenRejected err
      then do
        refreshed <- refreshMatrixAccessToken auth token
        action refreshed
      else
        throwIO err

refreshMatrixAccessToken
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> Text
  -> Eff es Text
refreshMatrixAccessToken auth expiredToken =
  MVar.withMVar auth.authRefreshLock \_ -> do
    currentAuthState <- IORef.readIORef auth.authState
    case currentAuthState.authAccessToken of
      Just token | token /= expiredToken ->
        pure token
      _ ->
        case currentAuthState.authRefreshToken of
          Nothing ->
            reloginMatrixAccessToken auth
          Just refreshToken -> do
            logInfo "Matrix access token expired; refreshing"
            let refreshWithToken = do
                  response <- refreshMatrixToken auth.authConfig refreshToken
                  let refreshedState = MatrixAuthState
                        { authAccessToken = Just response.refreshedAccessToken
                        , authRefreshToken = response.refreshedRefreshToken <|> currentAuthState.authRefreshToken
                        }
                  IORef.writeIORef auth.authState refreshedState
                  pure response.refreshedAccessToken
            refreshWithToken `catchSync` \err -> do
              logWarning [i|Matrix refresh token failed; logging in again: #{displayException err}|]
              reloginMatrixAccessToken auth

reloginMatrixAccessToken
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> Eff es Text
reloginMatrixAccessToken auth =
  case (auth.authConfig.loginUser, auth.authConfig.loginPassword) of
    (Just user, Just password) -> do
      logInfo [i|Matrix logging in again; attempts=#{matrixReloginAttempts}|]
      retryingMatrixLogin auth user password
    _ ->
      throwIO (userError "Matrix access token expired and no refresh token or login credentials are configured.")

retryingMatrixLogin
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixAuth
  -> Text
  -> Text
  -> Eff es Text
retryingMatrixLogin auth user password =
  attemptLogin 1
  where
    attemptLogin attempt =
      loginOnce `catchSync` \err ->
        if attempt < matrixReloginAttempts
          then do
            logWarning [i|Matrix relogin attempt #{attempt}/#{matrixReloginAttempts} failed: #{displayException err}; retrying in #{matrixReloginDelaySeconds} seconds|]
            threadDelay matrixReloginDelayMicroseconds
            attemptLogin (attempt + 1)
          else do
            logError [i|Matrix relogin attempt #{attempt}/#{matrixReloginAttempts} failed: #{displayException err}|]
            throwIO err

    loginOnce = do
      response <- matrixLogin auth.authConfig user password
      storeMatrixLoginResponse auth response
      pure response.loginAccessToken

storeMatrixLoginResponse :: Prim :> es => MatrixAuth -> MatrixLoginResponse -> Eff es ()
storeMatrixLoginResponse auth response =
  IORef.writeIORef auth.authState MatrixAuthState
    { authAccessToken = Just response.loginAccessToken
    , authRefreshToken = response.loginRefreshToken
    }

matrixAuthTokenRejected :: MatrixApiException -> Bool
matrixAuthTokenRejected = \case
  MatrixApiException _ status err ->
    HTTPStatus.statusCode status == 401 && err.errcode `elem` ["M_UNKNOWN_TOKEN", "M_FORBIDDEN"]
  MatrixTransportException{} ->
    False

data MatrixApiException
  = MatrixApiException !Text !HTTPStatus.Status !MatrixErrorResponse
  | MatrixTransportException !Text !Text
  deriving (Show, Eq)

instance Exception MatrixApiException where
  displayException =
    Text.unpack . matrixApiExceptionMessage

matrixApiExceptionMessage :: MatrixApiException -> Text
matrixApiExceptionMessage = \case
  MatrixApiException method status err ->
    [i|Matrix API request failed (#{method}): HTTP #{HTTPStatus.statusCode status} #{matrixErrorResponseText err}|]
  MatrixTransportException method message ->
    [i|Matrix API request failed (#{method}): #{message}|]

matrixUserFacingExceptionText :: Text -> SomeException -> Text
matrixUserFacingExceptionText label err =
  case fromException err of
    Just matrixErr ->
      matrixUserFacingApiError label matrixErr
    Nothing ->
      [i|Matrix #{label} failed: #{displayException err}|]

matrixUserFacingApiError :: Text -> MatrixApiException -> Text
matrixUserFacingApiError label = \case
  MatrixApiException _ status err
    | matrixAuthTokenRejected (MatrixApiException label status err) ->
        [i|Matrix #{label} failed: Matrix authentication failed after retrying login.|]
    | otherwise ->
        [i|Matrix #{label} failed: HTTP #{HTTPStatus.statusCode status} #{matrixErrorResponseText err}|]
  MatrixTransportException{} ->
    [i|Matrix #{label} failed: Matrix transport error.|]

matrixErrorResponseText :: MatrixErrorResponse -> Text
matrixErrorResponseText err =
  Text.intercalate "; " $
    [ err.errcode <> maybe "" (": " <>) err.matrixError
    ]
      <> maybe [] (\retry -> [[i|retry_after_ms=#{retry}|]]) err.retryAfterMs
      <> if err.softLogout then ["soft_logout=true"] else []

matrixApiException :: Text -> HttpException -> MatrixApiException
matrixApiException method = \case
  VanillaHttpException (Client.HttpExceptionRequest _ (Client.StatusCodeException response body)) ->
    case Aeson.eitherDecodeStrict body of
      Right err ->
        MatrixApiException method (Client.responseStatus response) err
      Left parseErr ->
        MatrixTransportException method [i|HTTP #{HTTPStatus.statusCode (Client.responseStatus response)} with non-Matrix error body: #{parseErr}|]
  VanillaHttpException (Client.HttpExceptionRequest _ content) ->
    MatrixTransportException method [i|HTTP transport error: #{show content :: String}|]
  VanillaHttpException err ->
    MatrixTransportException method [i|HTTP error: #{show err :: String}|]
  JsonHttpException message ->
    MatrixTransportException method [i|JSON error: #{message}|]

matrixReq :: IOE :> es => Text -> Eff es a -> Eff es a
matrixReq method action =
  action `catch` \(err :: HttpException) ->
    throwIO (matrixApiException method err)

matrixUnauthenticatedCall
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es)
  => Config
  -> Text
  -> Text
  -> (forall scheme. Option scheme -> Option scheme)
  -> (forall scheme. Url scheme -> Option scheme -> Req response)
  -> Eff es response
matrixUnauthenticatedCall cfg method logMessage addOptions buildRequest = do
  logInfo [i|Matrix API request: #{logMessage}|]
  withMatrixBaseUrl cfg.homeserver \baseUrl baseOptions ->
    matrixReq method $
      HTTP.runReqWithConfig matrixHttpConfig $
        buildRequest baseUrl (addOptions baseOptions)

matrixUnauthenticatedJsonCall
  :: ( HTTP.HTTP :> es
     , IOE :> es
     , KatipE :> es
     , Aeson.FromJSON response
     , HttpMethod method
     , HttpBody body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     )
  => Config
  -> Text
  -> Text
  -> (forall scheme. Option scheme -> Option scheme)
  -> method
  -> (forall scheme. Url scheme -> Url scheme)
  -> body
  -> Eff es response
matrixUnauthenticatedJsonCall cfg method logMessage addOptions httpMethod buildUrl body =
  responseBody <$> matrixUnauthenticatedCall cfg method logMessage addOptions \baseUrl options ->
    req httpMethod (buildUrl baseUrl) body jsonResponse options

matrixClientJsonCall
  :: ( HTTP.HTTP :> es
     , IOE :> es
     , KatipE :> es
     , Concurrent :> es
     , Prim :> es
     , Aeson.FromJSON response
     , HttpMethod method
     , HttpBody body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
     )
  => MatrixDriver
  -> Text
  -> Text
  -> (forall scheme. Option scheme -> Option scheme)
  -> method
  -> (forall scheme. Url scheme -> Url scheme)
  -> body
  -> Eff es response
matrixClientJsonCall driver method logMessage addOptions httpMethod buildUrl body =
  withMatrixAccessToken driver.auth \token ->
    matrixUnauthenticatedJsonCall driver.config method logMessage
      (\baseOptions -> matrixApiOptions (addOptions (baseOptions <> matrixAuth token)))
      httpMethod buildUrl body

matrixApiOptions :: Option scheme -> Option scheme
matrixApiOptions baseOptions =
  baseOptions
    <> responseTimeout matrixApiResponseTimeoutMicroseconds

matrixUploadOptions :: Text -> Text -> Option scheme -> Option scheme
matrixUploadOptions fileName mime baseOptions =
  baseOptions
    <> header "Content-Type" (TextEncoding.encodeUtf8 mime)
    <> "filename" =: fileName

matrixApiOptionsNoAuth :: Option scheme -> Option scheme
matrixApiOptionsNoAuth baseOptions =
  baseOptions
    <> responseTimeout matrixApiResponseTimeoutMicroseconds

matrixMediaDownloadRequest :: Config -> Text -> Text -> Text -> IO Client.Request
matrixMediaDownloadRequest cfg token serverName mediaId = do
  request <- Client.parseRequest (Text.unpack (matrixEndpointText cfg.homeserver ["_matrix", "client", "v1", "media", "download", serverName, mediaId]))
  pure request
    { Client.requestHeaders =
        ( "Authorization"
        , ByteString.pack [i|Bearer #{token}|]
        ) : Client.requestHeaders request
    , Client.responseTimeout = Client.responseTimeoutMicro matrixMediaDownloadResponseTimeoutMicroseconds
    }

matrixResponseByteStream :: Client.Request -> Client.Manager -> Q.ByteStream (ResourceT IO) ()
matrixResponseByteStream request manager = do
  response <- lift (StreamingHTTP.http request manager)
  Client.responseBody response

probeMatrixMediaDownload :: IOE :> es => Client.Request -> Client.Manager -> Eff es Text
probeMatrixMediaDownload request manager =
  bracket
    (liftIO $ Client.responseOpen (matrixMediaProbeRequest request) manager)
    (liftIO . Client.responseClose)
    \response -> do
      let status = Client.responseStatus response
      if HTTPStatus.statusIsSuccessful status
        then do
          void $ liftIO $ Client.brRead (Client.responseBody response)
          pure (matrixResponseMime response)
        else do
          body <- liftIO $ Client.brRead (Client.responseBody response)
          throwIO (matrixDownloadStatusException status body)

matrixResponseMime :: Client.Response body -> Text
matrixResponseMime response =
  fromMaybe "application/octet-stream" do
    raw <- List.lookup HTTPHeader.hContentType (Client.responseHeaders response)
    nonEmptyText (Text.takeWhile (/= ';') (TextEncoding.decodeUtf8 raw))

matrixMediaProbeRequest :: Client.Request -> Client.Request
matrixMediaProbeRequest request =
  request
    { Client.requestHeaders =
        ("Range", "bytes=0-0") : filter ((/= "Range") . fst) request.requestHeaders
    }

matrixDownloadStatusException :: HTTPStatus.Status -> StrictByteString.ByteString -> MatrixApiException
matrixDownloadStatusException status body =
  case Aeson.eitherDecodeStrict body of
    Right err ->
      MatrixApiException "authenticated media download" status err
    Left parseErr ->
      MatrixTransportException "authenticated media download" [i|HTTP #{HTTPStatus.statusCode status} with non-Matrix error body: #{parseErr}|]

matrixEndpointText :: Text -> [Text] -> Text
matrixEndpointText homeserver path =
  Text.dropWhileEnd (== '/') homeserver <> "/" <> Text.intercalate "/" path

matrixSyncOptions :: MatrixSync -> Option scheme -> Option scheme
matrixSyncOptions MatrixSync{syncSince, syncMode} baseOptions =
  baseOptions
    <> responseTimeout matrixSyncResponseTimeoutMicroseconds
    <> "timeout" =: matrixSyncTimeoutMilliseconds syncMode
    <> maybe mempty ("since" =:) syncSince

newtype MatrixRefreshRequest = MatrixRefreshRequest
  { requestRefreshToken :: Text
  }
  deriving (Generic)
    deriving Aeson.ToJSON via (PrefixedSnakeJSON "request" MatrixRefreshRequest)

data MatrixRefreshResponse = MatrixRefreshResponse
  { refreshedAccessToken :: !Text
  , refreshedRefreshToken :: !(Maybe Text)
  , refreshedExpiresInMs :: !(Maybe Integer)
  }
  deriving (Show, Eq, Generic)
    deriving Aeson.FromJSON via (PrefixedSnakeJSON "refreshed" MatrixRefreshResponse)

data MatrixErrorResponse = MatrixErrorResponse
  { errcode :: !Text
  , matrixError :: !(Maybe Text)
  , retryAfterMs :: !(Maybe Integer)
  , softLogout :: !Bool
  }
  deriving (Show, Eq)

instance Aeson.FromJSON MatrixErrorResponse where
  parseJSON = Aeson.withObject "MatrixErrorResponse" \o ->
    MatrixErrorResponse
      <$> o Aeson..: "errcode"
      <*> o Aeson..:? "error"
      <*> o Aeson..:? "retry_after_ms"
      <*> o Aeson..:? "soft_logout" Aeson..!= False

data MatrixLoginIdentifier = MatrixLoginIdentifier
  { loginIdentifierType :: !Text
  , loginIdentifierUser :: !Text
  }

instance Aeson.ToJSON MatrixLoginIdentifier where
  toJSON MatrixLoginIdentifier{loginIdentifierType, loginIdentifierUser} =
    Aeson.object
      [ "type" Aeson..= loginIdentifierType
      , "user" Aeson..= loginIdentifierUser
      ]

data MatrixLoginRequest = MatrixLoginRequest
  { loginIdentifier :: !MatrixLoginIdentifier
  , loginPassword :: !Text
  , loginDeviceId :: !(Maybe Text)
  , loginInitialDeviceDisplayName :: !(Maybe Text)
  , loginRefreshToken :: !Bool
  }

instance Aeson.ToJSON MatrixLoginRequest where
  toJSON MatrixLoginRequest{loginIdentifier, loginPassword, loginDeviceId, loginInitialDeviceDisplayName, loginRefreshToken} =
    Aeson.object
      [ "type" Aeson..= ("m.login.password" :: Text)
      , "identifier" Aeson..= loginIdentifier
      , "password" Aeson..= loginPassword
      , "device_id" Aeson..= loginDeviceId
      , "initial_device_display_name" Aeson..= loginInitialDeviceDisplayName
      , "refresh_token" Aeson..= loginRefreshToken
      ]

data MatrixLoginResponse = MatrixLoginResponse
  { loginUserId :: !Text
  , loginDeviceId :: !(Maybe Text)
  , loginAccessToken :: !Text
  , loginRefreshToken :: !(Maybe Text)
  , loginExpiresInMs :: !(Maybe Integer)
  }
  deriving (Show, Eq, Generic)
    deriving Aeson.FromJSON via (PrefixedSnakeJSON "login" MatrixLoginResponse)

incomingMessages
  :: (HTTP.HTTP :> es, Media.Media :> es, KatipE :> es, IOE :> es, Concurrent :> es, Prim :> es, Storage.Storage :> es)
  => MatrixDriver
  -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages driver =
  if matrixAuthConfigured cfg
    then do
      S.lift $ logInfo [i|Matrix sync starting: auth=#{matrixAuthMode cfg}|]
      storedSince <- S.lift loadSyncToken
      case storedSince of
        Just since ->
          syncLoop (Just since)
        Nothing -> do
          S.lift $ logInfo "Matrix sync state is empty; initializing from current homeserver state"
          initializeSyncState
    else S.lift $ logInfo "Matrix driver disabled: no access token, refresh token, or login credentials configured"
  where
    cfg = driver.config

    initializeSyncState = do
      result <- S.lift $ sync driver Nothing `catchSync` \err -> do
        logError [i|Matrix sync initialization failed, retrying: #{show err :: String}|]
        threadDelay matrixRetryDelayMicroseconds
        pure Nothing
      case result of
        Nothing ->
          initializeSyncState
        Just response -> do
          S.lift $ storeSyncToken response.nextBatch
          S.lift $ logInfo "Matrix sync state initialized; skipped initial timeline batch"
          syncLoop (Just response.nextBatch)

    syncLoop since = do
      result <- S.lift $ sync driver since `catchSync` \err -> do
        logError [i|Matrix sync failed, retrying: #{show err :: String}|]
        threadDelay matrixRetryDelayMicroseconds
        pure Nothing
      case result of
        Nothing ->
          syncLoop since
        Just response -> do
          runtimeCfg <- S.lift (liftIO (configWithRuntimeOverrides cfg))
          directRoomIds <- S.lift (directRooms driver)
          joinedCounts <- S.lift (joinedMemberCounts driver)
          probedDirectRoomIds <- S.lift (probeDirectRoomIds driver directRoomIds joinedCounts response)
          refreshedDirectRoomIds <- S.lift (directRooms driver)
          let effectiveDirectRoomIds = refreshedDirectRoomIds <> probedDirectRoomIds
              events = syncEvents effectiveDirectRoomIds response
              directCount = Set.size effectiveDirectRoomIds
          S.lift $ logInfo [i|Matrix sync batch: #{length events}; direct_rooms=#{directCount}|]
          for_ events \event -> do
            accessHandled <- S.lift (handleMatrixAccessCommand driver cfg event)
            unless accessHandled $
              case eventToIncomingMessageWith runtimeCfg event of
                Nothing -> do
                  let reason = matrixEventIgnoreReason runtimeCfg event
                  S.lift $ logDebug ("Ignoring Matrix event: " <> reason)
                  S.lift $ logInfo ("Ignoring Matrix event: " <> reason)
                Just message -> do
                  normalized <-
                    if FMBridge.isMirroredQQMatrixMessage message
                      then pure message
                      else S.lift (normalizeMatrixIncomingMessage driver message)
                  S.lift $ logDebug [i|incoming Matrix message: #{show normalized :: String}|]
                  S.lift $ logInfo [i|incoming Matrix message: #{matrixIncomingLogLine event} #{incomingMessageLogLine normalized}|]
                  S.yield normalized
          S.lift $ storeSyncToken response.nextBatch
          syncLoop (Just response.nextBatch)

configWithRuntimeOverrides :: Config -> IO Config
configWithRuntimeOverrides cfg = do
  exists <- Directory.doesFileExist matrixOverridePath
  if not exists
    then pure cfg
    else do
      lines' <- Text.lines <$> TextIO.readFile matrixOverridePath
      let overrides = parseMatrixRoomOverrides lines'
          opened = [room | (room, True) <- overrides]
          closed = ["-" <> room | (room, False) <- overrides]
      pure cfg
        { allowedRooms =
            cfg.allowedRooms <> opened <> closed
        }

matrixOverridePath :: FilePath
matrixOverridePath = "/data/matrix-room-overrides.txt"

parseMatrixRoomOverrides :: [Text] -> [(Text, Bool)]
parseMatrixRoomOverrides = mapMaybe \line ->
  case Text.uncons (Text.strip line) of
    Just ('+', room) | not (Text.null room) -> Just (room, True)
    Just ('-', room) | not (Text.null room) -> Just (room, False)
    _ -> Nothing

handleMatrixAccessCommand
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Config
  -> RoomEvent
  -> Eff es Bool
handleMatrixAccessCommand driver cfg RoomEvent{roomId, event}
  | event.type_ /= "m.room.message" = pure False
  | event.sender `notElem` cfg.superusers = pure False
  | otherwise =
      case event.content.body >>= matrixAccessAction of
        Nothing -> pure False
        Just action -> do
          overrides <- liftIO readMatrixRoomOverrides
          let room = matrixRoomIdText roomId
              sendResponse response = do
                void (sendText driver roomId Nothing response)
                logInfo [i|Matrix access command: action=#{action} room=#{room}|]
                pure True
              persist open response = do
                liftIO (writeMatrixRoomOverrides (setMatrixRoomOverride room open overrides))
                sendResponse response
          case action of
            "view" ->
              sendResponse (matrixRoomAccessStatus cfg room overrides)
            "open" ->
              persist True "已开放当前 Matrix 房间。"
            "close" ->
              persist False "已关闭当前 Matrix 房间。"
            _ -> pure False

matrixAccessAction :: Text -> Maybe Text
matrixAccessAction body =
  case normalizeMatrixAccessCommand body of
    command
      | command `elem` ["查看当前房间权限", "查看这个房间权限", "查看此房间权限", "查看本房间权限"] -> Just "view"
      | command `elem` ["开放当前房间", "开放这个房间", "开放此房间", "开放本房间", "开启当前房间", "开启这个房间", "开启此房间", "开启本房间"] -> Just "open"
      | command `elem` ["关闭当前房间", "关闭这个房间", "关闭此房间", "关闭本房间", "关闭当前房间", "关闭这个房间"] -> Just "close"
    _ -> Nothing

normalizeMatrixAccessCommand :: Text -> Text
normalizeMatrixAccessCommand body =
  let clean = Text.toLower (Text.strip body)
      prefixes = ["fm ", "fm，", "fm,", "fm：", "fm:", "fm"]
  in Text.strip (fromMaybe clean (firstJust (`Text.stripPrefix` clean) prefixes))
  where
    firstJust f = foldr ((<|>) . f) Nothing

readMatrixRoomOverrides :: IO [(Text, Bool)]
readMatrixRoomOverrides = do
  exists <- Directory.doesFileExist matrixOverridePath
  if exists
    then parseMatrixRoomOverrides . Text.lines <$> TextIO.readFile matrixOverridePath
    else pure []

writeMatrixRoomOverrides :: [(Text, Bool)] -> IO ()
writeMatrixRoomOverrides overrides =
  TextIO.writeFile matrixOverridePath $ Text.unlines
    [ (if open then "+" else "-") <> room
    | (room, open) <- overrides
    ]

setMatrixRoomOverride :: Text -> Bool -> [(Text, Bool)] -> [(Text, Bool)]
setMatrixRoomOverride room open overrides =
  (room, open) : filter ((/= room) . fst) overrides

matrixRoomAccessStatus :: Config -> Text -> [(Text, Bool)] -> Text
matrixRoomAccessStatus cfg room overrides =
  case List.lookup room overrides of
    Just True -> "当前 Matrix 房间已开放。"
    Just False -> "当前 Matrix 房间已关闭。"
    Nothing
      | room `elem` cfg.allowedRooms -> "当前 Matrix 房间在默认白名单中，已开放。"
      | otherwise -> "当前 Matrix 房间未开放。"

syncEvents :: Set MatrixRoomId -> SyncResponse -> [RoomEvent]
syncEvents directRoomIds response =
  [ RoomEvent
      { roomId
      , roomIsDirect = roomId `Set.member` directRoomIds || roomLooksDirect room
      , event
      }
  | (roomIdText, room) <- Map.toList response.rooms.join
  , let roomId = matrixRoomId roomIdText
  , event <- room.timeline.events
  ]

syncDirectRoomIds :: SyncResponse -> Set MatrixRoomId
syncDirectRoomIds =
  Set.fromList . fmap matrixRoomId . (.directRooms) . (.accountData)

syncInvitedRoomIds :: SyncResponse -> [Text]
syncInvitedRoomIds =
  Map.keys . (.invite) . (.rooms)

acceptInvitations
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> SyncResponse
  -> Eff es ()
acceptInvitations driver response =
  for_ (syncInvitedRoomIds response) \roomId -> do
    result <- eitherCall "accept room invitation" driver (MatrixJoinRoom (matrixRoomId roomId))
    case result of
      Right _ -> logInfo [i|Accepted Matrix room invitation: #{roomId}|]
      Left reason -> do
        logError [i|Failed to accept Matrix room invitation #{roomId}: #{reason}|]
        throwIO (userError (Text.unpack reason))

syncJoinedMemberCounts :: SyncResponse -> [(MatrixRoomId, Int)]
syncJoinedMemberCounts response =
  [ (roomId, count)
  | (roomIdText, room) <- Map.toList response.rooms.join
  , let roomId = matrixRoomId roomIdText
  , Just count <- [room.summary.joinedMemberCount]
  ]

probeDirectRoomIds
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Set MatrixRoomId
  -> Map MatrixRoomId Int
  -> SyncResponse
  -> Eff es (Set MatrixRoomId)
probeDirectRoomIds driver knownDirectRoomIds joinedCounts response =
  Set.fromList <$> filterM looksDirect roomIdsToProbe
  where
    roomIdsToProbe =
      [ roomId
      | (roomIdText, room) <- Map.toList response.rooms.join
      , let roomId = matrixRoomId roomIdText
      , not (roomId `Set.member` knownDirectRoomIds)
      , not (roomId `Map.member` joinedCounts)
      , isNothing room.summary.joinedMemberCount
      ]

    looksDirect roomId =
      (== Just 2) <$> joinedMemberCount driver roomId

roomLooksDirect :: JoinedRoom -> Bool
roomLooksDirect room =
  room.summary.joinedMemberCount == Just 2

matrixIncomingLogLine :: RoomEvent -> Text
matrixIncomingLogLine RoomEvent{roomId, roomIsDirect} =
  [i|room_id=#{roomId} direct=#{roomIsDirect}|]

matrixAuthConfigured :: Config -> Bool
matrixAuthConfigured cfg =
  isJust cfg.loginUser && isJust cfg.loginPassword

matrixAuthMode :: Config -> Text
matrixAuthMode cfg
  | isJust cfg.loginUser && isJust cfg.loginPassword = "login"
  | otherwise = "none"

replyToMatrix
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
replyToMatrix driver message body =
  matrixReplyResult <$> replyToMatrixResponses driver True message body

replyToMatrixMessages
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es [Either Text MessageId]
replyToMatrixMessages driver message body =
  map matrixMessageIdResult <$> replyToMatrixResponses driver True message body

streamingReplyToMatrix
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
streamingReplyToMatrix driver message body =
  matrixReplyResult <$> replyToMatrixResponses driver False message body

replyToMatrixResponses
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Bool
  -> IncomingMessage
  -> Text
  -> Eff es [Either Text SendMessageResponse]
replyToMatrixResponses driver complete message body =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      let matrixRoom = matrixRoomId roomId
          replyRelation = matrixReplyTo message
          text = Chat.renderReplyBody body
          imageRefs = Chat.replyImageUrls body
      textResponse <- if Text.null (Text.strip text)
        then pure Nothing
        else Just <$> sendMatrixReplyText driver complete matrixRoom replyRelation text
      imageResponses <- traverse (tryMatrixSendImage driver roomId replyRelation) imageRefs
      let responses = maybeToList textResponse <> imageResponses
      logMatrixSendErrors responses
      pure responses
    _ ->
      pure [Left "Matrix reply requires a Matrix room id."]

sendMatrixReplyText
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Bool
  -> MatrixRoomId
  -> Maybe MatrixReplyTo
  -> Text
  -> Eff es (Either Text SendMessageResponse)
sendMatrixReplyText driver complete roomId replyRelation text
  | complete =
      sendText driver roomId replyRelation text
  | otherwise =
      sendTextWithMentionsStreamComplete driver roomId replyRelation text [] False

getMessageContentMatrix
  :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es (Maybe ReferencedMessage)
getMessageContentMatrix driver message messageId =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      fetched <- fetchEvent driver (matrixRoomId roomId) (matrixEventId (messageIdText messageId))
      join <$> traverse (normalizeMatrixReferencedEvent driver) fetched
    _ ->
      pure Nothing

getSenderMemberInfoMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Eff es (Maybe Aeson.Value)
getSenderMemberInfoMatrix driver message =
  case (viaNonEmpty head message.chatAliases, message.senderId) of
    (Just roomId, Just userId) ->
      fmap Aeson.toJSON <$> fetchMember driver (matrixRoomId roomId) userId
    _ ->
      pure Nothing

getMemberInfoMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe Aeson.Value)
getMemberInfoMatrix driver message userId =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      fmap Aeson.toJSON <$> fetchMember driver (matrixRoomId roomId) userId
    _ ->
      pure Nothing

getUserAvatarMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe Aeson.Value)
getUserAvatarMatrix driver _ userId =
  fmap matrixProfileAvatarValue <$> fetchProfile driver userId

listGroupMembersMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Eff es (Maybe Aeson.Value)
listGroupMembersMatrix driver message =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      fmap matrixJoinedMembersValue <$> fetchJoinedMembers driver (matrixRoomId roomId)
    _ ->
      pure Nothing

mentionUserMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Text
  -> Text
  -> Eff es (Either Text MessageId)
mentionUserMatrix driver message userId body =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      let replyRelation = matrixReplyTo message
          text = matrixMentionText userId (Chat.renderReplyBody body)
      response <- sendTextWithMentions driver (matrixRoomId roomId) replyRelation text [userId]
      pure (matrixMessageIdResult response)
    _ ->
      pure (Left "Matrix mention reply requires a Matrix room id.")

matrixMentionText :: Text -> Text -> Text
matrixMentionText userId body =
  let text = Text.strip body
  in if userId `Text.isInfixOf` text
    then text
    else Text.unwords [userId, text]

matrixReferencedMessage :: Event -> Maybe ReferencedMessage
matrixReferencedMessage =
  matrixReferencedMessageFromEvent . applyLatestMatrixReplacement

matrixReferencedMessageFromEvent :: Event -> Maybe ReferencedMessage
matrixReferencedMessageFromEvent event = do
  guard (event.type_ == "m.room.message")
  let body = fromMaybe "" event.content.body
      imageUrls = matrixEventImageUrls event.raw
      files = map fst (matrixEventFileMediaRefs event.raw)
  guard (not (Text.null (Text.strip body)) || not (null imageUrls) || not (null files))
  pure ReferencedMessage
    { messageId = matrixEventMessageId <$> event.eventId
    , senderDisplayName = Just event.sender
    , senderIdentifier = Just event.sender
    , senderIsBot = False
    , text = Text.strip body
    , imageUrls
    , files
    }

applyLatestMatrixReplacement :: Event -> Event
applyLatestMatrixReplacement event =
  fromMaybe event do
    replacementContent <- Aeson.parseMaybe parseLatestReplacementContent event.raw
    content <- Aeson.parseMaybe Aeson.parseJSON replacementContent
    raw <- case event.raw of
      Aeson.Object fields ->
        Just (Aeson.Object (AesonKeyMap.insert "content" replacementContent fields))
      _ ->
        Nothing
    pure event{content, raw}
  where
    parseLatestReplacementContent =
      Aeson.withObject "Matrix event" \eventObject -> do
        unsigned <- eventObject Aeson..: "unsigned"
        relations <- Aeson.withObject "Matrix unsigned" (Aeson..: "m.relations") unsigned
        replacement <- Aeson.withObject "Matrix relations" (Aeson..: "m.replace") relations
        replacementEventContent <- Aeson.withObject "Matrix replacement" (Aeson..: "content") replacement
        Aeson.withObject "Matrix replacement content" (Aeson..: "m.new_content") replacementEventContent

normalizeMatrixReferencedEvent
  :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Event
  -> Eff es (Maybe ReferencedMessage)
normalizeMatrixReferencedEvent driver originalEvent =
  traverse withNormalizedImages (matrixReferencedMessageFromEvent event)
  where
    event = applyLatestMatrixReplacement originalEvent

    withNormalizedImages message = do
      imageUrls <- case matrixEventImageMediaRefs event.raw of
        [] ->
          normalizeMatrixMediaRefs driver message.imageUrls
        mediaRefs ->
          normalizeMatrixMediaRefsWithMetadata driver mediaRefs
      files <- normalizeMatrixFiles driver (matrixEventFileMediaRefs event.raw)
      pure ReferencedMessage
        { messageId = message.messageId
        , senderDisplayName = message.senderDisplayName
        , senderIdentifier = message.senderIdentifier
        , senderIsBot = Just event.sender == driver.config.userId
        , text = message.text
        , imageUrls
        , files
        }

normalizeMatrixIncomingMessage :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> IncomingMessage -> Eff es IncomingMessage
normalizeMatrixIncomingMessage driver message = do
  imageUrls <-
    case matrixEventImageMediaRefs message.raw of
      [] ->
        normalizeMatrixMediaRefs driver message.imageUrls
      mediaRefs ->
        normalizeMatrixMediaRefsWithMetadata driver mediaRefs
  files <- normalizeMatrixFiles driver (matrixEventFileMediaRefs message.raw)
  pure IncomingMessage
    { eventKind = message.eventKind
    , platform = message.platform
    , kind = message.kind
    , chatId = message.chatId
    , chatAliases = message.chatAliases
    , digest = message.digest
    , senderId = message.senderId
    , senderUsername = message.senderUsername
    , messageId = message.messageId
    , replyToMessageId = message.replyToMessageId
    , mentions = message.mentions
    , mentionUsernames = message.mentionUsernames
    , imageUrls
    , files
    , text = message.text
    , raw = message.raw
    }

normalizeMatrixMediaRefs :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> [Text] -> Eff es [Text]
normalizeMatrixMediaRefs driver =
  traverse (normalizeMatrixMediaRef driver Nothing)

normalizeMatrixMediaRefsWithMetadata :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> [MatrixMediaRef] -> Eff es [Text]
normalizeMatrixMediaRefsWithMetadata driver =
  traverse \mediaRef ->
    normalizeMatrixMediaRefWithMetadata driver mediaRef

normalizeMatrixMediaRef :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> Maybe Text -> Text -> Eff es Text
normalizeMatrixMediaRef driver preferredMime ref
  = normalizeMatrixMediaRefWithMetadata driver MatrixMediaRef
      { matrixMediaRefUrl = ref
      , matrixMediaRefMimeType = preferredMime
      , matrixMediaRefEncrypted = Nothing
      }

normalizeMatrixMediaRefWithMetadata :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixMediaRef -> Eff es Text
normalizeMatrixMediaRefWithMetadata driver mediaRef
  | "mxc://" `Text.isPrefixOf` Text.strip ref = do
      let mxcRef = Text.strip ref
      Media.mediaRefForSource mxcRef >>= \case
        Just cachedMediaRef ->
          pure cachedMediaRef
        Nothing ->
          cacheMatrixMediaRef driver mediaRef mxcRef ref
  | otherwise =
      pure ref
  where
    ref = mediaRef.matrixMediaRefUrl

cacheMatrixMediaRef :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixMediaRef -> Text -> Text -> Eff es Text
cacheMatrixMediaRef driver mediaRef mxcRef fallbackRef = do
  cached <- cacheMatrixMediaRefObject driver mediaRef mxcRef `catchSync` \err -> do
    logInfo [i|Matrix media normalization skipped for #{mxcRef}: #{displayException err}|]
    pure Nothing
  case cached of
    Nothing ->
      pure fallbackRef
    Just cachedMediaRef ->
      pure cachedMediaRef

cacheMatrixMediaRefObject :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixMediaRef -> Text -> Eff es (Maybe Text)
cacheMatrixMediaRefObject driver mediaRef mxcRef =
  fetchMatrixMediaObject driver mediaRef mxcRef >>= \case
    Nothing ->
      pure Nothing
    Just mediaObject ->
      Media.storeMediaObjectFromSource mxcRef mediaObject

fetchMatrixMediaObject :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => MatrixDriver -> MatrixMediaRef -> Text -> Eff es (Maybe Media.MediaObject)
fetchMatrixMediaObject driver mediaRef mxcRef = do
  downloadMedia driver mxcRef >>= traverse \media ->
    case mediaRef.matrixMediaRefEncrypted of
      Nothing ->
        pure (matrixMediaObject mediaRef.matrixMediaRefMimeType media)
      Just encrypted ->
        decryptMatrixMediaObject mediaRef.matrixMediaRefMimeType encrypted media

matrixMediaObject :: Maybe Text -> MatrixDownloadedMedia -> Media.MediaObject
matrixMediaObject preferredMime media =
  Media.MediaObject
    { bytes = media.downloadedBytes
    , mimeType = fromMaybe media.downloadedMimeType preferredMime
    , sourceName = media.downloadedName
    }

decryptMatrixMediaObject :: IOE :> es => Maybe Text -> MatrixEncryptedFile -> MatrixDownloadedMedia -> Eff es Media.MediaObject
decryptMatrixMediaObject preferredMime encrypted media = do
  plan <- either (liftIO . ioError . userError . Text.unpack) pure (matrixDecryptionPlan encrypted)
  pure Media.MediaObject
    { bytes = decryptMatrixEncryptedByteStream plan media.downloadedBytes
    , mimeType = fromMaybe media.downloadedMimeType preferredMime
    , sourceName = media.downloadedName
    }

matrixDecryptionPlan :: MatrixEncryptedFile -> Either Text MatrixDecryptionPlan
matrixDecryptionPlan encrypted = do
  key <- first ("invalid Matrix encrypted file key: " <>) (decodeBase64UrlText encrypted.encryptedFileKey)
  ivBytes <- first ("invalid Matrix encrypted file IV: " <>) (decodeBase64TextUnpadded encrypted.encryptedFileIv)
  expectedHash <- first ("invalid Matrix encrypted file sha256: " <>) (decodeBase64TextUnpadded encrypted.encryptedFileSha256)
  cipher <- case CryptoError.eitherCryptoError (CryptoCipher.cipherInit key :: CryptoError.CryptoFailable CryptoAES.AES256) of
    Left err ->
      Left [i|invalid Matrix encrypted file cipher key: #{show err :: String}|]
    Right value ->
      Right value
  iv <- maybe (Left "invalid Matrix encrypted file AES-CTR IV.") Right (CryptoCipher.makeIV ivBytes)
  expectedDigest <- case CryptoHash.digestFromByteString expectedHash :: Maybe (CryptoHash.Digest CryptoHash.SHA256) of
    Nothing ->
      Left "invalid Matrix encrypted file sha256 digest length."
    Just expected ->
      Right expected
  pure MatrixDecryptionPlan
    { decryptionCipher = cipher
    , decryptionIv = iv
    , decryptionExpectedHash = expectedDigest
    }

decryptMatrixEncryptedByteStream :: MatrixDecryptionPlan -> Q.ByteStream (ResourceT IO) () -> Q.ByteStream (ResourceT IO) ()
decryptMatrixEncryptedByteStream plan encryptedBytes =
  Q.fromChunks (go CryptoHash.hashInit 0 StrictByteString.empty (Q.toChunks encryptedBytes))
  where
    go
      :: CryptoHash.Context CryptoHash.SHA256
      -> Int
      -> StrictByteString.ByteString
      -> S.Stream (S.Of StrictByteString.ByteString) (ResourceT IO) ()
      -> S.Stream (S.Of StrictByteString.ByteString) (ResourceT IO) ()
    go context blockIndex pending chunks =
      lift (SP.next chunks) >>= \case
        Left () -> do
          let plainText = decryptMatrixChunkAt plan blockIndex pending
              finalContext = CryptoHash.hashUpdate context pending
              digest = CryptoHash.hashFinalize finalContext
          unless (StrictByteString.null plainText) do
            SP.yield plainText
          unless (digest == plan.decryptionExpectedHash) do
            liftIO (ioError (userError "Matrix encrypted file sha256 verification failed."))
        Right (chunk, rest) -> do
          let bytes = pending <> chunk
              readyLength = (StrictByteString.length bytes `div` matrixAesBlockSize) * matrixAesBlockSize
              (ready, nextPending) = StrictByteString.splitAt readyLength bytes
              plainText = decryptMatrixChunkAt plan blockIndex ready
              nextContext = CryptoHash.hashUpdate context ready
              nextBlockIndex = blockIndex + readyLength `div` matrixAesBlockSize
          unless (StrictByteString.null plainText) do
            SP.yield plainText
          go nextContext nextBlockIndex nextPending rest

decryptMatrixChunkAt :: MatrixDecryptionPlan -> Int -> StrictByteString.ByteString -> StrictByteString.ByteString
decryptMatrixChunkAt plan blockIndex bytes =
  CryptoCipher.ctrCombine plan.decryptionCipher (CryptoCipher.ivAdd plan.decryptionIv blockIndex) bytes

decryptMatrixEncryptedBytesForTest :: Text -> Text -> Text -> [StrictByteString.ByteString] -> IO [StrictByteString.ByteString]
decryptMatrixEncryptedBytesForTest key iv sha256 chunks =
  case matrixDecryptionPlan encrypted of
    Left err ->
      ioError (userError (Text.unpack err))
    Right plan ->
      runResourceT (SP.toList_ (Q.toChunks (decryptMatrixEncryptedByteStream plan (Q.fromChunks (SP.each chunks)))))
  where
    encrypted = MatrixEncryptedFile
      { encryptedFileUrl = "mxc://example.invalid/test"
      , encryptedFileKey = key
      , encryptedFileIv = iv
      , encryptedFileSha256 = sha256
      }

matrixAesBlockSize :: Int
matrixAesBlockSize =
  16

decodeBase64UrlText :: Text -> Either Text StrictByteString.ByteString
decodeBase64UrlText =
  first Text.pack . Base64URL.decode . TextEncoding.encodeUtf8 . Text.strip

decodeBase64TextUnpadded :: Text -> Either Text StrictByteString.ByteString
decodeBase64TextUnpadded =
  first Text.pack . Base64.decode . padBase64 . TextEncoding.encodeUtf8 . Text.strip

padBase64 :: StrictByteString.ByteString -> StrictByteString.ByteString
padBase64 bytes =
  case StrictByteString.length bytes `mod` 4 of
    0 -> bytes
    2 -> bytes <> "=="
    3 -> bytes <> "="
    _ -> bytes

matrixEventImageUrls :: Aeson.Value -> [Text]
matrixEventImageUrls =
  map (.matrixMediaRefUrl) . matrixEventImageMediaRefs

matrixEventImageMediaRefs :: Aeson.Value -> [MatrixMediaRef]
matrixEventImageMediaRefs =
  fromMaybe [] . Aeson.parseMaybe parse
  where
    parse =
      Aeson.withObject "Matrix event" \eventObject -> do
        content <- eventObject Aeson..:? "content" Aeson..!= Aeson.Object mempty
        Aeson.withObject "Matrix event content" parseContent content

    parseContent contentObject = do
      msgtype <- contentObject Aeson..:? "msgtype" Aeson..!= ("" :: Text)
      url <- contentObject Aeson..:? "url"
      encrypted <- contentObject Aeson..:? "file" >>= traverse parseEncryptedFile
      mimeType <- contentObject Aeson..:? "info" Aeson..!= Aeson.Object mempty >>=
        Aeson.withObject "Matrix image info" (Aeson..:? "mimetype")
      pure
        [ MatrixMediaRef
            { matrixMediaRefUrl = imageUrl
            , matrixMediaRefMimeType = nonEmptyText =<< mimeType
            , matrixMediaRefEncrypted = encryptedFile
            }
        | msgtype == "m.image"
        , (imageUrl, encryptedFile) <- maybeToList (matrixImageContentRef url encrypted)
        ]

    matrixImageContentRef url encrypted =
      case url of
        Just imageUrl ->
          Just (imageUrl, Nothing)
        Nothing -> do
          encryptedFile <- encrypted
          Just (encryptedFile.encryptedFileUrl, Just encryptedFile)

matrixEventFileMediaRefs :: Aeson.Value -> [(MessageFile, MatrixMediaRef)]
matrixEventFileMediaRefs =
  fromMaybe [] . Aeson.parseMaybe (Aeson.withObject "Matrix event" \eventObject -> do
    content <- eventObject Aeson..:? "content" Aeson..!= Aeson.Object mempty
    Aeson.withObject "Matrix event content" parseContent content)
  where
    parseContent content = do
      msgtype <- content Aeson..:? "msgtype" Aeson..!= ("" :: Text)
      explicitName <- content Aeson..:? "filename"
      body <- content Aeson..:? "body"
      let name = explicitName <|> body
      url <- content Aeson..:? "url"
      encrypted <- content Aeson..:? "file" >>= traverse parseEncryptedFile
      mimeType <- content Aeson..:? "info" Aeson..!= Aeson.Object mempty >>=
        Aeson.withObject "Matrix file info" (Aeson..:? "mimetype")
      pure do
        guard (msgtype `elem` ["m.file", "m.audio"])
        fileName <- maybeToList (name >>= nonEmptyText)
        (mediaUrl, encryptedFile) <- maybeToList (contentRef url encrypted)
        let mediaRef = MatrixMediaRef mediaUrl (nonEmptyText =<< mimeType) encryptedFile
        pure (MessageFile{name = fileName, ref = mediaUrl}, mediaRef)

    contentRef (Just url) _ = Just (url, Nothing)
    contentRef Nothing encrypted = do
      file <- encrypted
      pure (file.encryptedFileUrl, Just file)

normalizeMatrixFiles
  :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> [(MessageFile, MatrixMediaRef)]
  -> Eff es [MessageFile]
normalizeMatrixFiles driver =
  traverse \(file, mediaRef) -> do
    ref <- normalizeMatrixMediaRefWithMetadata driver mediaRef
    pure MessageFile{name = file.name, ref}

parseEncryptedFile :: Aeson.Value -> Aeson.Parser MatrixEncryptedFile
parseEncryptedFile =
  Aeson.withObject "Matrix encrypted file" \fileObject -> do
    url <- fileObject Aeson..: "url"
    iv <- fileObject Aeson..: "iv"
    sha256 <- fileObject Aeson..: "hashes" >>=
      Aeson.withObject "Matrix encrypted file hashes" (Aeson..: "sha256")
    key <- fileObject Aeson..: "key" >>=
      Aeson.withObject "Matrix encrypted file key" \keyObject -> do
        alg <- keyObject Aeson..:? "alg" Aeson..!= ("" :: Text)
        unless (alg == "A256CTR") do
          fail [i|unsupported Matrix encrypted file algorithm: #{alg}|]
        keyObject Aeson..: "k"
    pure MatrixEncryptedFile
      { encryptedFileUrl = url
      , encryptedFileKey = key
      , encryptedFileIv = iv
      , encryptedFileSha256 = sha256
      }

matrixProfileAvatarValue :: MatrixProfile -> Aeson.Value
matrixProfileAvatarValue profile =
  Aeson.object
    [ "platform" Aeson..= ("matrix" :: Text)
    , "user_id" Aeson..= profile.profileUserId
    , "displayname" Aeson..= profile.profileDisplayName
    , "avatar_url" Aeson..= profile.profileAvatarUrl
    ]

matrixJoinedMembersValue :: JoinedMembersResponse -> Aeson.Value
matrixJoinedMembersValue response =
  Aeson.object
    [ "joined" Aeson..= response.joinedMembers
    ]

matrixReplyTo :: IncomingMessage -> Maybe MatrixReplyTo
matrixReplyTo message =
  MatrixReplyTo <$> (matrixRawEventId message.raw <|> (matrixEventId . messageIdText <$> message.messageId))

uploadFileMatrix
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> FilePath
  -> Maybe Text
  -> Eff es (Either Text MessageId)
uploadFileMatrix driver message path requestedFileName =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      matrixUserFacingEither "file upload" do
        let fileName = Driver.uploadFileName path requestedFileName
            mime = Mime.mimeFromName (Text.pack path)
        size <- FileSystem.getFileSize path
        uploaded <- uploadMedia driver path fileName mime
        response <- sendFileMessage driver roomId (matrixReplyTo message) MatrixFileMessage
          { msgtype = matrixFileMsgtype mime
          , body = fileName
          , filename = fileName
          , url = uploaded.contentUri
          , info = MatrixFileInfo
              { mimetype = mime
              , size = size
              }
          }
        pure (matrixMessageIdResult response)
    _ ->
      pure (Left "Matrix file upload requires a Matrix room id.")

sendMatrixImage
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Text
  -> Maybe MatrixReplyTo
  -> Text
  -> Eff es (Either Text SendMessageResponse)
sendMatrixImage driver roomId replyRelation imageRef =
  case matrixMxcRef imageRef of
    Just contentUri ->
      sendMatrixImageMessage driver roomId replyRelation "image" contentUri "application/octet-stream" 0
    Nothing -> do
      let scope = matrixMediaScope driver
      Media.platformMediaRef "matrix" scope imageRef >>= \case
        Just contentUri ->
          sendMatrixImageMessage driver roomId replyRelation "image" contentUri "application/octet-stream" 0
        Nothing ->
          withMatrixImageFile imageRef \path fileName mime -> do
            size <- FileSystem.getFileSize path
            uploaded <- uploadMedia driver path fileName mime
            Media.storePlatformMediaRef "matrix" scope imageRef uploaded.contentUri
            sendMatrixImageMessage driver roomId replyRelation fileName uploaded.contentUri mime size

tryMatrixSendImage
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Text
  -> Maybe MatrixReplyTo
  -> Text
  -> Eff es (Either Text SendMessageResponse)
tryMatrixSendImage driver roomId replyRelation imageRef = do
  result <- trySync (sendMatrixImage driver roomId replyRelation imageRef)
  pure case result of
    Left err ->
      Left (matrixUserFacingExceptionText [i|image reply for #{imageRef}|] err)
    Right response ->
      response

matrixMediaScope :: MatrixDriver -> Text
matrixMediaScope driver =
  fromMaybe driver.config.homeserver driver.config.userId

sendMatrixImageMessage
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Text
  -> Maybe MatrixReplyTo
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Eff es (Either Text SendMessageResponse)
sendMatrixImageMessage driver roomId replyRelation fileName contentUri mime size =
  sendFileMessage driver roomId replyRelation MatrixFileMessage
    { msgtype = "m.image"
    , body = fileName
    , filename = fileName
    , url = contentUri
    , info = MatrixFileInfo
        { mimetype = mime
        , size = size
        }
    }

replyAudioMatrix
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> Text
  -> Maybe Text
  -> Eff es (Either Text MessageId)
replyAudioMatrix driver message audioRef caption =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      sendMatrixAudio driver roomId audioRef caption
    _ ->
      pure (Left "Matrix audio reply requires a Matrix room id.")

sendMatrixAudio
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> Text
  -> Text
  -> Maybe Text
  -> Eff es (Either Text MessageId)
sendMatrixAudio driver roomId audioRef caption =
  matrixUserFacingEither "audio reply" do
    case matrixMxcRef audioRef of
      Just contentUri -> do
        let fileName = "audio"
        response <- sendFileMessage driver roomId Nothing (matrixAudioMessage caption fileName contentUri "application/octet-stream" 0)
        pure (matrixMessageIdResult response)
      Nothing ->
        withMatrixAudioFile audioRef \path fileName mime -> do
          size <- FileSystem.getFileSize path
          uploaded <- uploadMedia driver path fileName mime
          response <- sendFileMessage driver roomId Nothing (matrixAudioMessage caption fileName uploaded.contentUri mime size)
          pure (matrixMessageIdResult response)

matrixUserFacingEither
  :: KatipE :> es
  => Text
  -> Eff es (Either Text a)
  -> Eff es (Either Text a)
matrixUserFacingEither label action =
  action `catch` \(err :: MatrixApiException) -> do
    logWarning [i|#{matrixApiExceptionMessage err}|]
    pure (Left (matrixUserFacingApiError label err))

matrixMessageIdResult :: Either Text SendMessageResponse -> Either Text MessageId
matrixMessageIdResult =
  fmap (matrixEventMessageId . (.eventId))

matrixReplyResult :: [Either Text SendMessageResponse] -> Either Text MessageId
matrixReplyResult responses =
  case errors of
    err : _ ->
      Left (matrixReplyFailureText sentIds err)
    [] ->
      case sentIds of
        sent : _ ->
          Right sent
        [] ->
          Left "Matrix reply did not send any message."
  where
    sentIds = [matrixEventMessageId response.eventId | Right response <- responses]
    errors = [err | Left err <- responses]

matrixReplyFailureText :: [MessageId] -> Text -> Text
matrixReplyFailureText sentIds err =
  case sentIds of
    [] ->
      err
    sent : _ ->
      [i|#{err} Text message was sent as #{messageIdText sent}, but one or more image messages failed.|]

logMatrixSendErrors :: KatipE :> es => [Either Text SendMessageResponse] -> Eff es ()
logMatrixSendErrors responses =
  traverse_ logWarning [err | Left err <- responses]

matrixAudioMessage :: Maybe Text -> Text -> Text -> Text -> Integer -> MatrixFileMessage
matrixAudioMessage caption fileName contentUri mime size =
  MatrixFileMessage
    { msgtype = "m.audio"
    , body = fromMaybe fileName (caption >>= nonEmptyText)
    , filename = fileName
    , url = contentUri
    , info = MatrixFileInfo
        { mimetype = mime
        , size = size
        }
    }

matrixUploadFileName :: FilePath -> Text
matrixUploadFileName path =
  let name = Text.pack (takeFileName path)
  in if Text.null name then "file" else name

matrixFileMsgtype :: Text -> Text
matrixFileMsgtype mime
  | "image/" `Text.isPrefixOf` clean = "m.image"
  | "audio/" `Text.isPrefixOf` clean = "m.audio"
  | "video/" `Text.isPrefixOf` clean = "m.video"
  | otherwise = "m.file"
  where
    clean = Text.toLower (Text.takeWhile (/= ';') mime)

matrixMxcRef :: Text -> Maybe Text
matrixMxcRef ref =
  let stripped = Text.strip ref
  in stripped <$ guard ("mxc://" `Text.isPrefixOf` stripped)

withMatrixImageFile
  :: (Media.Media :> es, FileSystem :> es, IOE :> es)
  => Text
  -> (FilePath -> Text -> Text -> Eff es a)
  -> Eff es a
withMatrixImageFile imageRef action =
  Media.localMediaPath imageRef >>= \case
    Just path -> do
      mediaInfo <- Media.mediaFileInfoByRef imageRef
      let fileName = fromMaybe (matrixUploadFileName path) (mediaInfo >>= (.sourceName))
          mime = maybe (matrixImageMimeType path) (.mimeType) mediaInfo
      action path fileName mime
    Nothing ->
      case matrixLocalPath imageRef of
        Just path ->
          action path (matrixUploadFileName path) (matrixImageMimeType path)
        Nothing ->
          case matrixDataImage imageRef of
            Just (mime, bytes) ->
              withTemporaryMatrixImage mime bytes \path ->
                action path (matrixUploadFileName path) mime
            Nothing ->
              throwIO (userError "Matrix image reply requires a media:, file://, data:image/*, or mxc:// image reference.")

withMatrixAudioFile
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> (FilePath -> Text -> Text -> Eff es (Either Text MessageId))
  -> Eff es (Either Text MessageId)
withMatrixAudioFile audioRef action =
  case matrixLocalPath audioRef of
    Just path ->
      action path (matrixUploadFileName path) (matrixAudioMimeType path)
    Nothing ->
      case matrixDataAudio audioRef of
        Just (mime, bytes) ->
          withTemporaryMatrixAudio mime bytes \path ->
            action path (matrixUploadFileName path) mime
        Nothing ->
          pure (Left "Matrix audio reply requires a file://, data:audio/*, or mxc:// audio reference.")

matrixLocalPath :: Text -> Maybe FilePath
matrixLocalPath ref =
  let stripped = Text.strip ref
  in case Text.stripPrefix "file://" stripped of
    Just path ->
      Just (Text.unpack path)
    Nothing
      | isLocalPathRef stripped ->
          Just (Text.unpack stripped)
      | otherwise ->
          Nothing

isLocalPathRef :: Text -> Bool
isLocalPathRef ref =
  "/" `Text.isPrefixOf` ref || "./" `Text.isPrefixOf` ref || "../" `Text.isPrefixOf` ref

matrixDataAudio :: Text -> Maybe (Text, Q.ByteStream (ResourceT IO) ())
matrixDataAudio ref = do
  rest <- Text.stripPrefix "data:audio/" (Text.strip ref)
  let (subtype, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  bytes <- either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))
  pure ("audio/" <> subtype, Q.fromStrict bytes)

matrixDataImage :: Text -> Maybe (Text, Q.ByteStream (ResourceT IO) ())
matrixDataImage ref = do
  rest <- Text.stripPrefix "data:image/" (Text.strip ref)
  let (subtype, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  bytes <- either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))
  pure ("image/" <> subtype, Q.fromStrict bytes)

withTemporaryMatrixImage
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> Q.ByteStream (ResourceT IO) ()
  -> (FilePath -> Eff es a)
  -> Eff es a
withTemporaryMatrixImage mime bytes action = do
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-matrix-" \dir -> do
      let path = dir </> ("matrix-image" <.> matrixImageExtension mime)
      liftIO (runResourceT (Q.writeFile path bytes))
      raise (action path)

withTemporaryMatrixAudio
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> Q.ByteStream (ResourceT IO) ()
  -> (FilePath -> Eff es a)
  -> Eff es a
withTemporaryMatrixAudio mime bytes action = do
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-matrix-" \dir -> do
      let path = dir </> ("matrix-audio" <.> matrixAudioExtension mime)
      liftIO (runResourceT (Q.writeFile path bytes))
      raise (action path)

matrixAudioMimeType :: FilePath -> Text
matrixAudioMimeType =
  Mime.mimeFromName . Text.pack

matrixImageMimeType :: FilePath -> Text
matrixImageMimeType =
  Mime.mimeFromName . Text.pack

matrixImageExtension :: Text -> String
matrixImageExtension mime =
  Text.unpack (Text.dropWhile (== '.') (Mime.extensionFromMime mime))

matrixAudioExtension :: Text -> String
matrixAudioExtension mime =
  Text.unpack (Text.dropWhile (== '.') (Mime.extensionFromMime mime))

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped

editMessageMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> MessageId
  -> Text
  -> Eff es Bool
editMessageMatrix driver message messageId body =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      case matrixEditableEditText body of
        Nothing ->
          pure True
        Just text -> do
          response <- editText driver (matrixRoomId roomId) (matrixEventId (messageIdText messageId)) text
          logMatrixSendErrors [response]
          pure (either (const False) (const True) response)
    _ ->
      pure False

completeMessageEditMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
completeMessageEditMatrix driver message messageId =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      let eventId = matrixEventId (messageIdText messageId)
      popStreamTextMessage driver.streamTextMessages eventId >>= \case
        Nothing ->
          pure True
        Just request -> do
          let completeRequest = completeMatrixEditMessageRequest request
          response <- eitherCall "complete stream m.room.message" driver (MatrixCompleteStreamMessage (matrixRoomId roomId) completeRequest)
          logMatrixSendErrors [response]
          pure (either (const False) (const True) response)
    _ ->
      pure False

matrixEditableEditText :: Text -> Maybe Text
matrixEditableEditText body
  | Text.null (Text.strip text) && not (null imageRefs) =
      Nothing
  | otherwise =
      Just text
  where
    text = Chat.renderReplyBody body
    imageRefs = Chat.replyImageUrls body

deleteMessageMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
deleteMessageMatrix driver message messageId =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      deleteEvent driver roomId messageId (currentRawEventId message messageId)
    _ ->
      pure False

currentRawEventId :: IncomingMessage -> MessageId -> Maybe MatrixEventId
currentRawEventId message messageId = do
  guard (message.messageId == Just messageId)
  matrixRawEventId message.raw

matrixRawEventId :: Aeson.Value -> Maybe MatrixEventId
matrixRawEventId =
  Aeson.parseMaybe (Aeson.withObject "Matrix event" \o -> matrixEventId <$> o Aeson..: "event_id")

eventToIncomingMessage :: RoomEvent -> Maybe IncomingMessage
eventToIncomingMessage =
  eventToIncomingMessageWith defaultConfig

eventToIncomingMessageWith :: Config -> RoomEvent -> Maybe IncomingMessage
eventToIncomingMessageWith cfg RoomEvent{roomId, roomIsDirect, event}
  | event.type_ == "m.room.redaction" = do
      redactedEventId <- matrixRedactedEventId event.raw
      pure IncomingMessage
        { eventKind = IncomingMessageDeleted
        , platform = PlatformMatrix
        , kind = if roomIsDirect then ChatPrivate else ChatGroup
        , chatId = Just (stableTextId (matrixRoomIdText roomId))
        , chatAliases = [matrixRoomIdText roomId]
        , digest = matrixMessageDigest cfg roomId roomIsDirect event ""
        , senderId = Just event.sender
        , senderUsername = Just event.sender
        , messageId = Just (matrixEventMessageId redactedEventId)
        , replyToMessageId = Nothing
        , mentions = []
        , mentionUsernames = []
        , imageUrls = []
        , files = []
        , text = ""
        , raw = event.raw
        }
  | otherwise = do
      guard (event.type_ == "m.room.message")
      guard (not (isOwnEvent cfg event))
      guard (not (matrixStreamIncomplete event.raw))
      body <- event.content.body
      guard (not (Text.null (Text.strip body)))
      pure IncomingMessage
        { eventKind = IncomingMessageCreated
        , platform = PlatformMatrix
        , kind = if roomIsDirect then ChatPrivate else ChatGroup
        , chatId = Just (stableTextId (matrixRoomIdText roomId))
        , chatAliases = [matrixRoomIdText roomId]
        , digest = matrixMessageDigest cfg roomId roomIsDirect event body
        , senderId = Just event.sender
        , senderUsername = Just event.sender
        , messageId = matrixEventMessageId <$> event.eventId
        , replyToMessageId = matrixEventMessageId <$> event.content.replyToEventId
        , mentions = []
        , mentionUsernames = matrixMentions cfg event.content body
        , imageUrls = matrixEventImageUrls event.raw
        , files = map fst (matrixEventFileMediaRefs event.raw)
        , text = Text.strip (matrixReplyBody event.content body)
        , raw = event.raw
        }

matrixReplyBody :: EventContent -> Text -> Text
matrixReplyBody content body
  | isJust content.replyToEventId
  , firstLine : _ <- Text.lines body
  , isMatrixReplyFallbackFirstLine firstLine =
      Text.unlines (dropWhile (Text.isPrefixOf "> ") (Text.lines body))
  | otherwise =
      body

isMatrixReplyFallbackFirstLine :: Text -> Bool
isMatrixReplyFallbackFirstLine line =
  case Text.words line of
    ">" : "*" : userId : _ -> isFallbackUserId userId
    ">" : userId : _ -> isFallbackUserId userId
    _ -> False
  where
    isFallbackUserId token =
      maybe False isMatrixUserId (Text.stripPrefix "<" token >>= Text.stripSuffix ">")

matrixRedactedEventId :: Aeson.Value -> Maybe MatrixEventId
matrixRedactedEventId =
  Aeson.parseMaybe $ Aeson.withObject "Matrix redaction event" \o ->
    (matrixEventId <$> o Aeson..: "redacts")
      <|> do
        content <- o Aeson..: "content"
        Aeson.withObject "Matrix redaction content" (fmap matrixEventId . (Aeson..: "redacts")) content

matrixEventIgnoreReason :: Config -> RoomEvent -> Text
matrixEventIgnoreReason cfg RoomEvent{roomId, event}
  | eventType /= "m.room.message" =
      [i|unsupported event type #{eventType}; #{context}|]
  | isOwnEvent cfg event =
      [i|own event; #{context}|]
  | isEditEvent event =
      [i|edit event; #{context}|]
  | matrixStreamIncomplete event.raw =
      [i|incomplete stream event; #{context}|]
  | isNothing event.content.body =
      [i|missing content.body; #{context}|]
  | Text.null (Text.strip (fromMaybe "" event.content.body)) =
      [i|blank content.body; #{context}|]
  | otherwise =
      [i|unknown reason; #{context}|]
  where
    eventType :: Text
    eventType = event.type_

    eventSender :: Text
    eventSender = event.sender

    eventIdText :: Text
    eventIdText = maybe "<none>" matrixEventIdText event.eventId

    eventMsgtype :: Text
    eventMsgtype = fromMaybe "<none>" event.content.msgtype

    context :: Text
    context =
      [i|room=#{roomId} sender=#{eventSender} event_id=#{eventIdText} msgtype=#{eventMsgtype}|]

matrixMessageDigest :: Config -> MatrixRoomId -> Bool -> Event -> Text -> MessageDigest
matrixMessageDigest cfg roomId _roomIsDirect event body =
  MessageDigest
    { chatIsAllowed = roomAllowed
    , senderIsAllowed = senderSuperuser
    , senderIsSuperuser = senderSuperuser
    , mentionsBot = maybe False (\botId -> botId `elem` event.content.mentions) cfg.userId
    , botId = cfg.userId
    }
  where
    roomAllowed =
      (senderSuperuser && isMatrixAccessCommand body)
        || ( not roomDenied
          && matrixRoomIdText roomId `elem` cfg.allowedRooms
           )
    roomDenied =
      ("-" <> matrixRoomIdText roomId) `elem` cfg.allowedRooms
    senderSuperuser =
      event.sender `elem` cfg.superusers

isMatrixAccessCommand :: Text -> Bool
isMatrixAccessCommand body =
  isJust (matrixAccessAction body)

matrixMentions :: Config -> EventContent -> Text -> [Text]
matrixMentions cfg content body =
  case content.mentions of
    [] ->
      [ userId
      | Just userId <- [cfg.userId]
      , userId `Text.isInfixOf` body
      ]
    mentions ->
      mentions

isOwnEvent :: Config -> Event -> Bool
isOwnEvent cfg event =
  cfg.userId == Just event.sender

isEditEvent :: Event -> Bool
isEditEvent event =
  matrixRelationType event.raw == Just "m.replace"

matrixStreamIncomplete :: Aeson.Value -> Bool
matrixStreamIncomplete =
  fromMaybe False . Aeson.parseMaybe parse
  where
    parse =
      Aeson.withObject "Matrix event" \eventObject -> do
        content <- eventObject Aeson..:? "content" Aeson..!= Aeson.Object mempty
        Aeson.withObject "Matrix event content" parseContent content

    parseContent contentObject = do
      metadata <- contentObject Aeson..:? "com.pfeiwu.ai.stream" Aeson..!= MatrixStreamMetadata True
      pure (not metadata.streamComplete)

matrixRelationType :: Aeson.Value -> Maybe Text
matrixRelationType =
  Aeson.parseMaybe $
    Aeson.withObject "Matrix event" \eventObject -> do
      content <- eventObject Aeson..: "content"
      Aeson.withObject "Matrix content" (\contentObject -> contentObject Aeson..: "m.relates_to") content >>=
        Aeson.withObject "Matrix relation" (Aeson..: "rel_type")

defaultConfig :: Config
defaultConfig = Config
  { homeserver = "https://matrix.org"
  , loginUser = Nothing
  , loginPassword = Nothing
  , deviceId = Nothing
  , directRooms = []
  , userId = Nothing
  , allowedRooms = []
  , superusers = []
  }

matrixLogin :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Config -> Text -> Text -> Eff es MatrixLoginResponse
matrixLogin cfg user password = do
  let request = MatrixLoginRequest
        { loginIdentifier = MatrixLoginIdentifier
            { loginIdentifierType = "m.id.user"
            , loginIdentifierUser = user
            }
        , loginPassword = password
        , loginDeviceId = cfg.deviceId
        , loginInitialDeviceDisplayName = Just "cosmobot"
        , loginRefreshToken = True
        }
  matrixUnauthenticatedJsonCall cfg "login" "login" matrixApiOptionsNoAuth
    POST
    (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "login")
    (ReqBodyJson request)

refreshMatrixToken :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es) => Config -> Text -> Eff es MatrixRefreshResponse
refreshMatrixToken cfg refreshToken = do
  let request = MatrixRefreshRequest refreshToken
  matrixUnauthenticatedJsonCall cfg "refresh" "refresh access token" matrixApiOptionsNoAuth
    POST
    (\baseUrl -> baseUrl /: "_matrix" /: "client" /: "v3" /: "refresh")
    (ReqBodyJson request)

parseMxcUri :: Text -> Maybe (Text, Text)
parseMxcUri ref = do
  rest <- Text.stripPrefix "mxc://" (Text.strip ref)
  let (serverName, mediaWithSlash) = Text.breakOn "/" rest
  mediaId <- Text.stripPrefix "/" mediaWithSlash
  guard (not (Text.null serverName))
  guard (not (Text.null mediaId))
  pure (serverName, mediaId)

withMatrixBaseUrl :: IOE :> es => Text -> (forall scheme. Url scheme -> Option scheme -> Eff es a) -> Eff es a
withMatrixBaseUrl homeserver action = do
  uri <- URI.mkURI homeserver
  case useURI uri of
    Nothing ->
      liftIO (ioError (userError [i|Unsupported Matrix homeserver URL: #{homeserver}. Use a full HTTP or HTTPS base URL.|]))
    Just (Left (baseUrl, baseOptions)) ->
      action baseUrl baseOptions
    Just (Right (baseUrl, baseOptions)) ->
      action baseUrl baseOptions

matrixAuth :: Text -> Option scheme
matrixAuth token =
  header "Authorization" (ByteString.pack [i|Bearer #{token}|])

matrixHttpConfig :: HttpConfig
matrixHttpConfig =
  defaultHttpConfig
    { httpConfigRetryJudge = \_ _ -> False
    , httpConfigRetryJudgeException = \_ _ -> False
    }

stableTextId :: Text -> Integer
stableTextId =
  Text.foldl' step 14695981039346656037
  where
    step acc char =
      fromIntegral ((fromIntegral acc `xor` fromIntegral (fromEnum char)) * fnvPrime :: Word64)
    fnvPrime :: Word64
    fnvPrime = 1099511628211

nonEmptyMatrixBody :: Text -> Text
nonEmptyMatrixBody body
  | Text.null (Text.strip body) = " "
  | otherwise = body

matrixOutgoingMentionUserIds :: Text -> [Text] -> [Text]
matrixOutgoingMentionUserIds body explicitUserIds =
  Set.toList (Set.fromList (filter isMatrixUserId (explicitUserIds <> matrixUserIdsInText body)))

fetchMatrixMentionNames
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => MatrixDriver
  -> MatrixRoomId
  -> [Text]
  -> Eff es (Map Text Text)
fetchMatrixMentionNames _ _ [] =
  pure Map.empty
fetchMatrixMentionNames driver roomId mentionUserIds = do
  result <- trySync (maybeCall driver (MatrixJoinedMembers roomId))
  case result of
    Left err -> do
      logInfo [i|Matrix mention display names unavailable: #{displayException err}|]
      pure Map.empty
    Right Nothing ->
      pure Map.empty
    Right (Just members) ->
      pure (matrixMentionNames mentionUserIds members)

matrixMentionNames :: [Text] -> JoinedMembersResponse -> Map Text Text
matrixMentionNames mentionUserIds members =
  Map.fromList
    [ (userId, name)
    | userId <- mentionUserIds
    , Just member <- [Map.lookup userId members.joinedMembers]
    , Just name <- [matrixMentionDisplayName =<< member.memberDisplayName]
    ]

matrixMentionDisplayName :: Text -> Maybe Text
matrixMentionDisplayName name = do
  displayName <- nonEmptyText name
  pure if "@" `Text.isPrefixOf` displayName
    then displayName
    else "@" <> displayName

matrixUserIdsInText :: Text -> [Text]
matrixUserIdsInText =
  mapMaybe matrixUserIdToken . Text.words

matrixUserIdToken :: Text -> Maybe Text
matrixUserIdToken raw =
  let token = Text.dropWhileEnd (`elem` matrixUserIdTrailingPunctuation) raw
  in token <$ guard (isMatrixUserId token)

matrixUserIdTrailingPunctuation :: [Char]
matrixUserIdTrailingPunctuation =
  ".,;:!?)]}>\"'"

isMatrixUserId :: Text -> Bool
isMatrixUserId token =
  "@" `Text.isPrefixOf` token && ":" `Text.isInfixOf` token

data RoomEvent = RoomEvent
  { roomId :: !MatrixRoomId
  , roomIsDirect :: !Bool
  , event :: !Event
  }
  deriving (Show)

data SyncResponse = SyncResponse
  { nextBatch :: !Text
  , rooms :: !Rooms
  , accountData :: !AccountData
  }
  deriving (Show, Generic)

instance Aeson.FromJSON SyncResponse where
  parseJSON = Aeson.withObject "SyncResponse" \o ->
    SyncResponse
      <$> o Aeson..: "next_batch"
      <*> o Aeson..:? "rooms" Aeson..!= Rooms Map.empty Map.empty
      <*> o Aeson..:? "account_data" Aeson..!= AccountData []

newtype AccountData = AccountData
  { directRooms :: [Text]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON AccountData where
  parseJSON = Aeson.withObject "AccountData" \o -> do
    events <- o Aeson..:? "events" Aeson..!= []
    pure AccountData
      { directRooms = concatMap accountDataEventDirectRooms events
      }

data AccountDataEvent = AccountDataEvent
  { accountDataEventType :: !Text
  , accountDataEventContent :: !Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON AccountDataEvent where
  parseJSON = Aeson.withObject "AccountDataEvent" \o ->
    AccountDataEvent
      <$> o Aeson..: "type"
      <*> o Aeson..:? "content" Aeson..!= Aeson.Object mempty

accountDataEventDirectRooms :: AccountDataEvent -> [Text]
accountDataEventDirectRooms event
  | event.accountDataEventType == "m.direct" =
      concat (fromMaybe [] (Aeson.parseMaybe parseDirectRooms event.accountDataEventContent))
  | otherwise =
      []
  where
    parseDirectRooms :: Aeson.Value -> Aeson.Parser [[Text]]
    parseDirectRooms =
      Aeson.withObject "m.direct content" \o ->
        traverse Aeson.parseJSON (AesonKeyMap.elems o)

data Rooms = Rooms
  { join :: Map Text JoinedRoom
  , invite :: Map Text Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Rooms where
  parseJSON = Aeson.withObject "Rooms" \o ->
    Rooms
      <$> o Aeson..:? "join" Aeson..!= Map.empty
      <*> o Aeson..:? "invite" Aeson..!= Map.empty

data JoinedRoom = JoinedRoom
  { timeline :: Timeline
  , summary :: RoomSummary
  }
  deriving (Show, Generic)

instance Aeson.FromJSON JoinedRoom where
  parseJSON = Aeson.withObject "JoinedRoom" \o ->
    JoinedRoom
      <$> o Aeson..:? "timeline" Aeson..!= Timeline []
      <*> o Aeson..:? "summary" Aeson..!= RoomSummary Nothing

newtype RoomSummary = RoomSummary
  { joinedMemberCount :: Maybe Int
  }
  deriving (Show, Generic)

instance Aeson.FromJSON RoomSummary where
  parseJSON = Aeson.withObject "RoomSummary" \o ->
    RoomSummary <$> o Aeson..:? "m.joined_member_count"

newtype JoinedMembersResponse = JoinedMembersResponse
  { joinedMembers :: Map Text MatrixMember
  }
  deriving (Show, Generic)

instance Aeson.FromJSON JoinedMembersResponse where
  parseJSON = Aeson.withObject "JoinedMembersResponse" \o -> do
    joined <- o Aeson..:? "joined" Aeson..!= Map.empty
    pure (JoinedMembersResponse (Map.mapWithKey matrixMemberFromJoined joined))

data MatrixMember = MatrixMember
  { memberUserId :: !Text
  , memberDisplayName :: !(Maybe Text)
  , memberAvatarUrl :: !(Maybe Text)
  , memberMembership :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON MatrixMember where
  toJSON MatrixMember{memberUserId, memberDisplayName, memberAvatarUrl, memberMembership} =
    Aeson.object
      [ "user_id" Aeson..= memberUserId
      , "displayname" Aeson..= memberDisplayName
      , "avatar_url" Aeson..= memberAvatarUrl
      , "membership" Aeson..= memberMembership
      ]

data MatrixMemberContent = MatrixMemberContent
  { memberContentDisplayName :: !(Maybe Text)
  , memberContentAvatarUrl :: !(Maybe Text)
  , memberContentMembership :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON MatrixMemberContent where
  parseJSON = Aeson.withObject "MatrixMemberContent" \o -> do
    displayName <- o Aeson..:? "displayname"
    joinedDisplayName <- o Aeson..:? "display_name"
    MatrixMemberContent
      <$> pure (displayName <|> joinedDisplayName)
      <*> o Aeson..:? "avatar_url"
      <*> o Aeson..:? "membership"

matrixMemberFromContent :: Text -> MatrixMemberContent -> MatrixMember
matrixMemberFromContent userId content =
  MatrixMember
    { memberUserId = userId
    , memberDisplayName = content.memberContentDisplayName
    , memberAvatarUrl = content.memberContentAvatarUrl
    , memberMembership = content.memberContentMembership
    }

matrixMemberFromJoined :: Text -> MatrixMemberContent -> MatrixMember
matrixMemberFromJoined =
  matrixMemberFromContent

data MatrixProfile = MatrixProfile
  { profileUserId :: !Text
  , profileDisplayName :: !(Maybe Text)
  , profileAvatarUrl :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

data MatrixProfileContent = MatrixProfileContent
  { profileContentDisplayName :: !(Maybe Text)
  , profileContentAvatarUrl :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON MatrixProfileContent where
  parseJSON = Aeson.withObject "MatrixProfileContent" \o ->
    MatrixProfileContent
      <$> o Aeson..:? "displayname"
      <*> o Aeson..:? "avatar_url"

newtype Timeline = Timeline
  { events :: [Event]
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Timeline where
  parseJSON = Aeson.withObject "Timeline" \o ->
    Timeline <$> o Aeson..:? "events" Aeson..!= []

data Event = Event
  { type_ :: !Text
  , sender :: !Text
  , eventId :: !(Maybe MatrixEventId)
  , content :: !EventContent
  , raw :: !Aeson.Value
  }
  deriving (Show, Generic)

instance Aeson.FromJSON Event where
  parseJSON value = Aeson.withObject "Event" parse value
    where
      parse o = do
        type_ <- o Aeson..: "type"
        sender <- o Aeson..: "sender"
        eventId <- fmap matrixEventId <$> o Aeson..:? "event_id"
        content <- o Aeson..:? "content" Aeson..!= EventContent Nothing Nothing [] Nothing
        pure Event{type_, sender, eventId, content, raw = value}

data EventContent = EventContent
  { msgtype :: !(Maybe Text)
  , body :: !(Maybe Text)
  , mentions :: ![Text]
  , replyToEventId :: !(Maybe MatrixEventId)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON EventContent where
  parseJSON = Aeson.withObject "EventContent" \o -> do
    msgtype <- o Aeson..:? "msgtype"
    body <- o Aeson..:? "body"
    mentions <- o Aeson..:? "m.mentions" Aeson..!= MatrixMentions []
    replyToEventId <- o Aeson..:? "m.relates_to" Aeson..!= MatrixRelatesTo Nothing
    pure EventContent
      { msgtype
      , body
      , mentions = mentions.userIds
      , replyToEventId = replyToEventId.inReplyToEventId
      }

newtype MatrixMentions = MatrixMentions
  { userIds :: [Text]
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON MatrixMentions)

instance Aeson.FromJSON MatrixMentions where
  parseJSON = Aeson.withObject "MatrixMentions" \o ->
    MatrixMentions <$> o Aeson..:? "user_ids" Aeson..!= []

newtype MatrixRelatesTo = MatrixRelatesTo
  { inReplyToEventId :: Maybe MatrixEventId
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MatrixRelatesTo where
  parseJSON = Aeson.withObject "MatrixRelatesTo" \o -> do
    inReplyTo <- o Aeson..:? "m.in_reply_to" Aeson..!= MatrixInReplyTo Nothing
    pure (MatrixRelatesTo inReplyTo.replyEventId)

instance Aeson.ToJSON MatrixRelatesTo where
  toJSON MatrixRelatesTo{inReplyToEventId} =
    Aeson.object
      [ "m.in_reply_to" Aeson..= MatrixInReplyTo inReplyToEventId
      ]

newtype MatrixInReplyTo = MatrixInReplyTo
  { replyEventId :: Maybe MatrixEventId
  }
  deriving (Show, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via (PrefixedSnakeJSON "reply" MatrixInReplyTo)

data SendMessageRequest = SendMessageRequest
  { msgtype :: !Text
  , body :: !Text
  , formattedBody :: !(Maybe Text)
  , replyRelation :: !(Maybe MatrixReplyTo)
  , mentions :: !MatrixMentions
  , streamMetadata :: !MatrixStreamMetadata
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SendMessageRequest where
  toJSON SendMessageRequest{msgtype, body, formattedBody, replyRelation, mentions, streamMetadata} =
    Aeson.object $
      [ "msgtype" Aeson..= msgtype
      , "body" Aeson..= body
      , "m.mentions" Aeson..= mentions
      , matrixStreamMetadataPair streamMetadata
      ]
        <> matrixFormattedBodyFields formattedBody
        <> maybe [] (\(MatrixReplyTo eventId) -> ["m.relates_to" Aeson..= MatrixRelatesTo (Just eventId)]) replyRelation

newtype MatrixStreamMetadata = MatrixStreamMetadata
  { streamComplete :: Bool
  }
  deriving (Show, Generic)

instance Aeson.FromJSON MatrixStreamMetadata where
  parseJSON = Aeson.withObject "MatrixStreamMetadata" \o ->
    MatrixStreamMetadata <$> o Aeson..: "complete"

instance Aeson.ToJSON MatrixStreamMetadata where
  toJSON MatrixStreamMetadata{streamComplete} =
    Aeson.object ["complete" Aeson..= streamComplete]

matrixStreamMetadataPair :: MatrixStreamMetadata -> Aeson.Pair
matrixStreamMetadataPair metadata =
  "com.pfeiwu.ai.stream" Aeson..= metadata

newtype MatrixUploadResponse = MatrixUploadResponse
  { contentUri :: Text
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON MatrixUploadResponse)

data MatrixFileInfo = MatrixFileInfo
  { mimetype :: !Text
  , size :: !Integer
  }
  deriving (Show, Generic, Aeson.ToJSON)

data MatrixFileMessage = MatrixFileMessage
  { msgtype :: !Text
  , body :: !Text
  , filename :: !Text
  , url :: !Text
  , info :: !MatrixFileInfo
  }
  deriving (Show, Generic)
    deriving Aeson.ToJSON via (SnakeJSON MatrixFileMessage)

data MatrixFileMessageRequest = MatrixFileMessageRequest
  { message :: !MatrixFileMessage
  , replyRelation :: !(Maybe MatrixReplyTo)
  , streamMetadata :: !MatrixStreamMetadata
  }
  deriving (Show, Generic)

instance Aeson.ToJSON MatrixFileMessageRequest where
  toJSON MatrixFileMessageRequest{message, replyRelation, streamMetadata} =
    case Aeson.toJSON message of
      Aeson.Object fields ->
        Aeson.Object (fields <> AesonKeyMap.fromList relationFields)
      value ->
        value
    where
      relationFields =
        [("com.pfeiwu.ai.stream", Aeson.toJSON streamMetadata)]
          <> maybe [] (\(MatrixReplyTo eventId) -> [("m.relates_to", Aeson.toJSON (MatrixRelatesTo (Just eventId)))]) replyRelation

data MatrixEditMessageRequest = MatrixEditMessageRequest
  { body :: !Text
  , formattedBody :: !(Maybe Text)
  , mentions :: !MatrixMentions
  , replacesEventId :: !MatrixEventId
  , streamMetadata :: !MatrixStreamMetadata
  }
  deriving (Show, Generic)

completeMatrixEditMessageRequest :: MatrixEditMessageRequest -> MatrixEditMessageRequest
completeMatrixEditMessageRequest request =
  MatrixEditMessageRequest
    { body = request.body
    , formattedBody = request.formattedBody
    , mentions = request.mentions
    , replacesEventId = request.replacesEventId
    , streamMetadata = MatrixStreamMetadata True
    }

instance Aeson.ToJSON MatrixEditMessageRequest where
  toJSON MatrixEditMessageRequest{body, formattedBody, mentions, replacesEventId, streamMetadata} =
    Aeson.object $
      [ "msgtype" Aeson..= ("m.text" :: Text)
      , "body" Aeson..= ("* " <> body)
      , "m.new_content" Aeson..= Aeson.object
          ( [ "msgtype" Aeson..= ("m.text" :: Text)
            , "body" Aeson..= body
            , "m.mentions" Aeson..= mentions
            , matrixStreamMetadataPair streamMetadata
            ]
              <> matrixFormattedBodyFields formattedBody
          )
      , "m.mentions" Aeson..= mentions
      , matrixStreamMetadataPair streamMetadata
      , "m.relates_to" Aeson..= Aeson.object
          [ "rel_type" Aeson..= ("m.replace" :: Text)
          , "event_id" Aeson..= matrixEventIdText replacesEventId
          ]
      ]
        <> matrixFormattedBodyFields (("* " <>) <$> formattedBody)

matrixFormattedBodyFields :: Maybe Text -> [Aeson.Pair]
matrixFormattedBodyFields = \case
  Nothing ->
    []
  Just html ->
    [ "format" Aeson..= ("org.matrix.custom.html" :: Text)
    , "formatted_body" Aeson..= html
    ]

formatMatrixMarkdown :: Text -> Maybe Text
formatMatrixMarkdown input =
  formatMatrixMarkdownWithMentionNames Map.empty input

formatMatrixMarkdownWithMentionNames :: Map Text Text -> Text -> Maybe Text
formatMatrixMarkdownWithMentionNames mentionNames input =
  case runIdentity (commonmarkWith matrixMarkdownSyntax "matrix-message" (linkifyMatrixMentions mentionNames input)) :: Either ParseError (Html ()) of
    Left _ ->
      Nothing
    Right html ->
      nonEmptyText (Text.strip (LazyText.toStrict (renderHtml html)))

linkifyMatrixMentions :: Map Text Text -> Text -> Text
linkifyMatrixMentions mentionNames =
  Text.concat . map (linkifyToken mentionNames) . Text.groupBy sameWhitespace
  where
    sameWhitespace left right =
      Char.isSpace left == Char.isSpace right

linkifyToken :: Map Text Text -> Text -> Text
linkifyToken mentionNames token
  | Text.all Char.isSpace token =
      token
  | isMatrixUserId userId =
      "[" <> escapeMarkdownLinkLabel displayName <> "](" <> matrixToUserUrl userId <> ")" <> suffix
  | otherwise =
      token
  where
    userId = Text.dropWhileEnd (`elem` matrixUserIdTrailingPunctuation) token
    suffix = Text.drop (Text.length userId) token
    displayName = matrixMentionDisplayText mentionNames userId

matrixMentionDisplayBody :: Map Text Text -> Text -> Text
matrixMentionDisplayBody mentionNames =
  Text.concat . map replaceToken . Text.groupBy sameWhitespace
  where
    sameWhitespace left right =
      Char.isSpace left == Char.isSpace right

    replaceToken token
      | Text.all Char.isSpace token =
          token
      | isMatrixUserId userId =
          matrixMentionDisplayText mentionNames userId <> suffix
      | otherwise =
          token
      where
        userId = Text.dropWhileEnd (`elem` matrixUserIdTrailingPunctuation) token
        suffix = Text.drop (Text.length userId) token

matrixMentionDisplayText :: Map Text Text -> Text -> Text
matrixMentionDisplayText mentionNames userId =
  fromMaybe userId (Map.lookup userId mentionNames >>= matrixMentionDisplayName)

escapeMarkdownLinkLabel :: Text -> Text
escapeMarkdownLinkLabel =
  Text.concatMap \case
    '\\' -> "\\\\"
    '[' -> "\\["
    ']' -> "\\]"
    '\n' -> " "
    '\r' -> " "
    char -> Text.singleton char

matrixToUserUrl :: Text -> Text
matrixToUserUrl userId =
  "https://matrix.to/#/" <> userId

matrixMarkdownSyntax :: SyntaxSpec Identity (Html ()) (Html ())
matrixMarkdownSyntax =
  gfmExtensions
    <> mathSpec
    <> footnoteSpec
    <> defaultSyntaxSpec

data RedactEventRequest = RedactEventRequest
  { reason :: Maybe Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

data SetTypingRequest = SetTypingRequest
  { timeout :: Int
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SetTypingRequest where
  toJSON SetTypingRequest{timeout} =
    Aeson.object
      [ "typing" Aeson..= True
      , "timeout" Aeson..= timeout
      ]

newtype SendMessageResponse = SendMessageResponse
  { eventId :: MatrixEventId
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (SnakeJSON SendMessageResponse)

newtype RedactEventResponse = RedactEventResponse
  { redactionEventId :: Text
  }
  deriving (Show, Generic)
    deriving Aeson.FromJSON via (PrefixedSnakeJSON "redaction" RedactEventResponse)

matrixSyncTimeoutMilliseconds :: MatrixSyncMode -> Int
matrixSyncTimeoutMilliseconds = \case
  MatrixInitialSync ->
    0
  MatrixLongPollSync ->
    matrixLongPollSyncTimeoutMilliseconds

matrixLongPollSyncTimeoutMilliseconds :: Int
matrixLongPollSyncTimeoutMilliseconds = 30000

matrixSyncResponseTimeoutMicroseconds :: Int
matrixSyncResponseTimeoutMicroseconds = 40000000

matrixApiResponseTimeoutMicroseconds :: Int
matrixApiResponseTimeoutMicroseconds = 10000000

matrixMediaDownloadResponseTimeoutMicroseconds :: Int
matrixMediaDownloadResponseTimeoutMicroseconds = 60000000

matrixReloginAttempts :: Int
matrixReloginAttempts = 12

matrixReloginDelaySeconds :: Int
matrixReloginDelaySeconds = 180

matrixReloginDelayMicroseconds :: Int
matrixReloginDelayMicroseconds = matrixReloginDelaySeconds * 1000000

matrixRetryDelayMicroseconds :: Int
matrixRetryDelayMicroseconds = 5000000
