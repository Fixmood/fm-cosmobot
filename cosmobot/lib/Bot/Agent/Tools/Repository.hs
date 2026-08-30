{-|
Module      : Bot.Agent.Tools.Repository
Description : Owner-only FM repository and pull-request workflow
Stability   : experimental
-}
module Bot.Agent.Tools.Repository
  ( fmRepositoryPRTool
  )
where

import Bot.Agent.Tool
import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Effect.HTTP as HTTP
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import Effectful.Process (Process, proc, readCreateProcessWithExitCode)
import Network.HTTP.Req
import System.Exit (ExitCode (..))

data RepositoryArgs = RepositoryArgs
  { action :: !Text
  , branch :: !(Maybe Text)
  , title :: !(Maybe Text)
  , body :: !(Maybe Text)
  , commitMessage :: !(Maybe Text)
  }

fmRepositoryPRTool
  :: (HTTP.HTTP :> es, Process :> es, IOE :> es)
  => Tool (Eff es)
fmRepositoryPRTool =
  allowWhen superuserOnly
  . noisy
  . withDescription "Manage FM's private source repository and pull requests. Actions: status, start, submit. Use status to inspect the current repository; start to create an fm/pr-* branch from fm/main; submit only after the owner explicitly asks to publish the current code changes. The tool checks forbidden runtime/secret files, commits, pushes, and creates a GitHub pull request. Never expose credentials."
  $ tool "fm_repository_pr"
      (parsedArguments repositorySchema parseRepositoryArgs)
      runRepository
  where
    runRepository args = do
      repo <- repositoryPath
      case Text.toCaseFold (Text.strip args.action) of
        "status" -> repositoryStatus repo
        "查看" -> repositoryStatus repo
        "start" -> startBranch repo args.branch
        "开始" -> startBranch repo args.branch
        "submit" -> submitPullRequest repo args
        "提交" -> submitPullRequest repo args
        value -> pure (argumentFailure ("不支持的仓库操作：" <> value))

repositoryPath :: IOE :> es => Eff es FilePath
repositoryPath =
  liftIO (fromMaybe "/work/fm-repository" <$> lookupEnv "FM_REPOSITORY_PATH")

repositoryStatus :: Process :> es => FilePath -> Eff es ToolResult
repositoryStatus repo = do
  branchResult <- git repo ["branch", "--show-current"]
  statusResult <- git repo ["status", "--short"]
  remoteResult <- git repo ["remote", "get-url", "origin"]
  pure $ case (branchResult, statusResult, remoteResult) of
    (Right current, Right changes, Right remote)
      | expectedRepository `Text.isInfixOf` Text.strip remote ->
          toolText $ Text.unlines
            [ "FM repository: " <> Text.strip remote
            , "Branch: " <> Text.strip current
            , "Changes:"
            , if Text.null (Text.strip changes) then "(clean)" else Text.take 6000 changes
            ]
      | otherwise -> argumentFailure "仓库 origin 不是 Fixmood/fm-cosmobot，已拒绝操作。"
    _ -> argumentFailure "FM 仓库尚未初始化或当前不可读取。"

startBranch :: Process :> es => FilePath -> Maybe Text -> Eff es ToolResult
startBranch repo requestedBranch = do
  remote <- git repo ["remote", "get-url", "origin"]
  status <- git repo ["status", "--porcelain"]
  case (remote, status, normalizeBranch requestedBranch) of
    (Right url, Right changes, Right branchName)
      | expectedRepository `Text.isInfixOf` Text.strip url
      , Text.null (Text.strip changes) -> do
          result <- runGitSteps repo
            [ ["fetch", "origin", baseBranch]
            , ["switch", baseBranch]
            , ["pull", "--ff-only", "origin", baseBranch]
            , ["switch", "-c", Text.unpack branchName]
            ]
          pure $ either argumentFailure
            (const (toolText ("已创建 PR 工作分支：" <> branchName <> "。可以开始修改代码。"))) result
      | expectedRepository `Text.isInfixOf` Text.strip url ->
          pure (argumentFailure "当前仓库已有未提交改动，请先查看或提交，不能切换基线分支。")
      | otherwise -> pure (argumentFailure "仓库 origin 不是 Fixmood/fm-cosmobot，已拒绝操作。")
    (_, _, Left err) -> pure (argumentFailure err)
    _ -> pure (argumentFailure "FM 仓库尚未初始化或当前不可读取。")

submitPullRequest
  :: (HTTP.HTTP :> es, Process :> es, IOE :> es)
  => FilePath
  -> RepositoryArgs
  -> Eff es ToolResult
submitPullRequest repo args = do
  current <- git repo ["branch", "--show-current"]
  status <- git repo ["status", "--porcelain"]
  diffCheck <- git repo ["diff", "--check"]
  case (current, status, diffCheck, requiredField "PR 标题" args.title) of
    (Right rawBranch, Right changes, Right _, Right prTitle)
      | let branchName = Text.strip rawBranch
      , not ("fm/pr-" `Text.isPrefixOf` branchName) ->
          pure (argumentFailure "只能从 fm/pr-* 工作分支提交 PR。")
      | Text.null (Text.strip changes) ->
          pure (argumentFailure "当前工作分支没有可提交的改动。")
      | Just forbidden <- findForbiddenPath changes ->
          pure (argumentFailure ("发现禁止提交的运行或敏感文件：" <> forbidden))
      | otherwise -> do
          addResult <- git repo ["add", "--all"]
          stagedDiff <- git repo ["diff", "--cached", "--no-ext-diff", "--unified=1"]
          case (addResult, stagedDiff) of
            (Right _, Right diff)
              | Just marker <- sensitiveMarker diff -> do
                  void (git repo ["reset"])
                  pure (argumentFailure ("提交内容疑似包含敏感信息（" <> marker <> "），已取消暂存。"))
              | otherwise -> do
                  let message = Text.strip (fromMaybe prTitle args.commitMessage)
                      branchName = Text.strip rawBranch
                  committed <- git repo
                    [ "-c", "user.name=FM"
                    , "-c", "user.email=fm@users.noreply.github.com"
                    , "commit", "-m", Text.unpack message
                    ]
                  case committed of
                    Left err -> pure (argumentFailure err)
                    Right _ -> do
                      pushed <- git repo ["push", "-u", "origin", Text.unpack branchName]
                      case pushed of
                        Left err -> pure (argumentFailure ("提交已创建，但推送失败：" <> err))
                        Right _ -> createPullRequest branchName prTitle (fromMaybe "" args.body)
            (Left err, _) -> pure (argumentFailure err)
            (_, Left err) -> pure (argumentFailure err)
    (Left err, _, _, _) -> pure (argumentFailure err)
    (_, Left err, _, _) -> pure (argumentFailure err)
    (_, _, Left err, _) -> pure (argumentFailure ("代码格式检查未通过：" <> err))
    (_, _, _, Left err) -> pure (argumentFailure err)

createPullRequest
  :: (HTTP.HTTP :> es, IOE :> es)
  => Text
  -> Text
  -> Text
  -> Eff es ToolResult
createPullRequest branchName prTitle prBody = do
  token <- liftIO (lookupEnv "FM_GITHUB_TOKEN")
  case Text.strip . Text.pack <$> token of
    Nothing -> pure . toolText $ pushedWithoutPR branchName
    Just "" -> pure . toolText $ pushedWithoutPR branchName
    Just githubToken -> do
      result <- trySync $ responseBody <$> HTTP.runReq
        (req POST
          (https "api.github.com" /: "repos" /: "Fixmood" /: "fm-cosmobot" /: "pulls")
          (ReqBodyJson (Aeson.object
            [ "title" Aeson..= prTitle
            , "head" Aeson..= branchName
            , "base" Aeson..= baseBranch
            , "body" Aeson..= prBody
            ]))
          jsonResponse
          ( header "Authorization" (ByteString.pack ("Bearer " <> Text.unpack githubToken))
         <> header "Accept" "application/vnd.github+json"
         <> header "X-GitHub-Api-Version" "2022-11-28"
         <> header "User-Agent" "FM-Cosmobot"
          ))
      pure $ case result of
        Left (_ :: SomeException) -> toolText (pushedWithoutPR branchName)
        Right value ->
          case AesonTypes.parseMaybe (Aeson.withObject "GitHub PR" (Aeson..: "html_url")) value of
            Just url -> toolText ("PR 已创建：" <> url)
            Nothing -> toolText (pushedWithoutPR branchName)

pushedWithoutPR :: Text -> Text
pushedWithoutPR branchName =
  Text.unlines
    [ "分支已安全推送，GitHub Actions 正在自动创建 PR。"
    , "如自动流程暂时失败，可在这里继续：https://github.com/Fixmood/fm-cosmobot/compare/" <> Text.pack baseBranch <> "..." <> branchName
    ]

git :: Process :> es => FilePath -> [String] -> Eff es (Either Text Text)
git repo args = do
  (code, output, err) <- readCreateProcessWithExitCode (proc "git" (["-C", repo] <> args)) ""
  pure $ case code of
    ExitSuccess -> Right (Text.pack output)
    ExitFailure _ -> Left (Text.take 2000 (Text.strip (Text.pack err)))

runGitSteps :: Process :> es => FilePath -> [[String]] -> Eff es (Either Text ())
runGitSteps repo = go
  where
    go [] = pure (Right ())
    go (args : rest) = git repo args >>= \case
      Left err -> pure (Left err)
      Right _ -> go rest

normalizeBranch :: Maybe Text -> Either Text Text
normalizeBranch requested = do
  suffix <- requiredField "分支名称" requested
  let clean = Text.toCaseFold (Text.strip suffix)
      normalized = fromMaybe clean (Text.stripPrefix "fm/pr-" clean)
  if Text.null normalized || Text.length normalized > 60 || Text.any (not . validBranchChar) normalized
    then Left "分支名称只能包含小写字母、数字和连字符，长度不超过 60。"
    else Right ("fm/pr-" <> normalized)
  where
    validBranchChar char = Char.isAsciiLower char || Char.isDigit char || char == '-'

requiredField :: Text -> Maybe Text -> Either Text Text
requiredField label value =
  case Text.strip <$> value of
    Just text | not (Text.null text) -> Right text
    _ -> Left ("缺少" <> label <> "。")

findForbiddenPath :: Text -> Maybe Text
findForbiddenPath status =
  find isForbidden (Text.lines status)
  where
    isForbidden line =
      let path = Text.toCaseFold (Text.strip (Text.drop 2 line))
      in any (`Text.isInfixOf` path)
          [ "config.toml", ".env", "runtime/", "work/", "tool-output/"
          , "source-backups/", ".sqlite", ".pem", ".key", ".log"
          ]

sensitiveMarker :: Text -> Maybe Text
sensitiveMarker diff
  | "begin openssh private key" `Text.isInfixOf` folded = Just "私钥"
  | "begin rsa private key" `Text.isInfixOf` folded = Just "私钥"
  | any hasSecretAssignment (Text.lines folded) = Just "非空凭据字段"
  | otherwise = Nothing
  where
    folded = Text.toCaseFold diff
    hasSecretAssignment line =
      "+" `Text.isPrefixOf` line
        && any (`Text.isInfixOf` line) ["api_key = \"", "password = \"", "token = \""]
        && not ("= \"\"" `Text.isInfixOf` line)
        && not ("something" `Text.isInfixOf` line)
        && not ("replace-" `Text.isInfixOf` line)

argumentFailure :: Text -> ToolResult
argumentFailure message =
  toolFailure (permanentArgumentFailure message message)

repositorySchema :: Aeson.Value
repositorySchema = objectSchema
  [ fieldText "action" "status, start, or submit"
  , fieldText "branch" "For start: branch suffix using lowercase letters, digits, and hyphens."
  , fieldText "title" "For submit: pull-request title."
  , fieldText "body" "For submit: concise pull-request description and verification."
  , fieldText "commit_message" "Optional commit message; defaults to the PR title."
  ]
  ["action"]

parseRepositoryArgs :: Aeson.Value -> AesonTypes.Parser RepositoryArgs
parseRepositoryArgs = Aeson.withObject "fm_repository_pr arguments" \object ->
  RepositoryArgs
    <$> object Aeson..: Key.fromText "action"
    <*> object Aeson..:? Key.fromText "branch"
    <*> object Aeson..:? Key.fromText "title"
    <*> object Aeson..:? Key.fromText "body"
    <*> object Aeson..:? Key.fromText "commit_message"

expectedRepository :: Text
expectedRepository = "Fixmood/fm-cosmobot"

baseBranch :: String
baseBranch = "fm/main"
