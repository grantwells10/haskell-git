module CommandHandler (commandHandler) where

import CommandParser (Command (..), CommandError (..), Flag (..), FlagType (..), ParsedCommand (..), defaultValidate)
import Commit
    ( buildTree, createCommitContent, getCurrentCommitOid, updateHEAD )
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Text qualified (pack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Index (readIndexFile, updateIndex, writeIndexFile)
import Utils
    ( doesDirectoryExist,
      createObject,
      writeFileFromByteString,
      createDirectoryIfMissing',
      createFileIfMissing,
      getHgitPath,
      getHeadsPath,
      getObjectsPath,
      getHeadPath,
      getHEADFilePath,
      stringToByteString )
import Branch ( listBranches, createBranch, deleteBranch )
import Commands
  ( commandInit,
    commandAdd,
    commandCommit,
    commandBranch
    -- Import more commands here as added
  )

-- | Main command handler that dispatches commands
commandHandler :: ParsedCommand -> IO (Either CommandError String)
commandHandler parsedCmd = do
  let cmdStr = subcommand . parsedSubcommand $ parsedCmd
      flags = parsedFlags parsedCmd
      args = parsedArguments parsedCmd

  -- Check if the repository exists for all commands except 'init'
  when (cmdStr /= "init") $ do
    repoExists <- doesDirectoryExist =<< getHgitPath
    unless repoExists $
      throwIO $
        userError ".hgit doesn't exist, call 'hgit init' first"

  -- Dispatch the command with exception handling
  result <- try $ case cmdStr of
    "init" -> handleInit
    "add" -> handleAdd flags args
    "commit" -> handleCommit flags args
    "branch" -> handleBranch flags args

    -- Add other command handlers here
    _ -> throwIO $ userError $ "Unknown subcommand: " ++ cmdStr

  -- Convert exceptions to CommandError
  case result of
    Left (ex :: SomeException) -> return $ Left (CommandError $ show ex)
    Right output -> return $ Right output

handleInit :: IO String
handleInit = do
  hgitPath <- getHgitPath
  repoExists <- doesDirectoryExist hgitPath
  if repoExists
    then return "hgit repository already exists. No action taken."
    else do
      initializeRepository
      return ""
  where
    initializeRepository :: IO ()
    initializeRepository = do
      createDirectoryIfMissing' =<< getObjectsPath
      createDirectoryIfMissing' =<< getHeadsPath
      headFilePath <- getHeadPath "main"
      createFileIfMissing headFilePath
      let headContent = "refs/heads/main"
      headPath <- getHEADFilePath
      writeFileFromByteString headPath $ stringToByteString headContent

handleAdd :: [(String, Maybe String)] -> [String] -> IO String
handleAdd flags args = do
  indexMap <- readIndexFile
  updatedIndexMapResult <- updateIndex indexMap args flags
  case updatedIndexMapResult of
    Left err -> throwIO $ userError $ show err
    Right updatedIndexMap -> do
      writeIndexFile updatedIndexMap
      return ""

handleCommit :: [(String, Maybe String)] -> [String] -> IO String
handleCommit flags args = do
  let Just (Just commitMsg) = lookup "message" flags
  indexMap <- readIndexFile
  when (Map.null indexMap) $
    error "Nothing to commit. The index is empty."
  treeOid <- buildTree indexMap
  currentTime <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%s" currentTime
  parentOid <- getCurrentCommitOid
  let commitContent = createCommitContent treeOid parentOid timestamp commitMsg
  commitOid <- createObject commitContent
  updateHEAD commitOid
  return ""

handleBranch :: [(String, Maybe String)] -> [String] -> IO String
handleBranch flags args = do
  case flags of
    [("delete", Just branchName)] -> do
      deleteBranch branchName
      return $ "Deleted branch '" ++ branchName ++ "'."
    [] ->
      case args of
        [] -> do
          listBranches
          return ""
        [branchName] -> do
          createBranch branchName
          return $ "Branch '" ++ branchName ++ "' created."
        _ -> throwIO $ userError "Invalid usage of 'hgit branch'. Use 'hgit branch', 'hgit branch <branchname>', or 'hgit branch -d <branchname>'."
    _ -> throwIO $ userError "Invalid usage of 'hgit branch'. Use 'hgit branch', 'hgit branch <branchname>', or 'hgit branch -d <branchname>'."