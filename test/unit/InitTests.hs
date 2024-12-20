-- | InitTests.hs
-- | This file contains the implementation of the init tests

{-# LANGUAGE OverloadedStrings #-}

module InitTests
  ( initTests,
  )
where

import CommandParser (ParsedCommand (..))
import Command (Command (..), CommandError (..))
import Commit (deserializeCommit, deserializeTree, getCurrentCommitOid)
import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
    removeDirectoryRecursive,
    setCurrentDirectory,
  )
import System.FilePath ((</>))
import Test.HUnit ( assertBool, assertFailure, Test(..))
import CommandHandler ( commandHandler )
import TestUtils(runCommand)
import Utils (createDirectoryIfMissing')

-- | HGit init command creates the expected structure.
testHGitInit :: Test
testHGitInit = TestCase $ do
  currentDir <- getCurrentDirectory
  let testDir = currentDir </> "test-dir"
  doesDirectoryExist testDir >>= \exists -> when exists (removeDirectoryRecursive testDir)
  createDirectoryIfMissing' testDir
  setCurrentDirectory testDir
  result <- runCommand Command.Init [] []

  case result of
    Left (CommandError err) -> assertFailure $ "Command failed with error: " ++ err
    Right _ -> do
      let hgitPath = testDir </> ".hgit"
      exists <- doesDirectoryExist hgitPath
      assertBool ".hgit directory should exist" exists

      let objectsPath = hgitPath </> "objects"
      objectsExists <- doesDirectoryExist objectsPath
      assertBool "objects directory should exist" objectsExists

      let refsHeadsPath = hgitPath </> "refs" </> "heads"
      refsHeadsExists <- doesDirectoryExist refsHeadsPath
      assertBool "refs/heads directory should exist" refsHeadsExists

      let headFilePath = hgitPath </> "HEAD"
      headFileExists <- doesFileExist headFilePath
      assertBool "HEAD file should exist" headFileExists

  setCurrentDirectory currentDir
  removeDirectoryRecursive testDir

-- | Collection of init tests
initTests :: Test
initTests =
  TestLabel "Init Tests" $
    TestList [
      TestLabel "HGit Init" testHGitInit
    ]
