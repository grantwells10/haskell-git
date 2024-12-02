{-# LANGUAGE OverloadedStrings #-}

module TestUtils
  ( testValidate,
    createFiles,
    runCommand,
    runAddCommand,
    runCommitCommand,
    verifyIndex,
    verifyBlobExists,
    withTestRepo,
  )
where

import CommandHandler (commandHandler)
import CommandParser
  ( Command (Command, flags, subcommand),
    CommandError (..),
    Flag (..),
    FlagType (..),
    ParsedCommand
      ( ParsedCommand,
        parsedArguments,
        parsedFlags,
        parsedSubcommand
      ),
    defaultValidate,
    parseCommand,
    parseFlagsAndArgs,
    parseInput,
  )
import Commit
  ( Commit (..),
    Tree (..),
    buildTree,
    createCommitContent,
    deserializeCommit,
    deserializeTree,
    getCurrentCommitOid,
    updateHEAD,
  )
import Control.Exception
  ( SomeException,
    catch,
    try,
  )
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Bifunctor qualified
import Data.ByteString.Char8 qualified as BS8
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Index (readIndexFile, writeIndexFile)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
    removeDirectoryRecursive,
    setCurrentDirectory,
  )
import System.FilePath
  ( makeRelative,
    normalise,
    splitDirectories,
    takeDirectory,
    takeFileName,
    (</>),
  )
import Test.HUnit
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Args (maxSuccess),
    Gen,
    Property,
    Testable,
    chooseInt,
    elements,
    forAll,
    frequency,
    listOf,
    listOf1,
    quickCheck,
    quickCheckWith,
    stdArgs,
    (==>),
  )
import Utils ( doesDirectoryExist, readFileAsByteString, sha1Hash )
import Commands (commandInit, commandAdd, commandCommit, commandBranch)

-- | Validation function that allows any flags and arguments
testValidate :: [(String, Maybe String)] -> [String] -> Either CommandError ()
testValidate _ _ = Right ()

-- | Helper function to create multiple files with specified content
createFiles :: [(FilePath, String)] -> IO ()
createFiles = mapM_ (\(f, content) -> createDirectoryIfMissing True (takeDirectory f) >> writeFile f content)

-- | Runs a specified command with given flags and arguments
runCommand :: Command -> [(String, Maybe String)] -> [String] -> IO (Either CommandError String)
runCommand cmd flags args = do
  let parsedCmd =
        ParsedCommand
          { parsedSubcommand = cmd,
            parsedFlags = flags,
            parsedArguments = args
          }
  commandHandler parsedCmd

-- | Runs the 'git add' command with given flags and arguments
runAddCommand :: [(String, Maybe String)] -> [String] -> IO ()
runAddCommand flags args = do
  resultAdd <- runCommand commandAdd flags args
  case resultAdd of
    Left (CommandError err) -> assertFailure $ "Add command failed: " ++ err
    Right _ -> return ()

-- | Runs the 'git commit' command with given flags and arguments
runCommitCommand :: [(String, Maybe String)] -> [String] -> IO ()
runCommitCommand flags args = do
  resultCommit <- runCommand commandCommit flags args
  case resultCommit of
    Left (CommandError err) -> assertFailure $ "Commit command failed: " ++ err
    Right _ -> return ()

-- | Verifies that specified files are present or absent in the index
verifyIndex :: [(FilePath, Bool)] -> IO ()
verifyIndex checks = do
  indexMap <- readIndexFile
  forM_ checks $ \(file, shouldBePresent) -> do
    let normalizedFile = normalise file
    if shouldBePresent
      then assertBool (file ++ " should be in index") (Map.member normalizedFile indexMap)
      else assertBool (file ++ " should not be in index") (not $ Map.member normalizedFile indexMap)

-- | Verifies that blob files exist for specified files and match expected content
verifyBlobExists :: FilePath -> FilePath -> String -> IO ()
verifyBlobExists testDir file content = do
  indexMap <- readIndexFile
  let oid = indexMap Map.! file
  let (dirName, fileName) = splitAt 2 oid
  let blobPath = testDir </> ".hgit" </> "objects" </> dirName </> fileName
  blobExists <- doesFileExist blobPath
  assertBool ("Blob file should exist for " ++ file) blobExists
  let expectedOid = sha1Hash $ BS8.pack content
  assertEqual
    ("OID should match SHA-1 of " ++ file)
    expectedOid
    oid

-- | Helper function to initialize a test repository
withTestRepo :: (FilePath -> IO ()) -> IO ()
withTestRepo action = do
  originalDir <- getCurrentDirectory
  let testDir = originalDir </> "test-repo"

  -- Remove testDir if it exists
  testDirExists <- doesDirectoryExist testDir
  when testDirExists $ removeDirectoryRecursive testDir

  -- Create and navigate to testDir
  createDirectoryIfMissing True testDir
  setCurrentDirectory testDir

  -- Initialize repository
  resultInit <- runCommand commandInit [] []
  case resultInit of
    Left (CommandError err) -> do
      setCurrentDirectory originalDir
      removeDirectoryRecursive testDir
      assertFailure $ "Initialization failed: " ++ err
    Right _ -> do
      action testDir
      setCurrentDirectory originalDir
      removeDirectoryRecursive testDir