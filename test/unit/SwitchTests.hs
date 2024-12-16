{-# LANGUAGE OverloadedStrings #-}

module SwitchTests
  ( switchTests
  ) where

import Test.HUnit
import TestUtils
  ( withTestRepo,
    createFiles,
    runCommand,
    verifyIndex
  )
import System.Directory
  ( doesFileExist,
    listDirectory
  )
import System.FilePath ((</>))
import Control.Monad (when)
import Index (readIndexFile, getAllFiles)
import Command (Command (..), CommandError(..))
import Data.Map.Strict qualified as Map
import Data.List (sort, isInfixOf)
import Utils (getHgitPath, stringToByteString, writeFileFromByteString, readFileAsByteString)

testSwitchNonExistentBranch :: Test
testSwitchNonExistentBranch = TestCase $ withTestRepo $ \testDir -> do
  let files = [("file1.txt", "Hello")]
  createFiles files
  runCommand Command.Add [] ["file1.txt"]
  runCommand Command.Commit [("message", Just "Initial commit")] []

  originalIndex <- readIndexFile
  originalFiles <- getAllFiles

  result <- runCommand Command.Switch [] ["no-such-branch"]
  case result of
    Left (CommandError msg) -> do
      assertBool "Error message should mention branch not existing" ("does not exist" `isInfixOf` msg)
    Right _ -> assertFailure "Expected failure when switching to non-existent branch."

  newIndex <- readIndexFile
  assertEqual "Index should not change after failed switch" originalIndex newIndex

  newFiles <- getAllFiles
  assertEqual "Working directory should not change after failed switch" (sort originalFiles) (sort newFiles)

testSwitchSameBranch :: Test
testSwitchSameBranch = TestCase $ withTestRepo $ \testDir -> do

  let files = [("file1.txt", "Hello")]
  createFiles files
  runCommand Command.Add [] ["file1.txt"]
  runCommand Command.Commit [("message", Just "Initial commit")] []

  originalIndex <- readIndexFile
  originalFiles <- getAllFiles

  result <- runCommand Command.Switch [] ["main"]
  case result of
    Left err -> assertFailure $ "Switching to same branch should not fail: " ++ show err
    Right _ -> return ()

  newIndex <- readIndexFile
  assertEqual "Index should not change after switching to the same branch" originalIndex newIndex

  newFiles <- getAllFiles
  assertEqual "Working directory should not change after switching to the same branch" (sort originalFiles) (sort newFiles)

testSwitchDifferentBranch :: Test
testSwitchDifferentBranch = TestCase $ withTestRepo $ \testDir -> do

  let filesMain =
        [ ("file1.txt", "Hello"),
          ("file2.txt", "World")
        ]
  createFiles filesMain
  runCommand Command.Add [] ["file1.txt", "file2.txt"]
  runCommand Command.Commit [("message", Just "Initial commit on main")] []

  mainIndex <- readIndexFile
  mainFiles <- getAllFiles

  resBranch <- runCommand Command.Branch [] ["feature"]
  case resBranch of
    Left (CommandError err) -> assertFailure $ "Branch creation failed: " ++ err
    Right _ -> return ()

  resSwitchFeature <- runCommand Command.Switch [] ["feature"]
  case resSwitchFeature of
    Left (CommandError err) -> assertFailure $ "Switch to feature should succeed: " ++ err
    Right _ -> return ()

  let filesFeature = [("file2.txt", "World-Updated"), ("file3.txt", "New file on feature")]
  createFiles filesFeature
  runCommand Command.Add [] ["file2.txt", "file3.txt"]
  runCommand Command.Commit [("message", Just "Commit on feature branch")] []

  featureIndex <- readIndexFile
  featureFiles <- getAllFiles

  assertBool "Index should differ after switching to feature and committing"
    (featureIndex /= mainIndex)
  assertBool "Files should differ after switching to feature and committing"
    (sort featureFiles /= sort mainFiles)

  resSwitchMain <- runCommand Command.Switch [] ["main"]
  case resSwitchMain of
    Left (CommandError err) -> assertFailure $ "Switching back to main should succeed: " ++ err
    Right _ -> return ()

  newIndex <- readIndexFile
  newFiles <- getAllFiles

  assertEqual "Index should match original main state after switching back" mainIndex newIndex
  assertEqual "Working directory should match original main state after switching back"
    (sort mainFiles) (sort newFiles)

testSwitchWithUncommitedChanges :: Test
testSwitchWithUncommitedChanges = TestCase $ withTestRepo $ \testDir -> do
  -- Main branch
  let filesMain = [("file1.txt", "Hello"), ("file2.txt", "World")]
  createFiles filesMain
  runCommand Command.Add [] ["file1.txt", "file2.txt"]
  runCommand Command.Commit [("message", Just "Init commit on main")] []

  mainIndex <- readIndexFile
  mainFiles <- getAllFiles

  -- New branch 
  _ <- runCommand Command.Branch [] ["newBranch"]
  switchTonewBranch <- runCommand Command.Switch [] ["newBranch"]
  case switchTonewBranch of
    Left (CommandError err) -> assertFailure $ "Switching to newBranch should succeed: " ++ err
    Right _ -> return ()

  newBranchIndexBeforeAdd <- readIndexFile
  newBranchFilesBeforeAdd <- getAllFiles

  -- Add on newBranch but don't commit
  let filesNewBranch = [("file3.txt", "Lebron James")]
  createFiles filesNewBranch
  runCommand Command.Add [] ["file3.txt"]

  newBranchIndexAfterAdd <- readIndexFile
  assertBool "Index should change after adding file" (newBranchIndexBeforeAdd /= newBranchIndexAfterAdd)

  -- Now try to switch back to main (should fail)
  switchToMain <- runCommand Command.Switch [] ["main"]
  case switchToMain of
    Left (CommandError msg) -> 
      assertBool "Switching to main should fail" 
      ("uncommitted changes" `isInfixOf` msg)
    Right _ -> assertFailure "Switching to main should fail"

  currIndex <- readIndexFile
  currFiles <- getAllFiles

  assertBool "Index should remain unchanged after failed switch" (newBranchIndexAfterAdd == currIndex)
  assertBool "Working directory should remain unchanged after failed switch"
    (sort (newBranchFilesBeforeAdd ++ ["file3.txt"]) == sort currFiles)

switchTests :: Test
switchTests = 
  TestLabel "Switch Command Tests" $
    TestList
      [ TestLabel "Switch Non-Existent Branch" testSwitchNonExistentBranch,
        TestLabel "Switch Same Branch" testSwitchSameBranch,
        TestLabel "Switch Different Branch and Back" testSwitchDifferentBranch,
        TestLabel "Switch with Uncommitted Changes" testSwitchWithUncommitedChanges
      ]
