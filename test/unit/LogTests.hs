-- | LogTests.hs
-- | This file contains the implementation of the log tests

{-# LANGUAGE OverloadedStrings #-}

module LogTests
  ( logTests
  )
where

import CommandHandler (commandHandler)
import CommandParser (ParsedCommand(..) )
import Command ( Command (..), CommandError (..) )
import Control.Monad (forM_, when)
import Data.List (isPrefixOf, find)
import Data.Map.Strict qualified as Map
import Index (readIndexFile)
import System.Directory (removeFile, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.HUnit
  ( assertBool,
    assertEqual,
    assertFailure,
    Test(..),
    Test(TestCase),
    Test(TestLabel),
    Test(TestList) )
import TestUtils
  ( createFiles,
    runCommand,
    withTestRepo )
import Utils ( readFileAsByteString, getHeadCommitOid )
import qualified Data.Bifunctor

-- | Splits a string into a list of substrings based on a given delimiter.
-- Example: splitOn "commit " "commit abc commit def" = ["", "abc ", "def"]
splitOn :: String -> String -> [String]
splitOn delim str = splitHelper delim str []
  where
    splitHelper :: String -> String -> [String] -> [String]
    splitHelper _ "" acc = reverse acc
    splitHelper d s acc =
      case findDelimiter d s of
        Nothing -> reverse (s : acc)
        Just (before, after) -> splitHelper d after (before : acc)

    -- | Finds the first occurrence of the delimiter in the string.
    -- Returns the substring before the delimiter and the substring after.
    findDelimiter :: String -> String -> Maybe (String, String)
    findDelimiter d s
      | d `isPrefixOf` s = Just ("", drop (length d) s)
      | otherwise =
          case s of
            [] -> Nothing
            (_:xs) ->
              fmap (Data.Bifunctor.first (head s :)) (findDelimiter d xs)

-- | Helper function to count the number of commits in the log output
countCommitsInLog :: String -> Int
countCommitsInLog logOutput =
  length $ filter (isPrefixOf "commit ") (lines logOutput)

-- | Helper function to remove leading and trailing whitespace
trimSpaces :: String -> String
trimSpaces = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Helper function to extract the commit message from a commit section
extractCommitMessage :: String -> String
extractCommitMessage section =
  let linesOfSection = lines section
      messageLines = dropWhile (not . isPrefixOf "Date:") linesOfSection
      messageContent = drop 1 messageLines
      msg = dropWhile (not . null) messageContent
      cleanMessages = map (dropWhile (== ' ')) msg
  in trimSpaces $ unwords cleanMessages

-- | Test that 'hgit log' outputs "No commits found." when there are no commits
testLogNoCommits :: Test
testLogNoCommits = TestCase $ withTestRepo $ \_testDir -> do
  
  logResult <- runCommand Command.Log [] []
  -- Check the output
  case logResult of
    Left err -> assertFailure $ "Log command failed: " ++ show err
    Right output -> do
      assertEqual "Log should indicate no commits found." "No commits found." output

-- | Test that 'hgit log' correctly displays one commit
testLogSingleCommit :: Test
testLogSingleCommit = TestCase $ withTestRepo $ \_testDir -> do
  -- Create and commit a single file
  let initialFiles =
        [ ("file1.txt", "Hello World")
        ]
  createFiles initialFiles
  runCommand Command.Add [] ["file1.txt"]
  runCommand Command.Commit [("message", Just "Initial commit")] []

  logResult <- runCommand Command.Log [] []

  -- Check the output
  case logResult of
    Left err -> assertFailure $ "Log command failed: " ++ show err
    Right output -> do
      let commitCount = countCommitsInLog output
      assertEqual "Log should display one commit." 1 commitCount

-- | Test that 'hgit log' correctly displays multiple commits
testLogMultipleCommits :: Test
testLogMultipleCommits = TestCase $ withTestRepo $ \_testDir -> do
  -- Create and commit first file
  let files1 =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files1
  runCommand Command.Add [] ["file1.txt", "file2.txt"]
  runCommand Command.Commit [("message", Just "Initial commit")] []

  -- Create and commit second file
  let files2 =
        [ ("file3.txt", "Second commit file")
        ]
  createFiles files2
  runCommand Command.Add [] ["file3.txt"]
  runCommand Command.Commit [("message", Just "Second commit")] []

  -- Create and commit third file
  let files3 =
        [ ("file4.txt", "Third commit file")
        ]
  createFiles files3
  runCommand Command.Add [] ["file4.txt"]
  runCommand Command.Commit [("message", Just "Third commit")] []

  logResult <- runCommand Command.Log [] []

  -- Check the output
  case logResult of
    Left err -> assertFailure $ "Log command failed: " ++ show err
    Right output -> do
      let commitCount = countCommitsInLog output
      assertEqual "Log should display three commits." 3 commitCount

-- | Test that the commits in the log match the expected messages
testLogCommitMessages :: Test
testLogCommitMessages = TestCase $ withTestRepo $ \_testDir -> do
  -- Create and commit first file
  let files1 =
        [ ("file1.txt", "Hello World"),
          ("file2.txt", "Initial commit file")
        ]
  createFiles files1
  runCommand Command.Add [] ["file1.txt", "file2.txt"]
  runCommand Command.Commit [("message", Just "Initial commit")] []

  -- Create and commit second file
  let files2 =
        [ ("file3.txt", "Second commit file")
        ]
  createFiles files2
  runCommand Command.Add [] ["file3.txt"]
  runCommand Command.Commit [("message", Just "Second commit")] []

  logResult <- runCommand Command.Log [] []

  -- Check the output
  case logResult of
    Left err -> assertFailure $ "Log command failed: " ++ show err
    Right output -> do
      -- Split the log output into individual commits based on "commit " prefix
      let commitSections = filter (not . null) $ splitOn "commit " output
          expectedMessages = ["Initial commit", "Second commit"]
          actualMessages = map extractCommitMessage commitSections
      assertEqual "Log should display correct commit messages." expectedMessages actualMessages

-- | Collection of all log tests
logTests :: Test
logTests =
  TestLabel "Log Command Tests" $
    TestList
      [ TestLabel "Log Without Commits" testLogNoCommits,
        TestLabel "Log After Single Commit" testLogSingleCommit,
        TestLabel "Log After Multiple Commits" testLogMultipleCommits,
        TestLabel "Log Commit Messages Order" testLogCommitMessages
      ]